;;; system-browser.el --- System browser      -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; A Smalltalk-like browser for programming languages.

;;; Code:

(require 'window-layout)
(require 'let-alist)
(require 'eieio)

;;------ Model ------------------------------------------

(defclass ssb:system-browser-system ()
  ((selected-module :accessor ssb:selected-module
                    :initform nil
                    :documentation "The current selected module")
   (selected-category :accessor ssb:selected-category
                      :initform nil
                      :documentation "The current selected category")
   (selected-definition :accessor ssb:selected-definition
                        :initform nil
                        :documentation "The current seleccted definition")))

(cl-defgeneric ssb:list-modules (system-browser-system)
  (:documentation "List modules of SYSTEM-BROWSER-SYSTEM."))
(cl-defgeneric ssb:list-categories (system-browser-system module)
  (:documentation "Lisp categories of MODULE for SYSTEM-BROWSER-SYSTEM."))
(cl-defgeneric ssb:list-definitions (system-browser-system module category))
(cl-defgeneric ssb:modules-buffer-mode-line-format (system-browser-system))
(cl-defgeneric ssb:categories-buffer-mode-line-format (system-browser-system))
(cl-defgeneric ssb:definitions-buffer-mode-line-format (system-browser-system))
(cl-defgeneric ssb:definition-buffer-mode-line-format (system-browser-system))
(cl-defgeneric ssb:system-initialize-definition-buffer (system-browser-system))
(cl-defgeneric ssb:get-module-properties (system-browser-system module)
  (:documentation "Return properties of MODULE.
Return value is an alist with keys 'source, 'file, 'position, 'documentation"))
(cl-defgeneric ssb:get-definition-properties (system-browser-system definition category module)
  (:documentation "Return properties of DEFINITION.
Return value is an alist with keys 'source, 'file, 'position, 'documentation"))

(cl-defmethod ssb:read-module-name (system prompt)
  (:documentation "Read module name from minibuffer."))

;; Default mode-line
(cl-defmethod ssb:modules-buffer-mode-line-format (system-browser-system)
  (ignore system-browser-system)
  "Modules")

(cl-defmethod ssb:categories-buffer-mode-line-format (system-browser-system)
  (ignore system-browser-system)
  "Categories")

(cl-defmethod ssb:definitions-buffer-mode-line-format (system-browser-system)
  (ignore system-browser-system)
  "Definitions")

(cl-defmethod ssb:definition-buffer-mode-line-format (system-browser-system)
  (ignore system-browser-system)
  nil)

(defvar ssb:modules-buffer)
(defvar ssb:catgories-buffer)
(defvar ssb:definitions-buffer)
(defvar ssb:definitions-buffer)
(defvar ssb:documentation-buffer)

(defvar ssb:current-browser-system nil
  "The current system browser system.")

(defvar system-browser-start-hook nil
  "Hook that runs when system browser is opened.")

;;--------- Settings ---------------------------------

(defgroup system-browser nil
  "System browser configuration"
  :group 'applications)

(defcustom ssb:show-documentation-buffer nil
  "Show documentation buffer in system browser."
  :type 'boolean
  :group 'system-browser
  :tag "Show documentation buffer")

(defcustom ssb:downcase-definition-names t
  "Show system-browser definition names in lowercase."
  :type 'boolean
  :group 'system-browser
  :tag "Downcase definition names")

(defcustom ssb:list-internal-definitions t
  "List modules internal defintions, apart from the exported."
  :type 'boolean
  :group 'system-browser
  :tag "List internal definitions")

(defcustom ssb:preserve-definition-buffer-on-exit t
  "Keep the current system browser definition buffer and file alive when the system browser is closed."
  :type 'boolean
  :group 'system-browser
  :tag "Preserve definition buffer on exit")

;;------- Faces --------------------------

(defface ssb:definition-list-item-face
  '((((background light))
     :foreground "black"
     :height 0.9)
    (((background dark))
     :foreground "white"
     :height 0.9))
  "Face for system-browser definitions list items"
  :group 'system-browser-faces)

(defface ssb:mode-line-buttons-face
  '((((background light))
     :box (:line-width 2 :color "dark grey")
     :background "light grey" :foreground "black")
    (((background dark))
     :box (:line-width 2 :color "dark grey")
     :background "black" :foreground "white"))
  "Face for system-browser buttons in mode-line"
  :group 'system-browser-faces)

(defface ssb:definitions-list-header-face
  '((t :inherit bold))
  "Face for system-browser definitions list headers"
  :group 'system-browser-faces)

;;-------- Buffers ---------------------------------

(defvar-local ssb:system-browser-buffer-type nil)

(defvar ssb:mode-line-toggle-docs-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] 'system-browser-toggle-docs)
    map))

(defun ssb:setup-selection-list-buffer ()
  ;; TODO: the following COPY-FACE is global. We need to do something to apply locally.
  (copy-face 'mode-line 'header-line)
  (setq header-line-format mode-line-format)
  (setq mode-line-format nil)
  (hl-line-mode)
  (system-browser-mode)
  (system-browser-sel-mode))

(defun ssb:initialize-modules-buffer ()
  (setq ssb:modules-buffer (get-buffer-create "*ssb-modules*"))
  (with-current-buffer ssb:modules-buffer
    (ssb:setup-selection-list-buffer)
    (when (ssb:modules-buffer-mode-line-format ssb:current-browser-system)
      (setq header-line-format (ssb:modules-buffer-mode-line-format ssb:current-browser-system)))
    (setq ssb:system-browser-buffer-type 'modules)))

(defun ssb:initialize-categories-buffer ()
  (setq ssb:categories-buffer (get-buffer-create "*ssb-categories*"))
  (with-current-buffer ssb:categories-buffer
    (ssb:setup-selection-list-buffer)
    (when (ssb:categories-buffer-mode-line-format ssb:current-browser-system)
      (setq header-line-format (ssb:categories-buffer-mode-line-format ssb:current-browser-system)))
    (setq ssb:system-browser-buffer-type 'categories)))

(defun ssb:initialize-definitions-buffer ()
  (setq ssb:definitions-buffer (get-buffer-create "*ssb-definitions*"))
  (with-current-buffer ssb:definitions-buffer
    (ssb:setup-selection-list-buffer)
    (when (ssb:definitions-buffer-mode-line-format ssb:current-browser-system)
      (setq header-line-format (ssb:definitions-buffer-mode-line-format ssb:current-browser-system)))
    (setq ssb:system-browser-buffer-type 'definitions)))

(defun ssb:initialize-definition-buffer ()
  (setq ssb:definition-buffer (get-buffer-create "*ssb-definition*"))

  (with-current-buffer ssb:definition-buffer
    (setq buffer-read-only nil)

    (ssb:system-initialize-definition-buffer ssb:current-browser-system)

    ;; Show visited file in mode-line
    (setq mode-line-format (cons '(:eval (file-name-nondirectory buffer-file-name))
                                 (cdr mode-line-format)))

    ;; Buttons in mode-line
    (push '(:eval (propertize "toggle docs "
                              'local-map ssb:mode-line-toggle-docs-map
                              'face 'ssb:mode-line-buttons-face
                              'mouse-face 'mode-line-highlight))
          mode-line-format)
    (push '(:eval (propertize "quit "
                              'local-map quit-system-browser
                              'face 'ssb:mode-line-buttons-face
                              'mouse-face 'mode-line-highlight))
          mode-line-format)

    (setq ssb:system-browser-buffer-type 'definition)

    (system-browser-mode)))

(defun ssb:initialize-documentation-buffer ()
  (setq ssb:documentation-buffer (get-buffer-create "*ssb-documentation*"))
  (with-current-buffer ssb:documentation-buffer
    (setq ssb:system-browser-buffer-type 'documentation)))

(defun ssb:update-modules-buffer ()
  (let ((modules (ssb:list-modules ssb:current-browser-system)))
    (with-current-buffer ssb:modules-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (dolist (module-name modules)
        (insert-button (if ssb:downcase-definition-names
                           (downcase module-name)
                         module-name)
                       'action (lambda (btn)
                                 (ignore btn)
                                 (ssb:select-module module-name))
                       'face 'ssb:definition-list-item-face
                       'follow-link t
                       'help-echo "Browse module")
        (newline))
      (setq buffer-read-only t))
    (when modules
      (ssb:select-module (cl-first modules)))))

(defun ssb:select-module (module)
  (oset ssb:current-browser-system selected-module module)
  (ssb:update-categories-buffer module)
  (wlf:select ssb:wm 'modules)
  ;; Move cursor to the line of the selection
  (let ((item-pos (1+ (cl-position module (ssb:list-modules ssb:current-browser-system) :test 'cl-equalp))))
    (with-current-buffer ssb:modules-buffer
      (goto-line item-pos))))

(defun ssb:update-categories-buffer (module)
  (let ((categories (ssb:list-categories ssb:current-browser-system module)))
    (with-current-buffer ssb:categories-buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (propertize module
			  'face 'ssb:definitions-list-header-face))
      (newline)
      (dolist (category categories)
        (insert-button category
                       'action (lambda (btn)
                                 (ignore btn)
                                 (ssb:select-category module category))
                       'follow-link t
                       'face 'ssb:definition-list-item-face
                       'help-echo "Browse category")
        (newline))
      (setq buffer-read-only t))

    (let-alist (ssb:get-module-properties ssb:current-browser-system module)

      ;; Show module definition source in definition buffer
      (if (and .file .position)
          (progn
            (when (not (buffer-live-p ssb:definition-buffer))
              (ssb:initialize-definition-buffer))
            (ssb:set-definition-buffer-file .file .position)
            (ssb:set-documentation-buffer-contents (or .documentation "This module is not documented."))
            (ssb:select-category module (cl-first categories)))
        (message "Definition source not found.")
        ))))

(defun ssb:select-category (module category)
  (oset ssb:current-browser-system selected-category  category)
  (ssb:update-definitions-buffer module category)
  (wlf:select ssb:wm 'categories)
  (let ((item-pos (1+ (cl-position category (ssb:list-categories ssb:current-browser-system module) :test 'cl-equalp))))
    (with-current-buffer ssb:categories-buffer
      (goto-line (1+ item-pos)))))

(defun ssb:update-definitions-buffer (module category)
  (with-current-buffer ssb:definitions-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (propertize category 'face
                        'ssb:definitions-list-header-face))
    (newline)
    (dolist (definition (ssb:list-definitions ssb:current-browser-system module category))
      (insert-button (if ssb:downcase-definition-names
                         (downcase definition)
                       definition)
                     'action (lambda (btn)
                               (ignore btn)
                               (ssb:select-definition module category definition))
                     'face 'ssb:definition-list-item-face
                     'follow-link t
                     'help-echo "Browse definition")
      (newline))
    (setq buffer-read-only t))
  (wlf:select ssb:wm 'definitions)
  (goto-char 1))

(defun ssb:select-definition (module category definition)
  (oset ssb:current-browser-system selected-definition definition)
  (ssb:update-definition-buffer module category definition)
  (ssb:update-documentation-buffer module category definition)
  (wlf:select ssb:wm 'definitions)

  ;; Move cursor to the line of the selection
  (let ((item-pos (1+ (cl-position definition (ssb:list-definitions ssb:current-browser-system module category) :test 'cl-equalp))))
    (with-current-buffer ssb:definitions-buffer
      (goto-line (1+ item-pos)))))

(defun ssb:set-definition-buffer-file (file &optional position)
  (wlf:select ssb:wm 'definition)
  (wlf:set-buffer ssb:wm 'definition ssb:definition-buffer)

  (cl-block func
    (with-current-buffer ssb:definition-buffer

      ;; Check for unsaved changes in definition buffer
      (when (buffer-modified-p)
        (when (not (yes-or-no-p "System Browser definition buffer modified. Discard changes? "))
          (cl-return-from func)))

      ;; For some reason, sometimes definition buffer sets to read-only.
      ;; The following prevents that:
      (setq buffer-read-only nil)

      (erase-buffer)
      (insert-file-contents file)
      ;; Assign file to buffer so changes in definition buffer can be saved
      (setq buffer-file-name file)
      (setq default-directory (file-name-directory file))

      (set-buffer-modified-p nil)

      (when position
        (goto-char position)
        (recenter-top-bottom 0)))))

(defun ssb:update-definition-buffer (module category definition)
  (when (not (buffer-live-p ssb:definition-buffer))
    (ssb:initialize-definition-buffer))
  (let-alist (ssb:get-definition-properties ssb:current-browser-system
					    definition category module)
      (if (and .file .position)
          (with-current-buffer ssb:definition-buffer
            (wlf:select ssb:wm 'definition)
            (switch-to-buffer ssb:definition-buffer nil t)
            (ssb:set-definition-buffer-file .file .position))
        (message "Definition source not found."))))

(defun ssb:set-documentation-buffer-contents (contents)
  (with-current-buffer ssb:documentation-buffer
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert contents)
    (goto-char 0)
    (setq buffer-read-only t)))

(defun ssb:update-documentation-buffer (module category definition)
  (let-alist (ssb:get-definition-properties ssb:current-browser-system
					    definition category module)
    (let ((contents (or .documentation "This definition is not documented.")))
      (when (string= category "variables")
        (setq contents (concat contents "\n\n"))
        (if (not .boundp)
            (setq contents (concat contents "The variable is UNBOUND."))
          (progn
            (setq contents (concat contents (propertize "Variable value: " 'face 'bold)))
            (setq contents (concat contents .value)))))
      (ssb:set-documentation-buffer-contents contents))))

;;---- Window management ---------------------------

(defvar ssb:wm)

(defun ssb:set-windows-dedicated ()
  (let ((winfo-list (wlf:wset-winfo-list ssb:wm)))
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'modules winfo-list)) t)
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'categories winfo-list)) t)
    (set-window-dedicated-p (wlf:window-window (wlf:get-winfo 'definitions winfo-list)) t)))

(defun ssb:initialize-windows ()
  (setq ssb:wm
        (wlf:layout
         '(| (:left-size-ratio 0.20)
             (- (:left-size-ratio 0.33)
                modules
                (- categories
                   definitions))
             (- (:left-size-ratio 0.66)
                definition
                documentation))
         '((:name modules
                  :buffer "*ssb-modules*")
           (:name categories
                  :buffer "*ssb-categories*")
           (:name definitions
                  :buffer "*ssb-definitions*")
           (:name definition
                  :buffer "*ssb-definition*")
           (:name documentation
                  :buffer "*ssb-documentation*")
           )))
  ;; Mark selection windows as dedicated
  (ssb:set-windows-dedicated))

(defun ssb:system-browser-initialize ()
  ;; Initialize system browser buffers
  (ssb:initialize-modules-buffer)
  (ssb:initialize-categories-buffer)
  (ssb:initialize-definitions-buffer)
  (ssb:initialize-definition-buffer)
  (ssb:initialize-documentation-buffer)

  (ssb:initialize-windows)

  (when (not ssb:show-documentation-buffer)
    (wlf:hide ssb:wm 'documentation)
    ;; There's a bug with wlf:hide, that removes the window-dedicated-p flag from the windows.
    ;; So, we reestablish it here:
    (ssb:set-windows-dedicated))

  (ssb:update-modules-buffer)
  (wlf:select ssb:wm 'modules))

(defun system-browser ()
  "Open the currently instantiated system browser."
  (interactive)
  
  (ssb:system-browser-initialize)
  (run-hooks 'system-browser-start-hook))

;;------- Commands ------------------------------------------------

(defun system-browser-reset-layout ()
  "Reset system browser layout. Use this when Emacs windows break the browser's layout."
  (interactive)
  (wlf:reset-window-sizes ssb:wm)
  (ssb:initialize-windows))

(defun quit-system-browser ()
  "Quit the system browser."
  (interactive)

  ;; Try killing the definition buffer first, as it may have been modified
  (kill-buffer ssb:definition-buffer)

  (kill-buffer ssb:modules-buffer)
  (kill-buffer ssb:categories-buffer)
  (kill-buffer ssb:definitions-buffer)
  (kill-buffer ssb:documentation-buffer)

  (wlf:clear-windows ssb:wm t))

(defun system-browser-browse-module (module-name)
  "Browse a particular module completed from command bar."
  (interactive (list (ssb:read-module-name ssb:current-browser-system "Browse module: ")))
  (ssb:select-module module-name))

(defun system-browser-browse-definition (definition-name)
  "Browse a definition in current module and category."
  (interactive (list (completing-read (format "Browse definition in %s %s: "
                                              (ssb:selected-module ssb:current-browser-system)
                                              (ssb:selected-category ssb:current-browser-system))
                                      (ssb:list-definitions
                                       ssb:current-browser-system
                                       (ssb:selected-module ssb:current-browser-system)
                                       (ssb:selected-category ssb:current-browser-system))
                                      nil t)))
  (ssb:select-definition
   (ssb:selected-module ssb:current-browser-system)
   (ssb:selected-category ssb:current-browser-system)
   definition-name))

(defun system-browser-next-module ()
  "Select next module in system browser."
  (interactive)
  (let* ((modules (ssb:list-modules ssb:current-browser-system))
         (module (ssb:selected-module ssb:current-browser-system))
         (position (cl-position module modules :test 'cl-equalp))
         (next-module (nth (1+ position) modules)))
    (when next-module
      (ssb:select-module next-module))))

(defun system-browser-prev-module ()
  "Select previous module in system browser."
  (interactive)
  (let* ((modules (ssb:list-modules ssb:current-browser-system))
         (module (ssb:selected-module ssb:current-browser-system))
         (position (cl-position module modules :test 'cl-equalp))
         (prev-module (nth (1- position) modules)))
    (when prev-module
      (ssb:select-module prev-module))))

(defun system-browser-next-category ()
  "Select next category in system browser."
  (interactive)
  (let* ((categories (ssb:list-categories ssb:current-browser-system
                                          (ssb:selected-module ssb:current-browser-system)))
         (category (ssb:selected-category ssb:current-browser-system))
         (position (cl-position category categories :test 'cl-equalp))
         (next-category (nth (1+ position) categories)))
    (when next-category
      (ssb:select-category (ssb:selected-module ssb:current-browser-system)
                           next-category))))

(defun system-browser-prev-category ()
  "Select previous category in system browser."
  (interactive)
  (let* ((categories (ssb:list-categories ssb:current-browser-system
                                          (ssb:selected-module ssb:current-browser-system)))
         (category (ssb:selected-category ssb:current-browser-system))
         (position (cl-position category categories :test 'cl-equalp))
         (prev-category (nth (1- position) categories)))
    (when prev-category
      (ssb:select-category (ssb:selected-module ssb:current-browser-system)
                           prev-category))))

(defun system-browser-next-definition ()
  "Select next definition in system browser."
  (interactive)
  (let* ((definitions (ssb:list-definitions ssb:current-browser-system
                                            (ssb:selected-module ssb:current-browser-system)
                                            (ssb:selected-category ssb:current-browser-system)))
         (definition (ssb:selected-definition ssb:current-browser-system))
         (position (cl-position definition definitions :test 'cl-equalp))
         (next-definition (or (and position (nth (1+ position) definitions))
                              (cl-first definitions))))
    (when next-definition
      (ssb:select-definition
       (ssb:selected-module ssb:current-browser-system)
       (ssb:selected-category ssb:current-browser-system)
       next-definition))))

(defun system-browser-prev-definition ()
  "Select previous definition in system browser."
  (interactive)
  (let* ((definitions (ssb:list-definitions ssb:current-browser-system
                                            (ssb:selected-module ssb:current-browser-system)
                                            (ssb:selected-category ssb:current-browser-system)))
         (definition (ssb:selected-definition ssb:current-browser-system))
         (position (cl-position definition definitions :test 'cl-equalp))
         (prev-definition (and position (nth (1- position) definitions))))
    (when prev-definition
      (ssb:select-definition
       (ssb:selected-module ssb:current-browser-system)
       (ssb:selected-category ssb:current-browser-system)
       prev-definition))))

(defun system-browser-next-selection ()
  "Select next item in system browser selection buffer."
  (interactive)
  (cl-case ssb:system-browser-buffer-type
    (modules (system-browser-next-module))
    (categories (system-browser-next-category))
    (definitions (system-browser-next-definition))
    (t (error "Invalid buffer"))))

(defun system-browser-prev-selection ()
  "Select previous item in system browser selection buffer."
  (interactive)
  (cl-case ssb:system-browser-buffer-type
    (modules (system-browser-prev-module))
    (categories (system-browser-prev-category))
    (definitions (system-browser-prev-definition))))

(defun system-browser-find-selection ()
  "Find a selection in the current system browser selection list buffer."
  (interactive)
  (cl-case ssb:system-browser-buffer-type
    (modules (call-interactively 'system-browser-browse-module))
    (definitions (call-interactively 'system-browser-browse-definition))))

(defun system-browser-refresh (&optional hard)
  "Refresh the system browser contents and reset its layout."
  (interactive)
  (when hard
    (setq ssb:current-browser-system (make-instance (eieio-object-class ssb:current-browser-system))))
  (ssb:system-browser-initialize))

(defun system-browser-toggle-docs ()
  "Toggle documentation panel in system browser."
  (interactive)
  (wlf:toggle ssb:wm 'documentation)
  ;; There's a bug with wlf:toggle, that removes the window-dedicated-p flag from the windows.
  ;; So, we reestablish it here:
  (ssb:set-windows-dedicated))

(defun system-browser-toggle-internal-definitions ()
  "Toggle internal definitions listing in system browser."
  (interactive)
  (setq ssb:list-internal-definitions (not ssb:list-internal-definitions))
  (system-browser-refresh))

(defun system-browser-customize ()
  "Customize system browser."
  (interactive)
  (customize-group 'system-browser))

(defun system-browser-help ()
  "Show help about system browser."
  (interactive)
  (apropos-command "system-browser"))

;;------ Menu ----------------------------

(defvar system-browser-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-q") 'quit-system-browser)
    map))

(define-minor-mode system-browser-mode
  "System browser minor mode."
  :init-value nil
  :lighter " system-browser"
  :keymap system-browser-mode-map
  :group 'system-browser)

(easy-menu-define
  system-browser-mode-menu system-browser-mode-map
  "Menu for system-browser"
  '("System Browser"
    ["Browse module..." system-browser-browse-module
     :help "Browse a module"]
    ["Browse definition..." system-browser-browse-definition
     :help "Browse a definition"]
    "--"
    ["Refresh browser" system-browser-refresh
     :help "Refresh the system browser"]
    ["Reset layout" system-browser-reset-layout
     :help "Reset system browser windows layout"]
    ["Toggle internal defintions" system-browser-toggle-internal-definitions
     :help "Toggle the listing of internal defintions"]
    ["Toggle documentation panel" system-browser-toggle-docs
     :help "Toggle documentation panel"]
    "--"
    ["Settings" system-browser-customize
     :help "Customize system browser"]
    ["Help" system-browser-help
     :help "Help on system browser"]
    ["Quit" quit-system-browser
     :help "Quit System Browser"]))

(defun system-browser-cycle-next-module (letter)
  (let* ((modules (ssb:list-modules ssb:current-browser-system))
         (position (1+ (or (and (ssb:selected-module ssb:current-browser-system)
                                (cl-position (ssb:selected-module ssb:current-browser-system)
                                             modules :test 'cl-equalp))
                           -1))))
    ;; Find next module that begins with LETTER, starting from POSITION
    (cl-flet ((find-next-module (position)
                (cl-find-if (lambda (module)
                              (char-equal (aref module 0) letter))
                            (cl-subseq modules position))))
      (let ((module (or (find-next-module position)
                        (find-next-module 0))))
        (when module
          (ssb:select-module module))))))

(defun system-browser-cycle-next-category (letter)
  (let* ((categories (ssb:list-categories ssb:current-browser-system
                                          (ssb:selected-module ssb:current-browser-system)))
         (position (1+ (or (and (ssb:selected-category ssb:current-browser-system)
                                (cl-position (ssb:selected-category ssb:current-browser-system)
                                             categories :test 'cl-equalp))
                           -1))))
    ;; Find next category that begins with LETTER, starting from POSITION
    (cl-flet ((find-next-category (position)
                (cl-find-if (lambda (category)
                              (char-equal (aref category 0) letter))
                            (cl-subseq categories position))))
      (let ((category (or (find-next-category position)
                          (find-next-category 0))))
        (when category
          (ssb:select-category (ssb:selected-module ssb:current-browser-system) category))))))

(defun system-browser-cycle-next-definition (letter)
  (let* ((definitions (ssb:list-definitions ssb:current-browser-system
                                            (ssb:selected-module ssb:current-browser-system)
                                            (ssb:selected-category ssb:current-browser-system)))
         (position (1+ (or (and (ssb:selected-definition ssb:current-browser-system)
                                (cl-position (ssb:selected-definition ssb:current-browser-system)
                                             definitions :test 'cl-equalp))
                           -1))))
    ;; Find next definition that begins with LETTER, starting from POSITION
    (cl-flet ((find-next-definition (position)
                (cl-find-if (lambda (definition)
                              (char-equal (aref definition 0) letter))
                            (cl-subseq definitions position))))
      (let ((definition (or (find-next-definition position)
                            (find-next-definition 0))))
        (when definition
          (ssb:select-definition
           (ssb:selected-module ssb:current-browser-system)
           (ssb:selected-category ssb:current-browser-system)
           definition))))))

(defun system-browser-cycle-selection ()
  (interactive)
  (let ((letter (aref (this-command-keys) 0)))
    (cl-case ssb:system-browser-buffer-type
      (modules (system-browser-cycle-next-module letter))
      (categories (system-browser-cycle-next-category letter))
      (definitions (system-browser-cycle-next-definition letter)))))

(defun system-browser-switch-next-buffer ()
  (interactive)
  (when (not (null ssb:system-browser-buffer-type))
    (let* ((windows '(modules categories definitions definition))
           (next-window (nth (mod (1+ (cl-position ssb:system-browser-buffer-type windows)) (length windows)) windows)))
      (wlf:select ssb:wm next-window))))

(defun system-browser-switch-prev-buffer ()
  (interactive)
  (when (not (null ssb:system-browser-buffer-type))
    (let* ((windows '(modules categories definitions definition))
           (next-window (nth (mod (1- (cl-position ssb:system-browser-buffer-type windows)) (length windows)) windows)))
      (wlf:select ssb:wm next-window))))

(defvar system-browser-sel-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-p") 'system-browser-prev-selection)
    (define-key map (kbd "C-n") 'system-browser-next-selection)
    (define-key map (kbd "<down>") 'system-browser-next-selection)
    (define-key map (kbd "<up>") 'system-browser-prev-selection)
    (define-key map (kbd "C-f") 'system-browser-find-selection)
    (define-key map (kbd "TAB") 'system-browser-switch-next-buffer)
    (define-key map (kbd "<backtab>") 'system-browser-switch-prev-buffer)
    (define-key map (kbd "<right>") 'system-browser-switch-next-buffer)
    (define-key map (kbd "<left>") 'system-browser-switch-prev-buffer)
    (dolist (char (cl-coerce "abcdefghijklmnñopqrstuvwxyz" 'list))
      (define-key map (string char) 'system-browser-cycle-selection))
    map))

(define-minor-mode system-browser-sel-mode
  "Minor mode for Emacs System Browser selection list buffers."
  :init-value nil
  :lighter " system-browser-sel"
  :keymap system-browser-sel-mode-map
  :group 'system-browser-sel)

(provide 'system-browser)

;;; system-browser.el ends here
