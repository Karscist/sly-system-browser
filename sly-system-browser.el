;;; system-browser.el --- System Browser backend for Common Lisp   -*- lexical-binding: t -*-

;;; Commentary:
;;; System Browser backend for Common Lisp.

;;; Code:

(require 'system-browser)
(require 'sly)

(defgroup system-browser-cl nil
  "System Browser Common Lisp backend."
  :group 'system-browser)

(defcustom ssb/cl:start-sly-automatically nil
  "When enabled, Sly is started automatically when system browser is started."
  :type 'boolean
  :group 'system-browser-cl
  :tag "Start Sly automatically")

(defcustom ssb/cl:load-asdf-systems-on-browse t
  "When enabled, load ASDF systems before browsing them."
  :type 'boolean
  :group 'system-browser-cl
  :tag "Load ASDF systems on browse")

(defcustom ssb/cl:asdf-system (cons "" nil)
  "When set, system-browser will browse the ASDF system on start.
The first argument specifies the ASDF system name.
The second argument indicates if include system's direct dependencies or not."
  :type '(cons (string :tag "ASDF system name")
               (boolean :tag "Include direct dependencies"))
  :group 'system-browser-cl
  :tag "ASDF system")

(add-hook 'system-browser-start-hook 'ssb/cl:maybe-browse-customized-asdf-system)

(defclass ssb/cl:common-lisp-system (ssb:system-browser-system)
  ((modules-list-function :accessor ssb:modules-list-function
                          :initform nil
                          :documentation "Function used to get the list of modules, when present")))

(defun ssb/cl:list-all-cl-packages ()
  "Return list of Common Lisp packages."
  (sly-eval '(cl:sort (cl:mapcar 'cl:package-name (cl:list-all-packages)) 'cl:string<)))

(defun ssb/cl:asdf-system-packages (system-name &optional include-direct-dependencies)
  (sly-eval `(ssb:asdf-system-packages ,system-name ,include-direct-dependencies)))

(cl-defmethod ssb:list-modules ((system ssb/cl:common-lisp-system))
  (if (ssb:modules-list-function system)
      (funcall (ssb:modules-list-function system))
    (ssb/cl:list-all-cl-packages)))

(defun ssb/cl:maybe-browse-customized-asdf-system ()
  (when (not (zerop (length (car ssb/cl:asdf-system))))
    (when (cdr ssb/cl:asdf-system)
      (setq current-prefix-arg (cdr ssb/cl:asdf-system)))
    (system-browser-browse-system (car ssb/cl:asdf-system))))

(cl-defmethod ssb:system-initialize-definition-buffer ((system ssb/cl:common-lisp-system))
  (lisp-mode))

(cl-defmethod ssb:get-module-properties ((system ssb/cl:common-lisp-system) module)
  (let* ((module-properties (sly-eval `(ssb::serialize-for-emacs (def-properties:package-properties ,module t))))
         (source (cl-find :source module-properties :key 'car))
         (file (and source
                    (or (cadr (cl-find :file (cl-remove-if-not 'listp source) :key 'car))
                        (caddr (cl-find :buffer-and-file (cl-remove-if-not 'listp source) :key 'car)))))
         (position (and source (or
                                (cadr (cl-find :position (cl-remove-if-not 'listp source) :key 'car))
                                (cadr (cl-find :offset (cl-remove-if-not 'listp source) :key 'car)))))
         (documentation (cdr (assoc :documentation module-properties))))
    (list (cons 'source source)
	  (cons 'file file)
	  (cons 'position position)
	  (cons 'documentation documentation))))

(cl-defmethod ssb:get-definition-properties ((system ssb/cl:common-lisp-system) definition category module)
  (let ((definition-function
         (cond
          ((string= category "functions") 'def-properties:function-properties)
          ((string= category "variables") 'def-properties:variable-properties)
          ((string= category "macros") 'def-properties:macro-properties)
          ((string= category "classes") 'def-properties:class-properties)
          ((string= category "generic functions") 'def-properties:generic-function-properties)
          (t (error "Invalid category: %s" category)))))
    (let* ((definition-properties (sly-eval `(ssb:serialize-for-emacs (,definition-function (cl:intern ,definition ,module) t))))
           (source (cl-find :source definition-properties :key 'car))
           (file (and source (or
                              (cadr (cl-find :file (cl-remove-if-not 'listp source) :key 'car))
                              (caddr (cl-find :buffer-and-file (cl-remove-if-not 'listp source) :key 'car)))))
           (position (and source (or
                                  (cadr (cl-find :position (cl-remove-if-not 'listp source) :key 'car))
                                  (cadr (cl-find :offset (cl-remove-if-not 'listp source) :key 'car)))))
	   (documentation (cdr (assoc :documentation definition-properties))))
      (list (cons 'source source)
	    (cons 'file file)
	    (cons 'position position)
	    (cons 'documentation documentation)))))

(cl-defmethod ssb:read-module-name ((system ssb/cl:common-lisp-system) prompt)
  (sly-read-package-name prompt))

(cl-defmethod ssb:list-categories ((system ssb/cl:common-lisp-system) module)
  (ignore system module)
  '("functions" "variables" "macros" "classes" "generic functions"))

(cl-defmethod ssb:list-definitions ((system ssb/cl:common-lisp-system) module category)
  (ignore system)
  (let ((definition-type
         (cond
          ((string= category "functions") :function)
          ((string= category "variables") :variable)
          ((string= category "macros") :macro)
          ((string= category "classes") :class)
          ((string= category "generic functions") :generic-function))))

    (sly-eval `(ssb:list-definitions ,module ,definition-type :include-internal-p ,ssb:list-internal-definitions))))

(defun system-browser-browse-system (system-name)
  "Browse ASDF system packages."
  (interactive (list (sly-asdf-read-system-name)))
  (if (zerop (length system-name))
      (oset ssb:current-browser-system modules-list-function nil)
    (let ((include-direct-dependencies (not (null current-prefix-arg))))
      (when ssb:load-asdf-systems-on-browse
        (sly-eval `(cl:progn (asdf:operate 'asdf:load-op ,system-name) nil)))
      (oset ssb:current-browser-system modules-list-function
            (lambda ()
              (ssb:asdf-system-packages system-name include-direct-dependencies)))))
  (system-browser-refresh))

(defun sly-system-browser ()
  "Open the Common Lisp system browser."
  (interactive)
  (cl-block system-browser

    ;; Start Sly if needed
    (when (not (sly-connected-p))
      (when (or ssb:start-sly-automatically
                (yes-or-no-p "Sly is not connected. Start? "))
        (add-hook 'sly-connected-hook 'system-browser t)
        (sly))
      (cl-return-from system-browser))
    
    (setq ssb:current-browser-system (make-instance 'ssb/cl:common-lisp-system))
    (system-browser)))

;;------ SLY --------------------------------------------

(define-sly-contrib sly-system-browser
  "Smalltalk-like system browser for Common Lisp"
  (:authors "Ethereal")
  (:license "GPL")
  (:sly-dependencies sly-asdf)
  (:slynk-dependencies slynk-system-browser))

(provide 'sly-system-browser)

;;; system-browser-cl.el ends here
