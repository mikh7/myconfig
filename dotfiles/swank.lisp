;; (ql:quickload :log4cl)
(log:info "~~/.swank.lisp loading")

(setq swank:*use-dedicated-output-stream* t)

;; (pushnew '(*print-right-margin* . nil)
;;          swank:*default-worker-thread-bindings*)

;; (defclass cc-ck-fake-system (asdf:system)
;;   ())

;; (defmethod asdf:system-source-directory ((system cc-ck-fake-system))
;;   (pathname-directory-pathname (system-source-file)))

;; (setf swank:*fasl-pathname-function* 
;;       (lambda (input-file options)
;;         (log:expr input-file options)
;;         (ensure-directories-exist 
;;          (or (ignore-errors
;;               (first 
;;                (asdf:output-files 
;;                 (make-instance 'asdf:compile-op)
;;                 (make-instance 'asdf:cl-source-file :name (pathname-name (pathname input-file))
;;                  :parent 
;;                  (make-instance 'asdf:system :pathname #p"/" :name "")
;;                  :pathname input-file))))
;;              (compile-file-pathname input-file)))))

;; Make C-c C-k in SLIME also use the correct directory
(let (fasl-finder-sym)
  (when (and (find-package :swank)
             (setq fasl-finder-sym
                   (find-symbol "*FASL-PATHNAME-FUNCTION*" :swank))
             (null (symbol-value fasl-finder-sym)))
    (set fasl-finder-sym
         (lambda (path options)
           (declare (ignore options))
           (let ((fasl-path
                   (asdf:apply-output-translations (compile-file-pathname path))))
             (when fasl-path
               (ensure-directories-exist fasl-path)
               fasl-path))))))

(swank:swank-require :swank-listener-hooks)
(swank:swank-require :swank-arglists)

(in-package :swank)

;; This fixes defmethod highlighting when closer-mop redefines defmethod
(defmethod arglist-dispatch :around ((operator t) arguments)
  (let* ((closer-package (find-package :closer-mop))
         (closer-defmethod (when closer-package
                             (find-symbol (symbol-name 'cl:defmethod)
                                          closer-package))))
    (if (eq operator closer-defmethod)
        (arglist-dispatch 'cl:defmethod arguments)
        (call-next-method))))


(defmethod arglist-dispatch ((operator (eql 'demacs:def)) arguments)
  "Highlight demacs'def"
  (flet ((is-method (elem)
           (or (eq elem 'method)
               (and (consp elem)
                    (eq (car elem) 'method)))))
    (match (cons operator arguments)
      (('demacs:def (#'is-method elem) (#'function-exists-p gf-name) . rest)
       (let ((gf (fdefinition gf-name)))
         (when (typep gf 'generic-function)
           (with-available-arglist (arglist) (decode-arglist (arglist gf))
             (let ((qualifiers (loop for x in rest
                                  until (or (listp x) (empty-arg-p x))
                                  collect x)))
               (return-from arglist-dispatch
                 (make-arglist :provided-args `(method ,gf-name ,@qualifiers)
                               :required-args `(,arglist)
                               :rest "body" :body-p t)))))))
      (_))                              ; Fall through
    )
  (call-next-method))

(defvar *compile-string-filename* nil
  "File where form being compiled by Slime `slime-compile-defun' comes from")

(export '*compile-string-filename*)

(defvar *old-compile-string-for-emacs*
  (fdefinition 'compile-string-for-emacs))

(setf (fdefinition 'compile-string-for-emacs)
      (lambda (string buffer position filename policy)
        (let ((*compile-string-filename* filename))
          (funcall *old-compile-string-for-emacs*
                   string buffer position filename policy))))

#+clisp
(defun swank-backend::fspec-pathname (spec)
  (let ((path spec)
	type
        lines)
    (when (and (consp path)
               (consp (car path))
               (endp (cdr path))
               (pathnamep (cadar path)))
      (setq path (first path)))
    (when (consp path)
      (psetq type (car path)
	     path (cadr path)
             lines (cddr path)))
    (when (and path (pathnamep path))
      (let* ((dir (pathname-directory path))
             (pos (search '(".cache" "common-lisp") dir :test 'equal)))
        (when pos 
          (setq path
                (merge-pathnames
                 (make-pathname
                  :directory
                  (cons :absolute (subseq dir (+ pos 3))))
                 path)))))
    (when (and path
               (member (pathname-type path)
                       custom:*compiled-file-types* :test #'equal))
      (setq path
            (loop for suffix in custom:*source-file-types*
                  thereis (probe-file (make-pathname :defaults path
                                                     :type suffix)))))
    (values path type lines)))

(defslimefun mm/save-snapshot (image-file)
  (let* ((connection *emacs-connection*)
         (style (connection.communication-style connection)))
    (flet ((complete (success)
	     (swank::with-connection (connection)
	       (swank::background-message
		"Dumping lisp image ~A ~:[failed!~;succeeded.~]" 
		image-file success)))
	   (awaken ()
             (setq *connections* nil *emacs-connection* nil)
             #+sbcl(sb-impl::toplevel-repl nil)
             (error "After toplevel REPL")))
      (swank-backend:background-save-image
       image-file
       :restart-function #'awaken
       :completion-function #'complete
       :communication-style style)
      (format nil "Started dumping lisp to ~A..." image-file))))

(log:info "~~/.swank.lisp done")
