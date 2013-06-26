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


(defparameter *mm/log-config* '(:sane2 :thread -12 :pretty :nopackage :info))

(defun mm/reset-logging ()
  (log4cl:clear-logging-configuration)
  (apply 'log:config *mm/log-config*))

(mm/reset-logging)

(defun mm/slime-repl-connected ()
  (apply 'log:config (remove :info *mm/log-config*)))

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
             (mm/reset-logging)
             #+sbcl (sb-impl::toplevel-repl nil)
             (error "After toplevel REPL")))
      (swank-backend:background-save-image
       image-file
       :restart-function #'awaken
       :completion-function #'complete
       :communication-style style)
      (format nil "Started dumping lisp to ~A..." image-file))))

#+sbcl
(defun mydump (filename &key restart-function
                             completion-function
                             executable
                             save-runtime-options
                             compression)
  (declare (type function restart-function completion-function))
  (let* (pid
         (connection *emacs-connection*)
         (style (connection.communication-style connection)))
    (labels ((restart-sbcl ()
               (sb-debug::enable-debugger)
               (setf sb-impl::*descriptor-handlers* nil)
               (setq *connections* nil *emacs-connection* nil)
               (mm/reset-logging)
               (funcall restart-function))
             (logg (&rest args)
               (ignore-errors 
                (apply #'format sb-sys:*tty* args)
                (terpri sb-sys:*tty*)
                (finish-output sb-sys:*tty*)))
             (waiter (&optional fd)
               ;; (logg "FD-HANDLER for pid ~d fd ~d" pid fd)
               (when fd 
                 ;; (logg "before invalidate ~d" fd) 
                 (sb-sys:invalidate-descriptor fd) 
                 ;; (logg "before close ~d" fd)
                 (sb-posix:close fd))
               ;; (logg "before waitpid")
               (multiple-value-bind (rpid status) (sb-posix:waitpid pid 0)
                 ;; (logg "waitpid returned")
                 (assert (= pid rpid))
                 (assert (sb-posix:wifexited status))
                 (funcall completion-function
                          (zerop (sb-posix:wexitstatus status))))
               ;; (logg "After waitpid")
               )
             (forker () 
               (if (eq style :spawn) 
                   (cond ((= (setq pid (sb-posix:fork)) 0)
                          (sb-debug::disable-debugger)
                          ;; (logg "I'm a child in forker()")
                          (apply #'sb-ext:save-lisp-and-die filename
                                 (when restart-function
                                   (list :toplevel #'restart-sbcl))))
                         (t
                          ;; (logg "I'm a parent of ~d, will use thread to wait" pid)
                          (spawn #'waiter :name "background-save-image")))
                   (multiple-value-bind (pipe-in pipe-out) (sb-posix:pipe)
                     (cond ((= (setq pid (sb-posix:fork)) 0)
                            (sb-posix:close pipe-in)
                            (sb-debug::disable-debugger)
                            ;; (logg "I'm a child in forker()")
                            
                            (setf sb-impl::*descriptor-handlers* nil)
                            (setq *connections* nil *emacs-connection* nil)
                            (log4cl:clear-logging-configuration)
                            (apply 'log:config *mm/log-config*)
                            ;; (logg "I'm a child in forker()")
                            (sb-ext:save-lisp-and-die
                             filename
                             :executable executable 
                             :save-runtime-options save-runtime-options
                             :toplevel #'restart-sbcl
                             :save-runtime-options save-runtime-options
                             :compression compression))
                           (t
                            ;; (logg "I'm a parent of ~d, will use fd-handler to wait" pid)
                            (sb-posix:close pipe-out)
                            (sb-sys:add-fd-handler pipe-in :input #'waiter)
                            ;; (logg "Added fd handler to ~d~% handlers are now ~s" pipe-in
                            ;;       sb-impl::*descriptor-handlers*)
                            ))))))
      (call-with-only-initial-thread #'forker))))


(log:info "~~/.swank.lisp done")
