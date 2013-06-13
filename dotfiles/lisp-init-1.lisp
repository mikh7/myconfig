(in-package :cl-user) 

(load (make-pathname
       :directory
       (append (pathname-directory (user-homedir-pathname)) 
               '("quicklisp"))
       :name "setup"
       :type "lisp"))

(defvar home-dir (user-homedir-pathname))
(defvar myconfig-dir (merge-pathnames (pathname "myconfig/") home-dir))
(defvar lisp-systems-dir (merge-pathnames (pathname "lisp-systems/") home-dir))

(defvar *initial-random-state* (make-random-state *random-state*))
(defun randomize ()
  (setf *random-state* (make-random-state *initial-random-state*)))
(setf *print-length* 100)

(asdf:initialize-source-registry
 `(:source-registry
   :inherit-configuration
   (:tree "~/lisp-systems")
   (:directory "~/cvs/slime")))

(asdf:load-system :asdf-policy-locations)

(dolist (elem '("/usr/src/local/stumpwm"
                "~/projects/lisp"
                "~/tt-hg/sbcl"
                ;; "~/tt-stephilize/src/sbcl"
                ))
  (pushnew elem ql:*local-project-directories* :test 'equal))

(ql:quickload :alexandria)
(ql:quickload :iterate)
(when (boundp iterate::*always-declare-variables*)
  (setf iterate::*always-declare-variables* t))
(ql:quickload :demacs)
(ql:quickload :log4cl)
(ql:quickload :anaphora)
(ql:quickload :cl-maxlib)
(ql:quickload :quickproject)
(ql:quickload :uiop)
(ql:quickload :cl-ppcre)
(ql:quickload :usocket)
(ql:quickload :flexi-streams)
(ql:quickload :babel)
(ql:quickload :babel-streams)
(ql:quickload :ironclad)
(ql:quickload :trivial-garbage)
(ql:quickload :trivial-backtrace)
(ql:quickload :named-readtables)
(ql:quickload :stefil)
(ql:quickload :fiveam)
(ql:quickload :arnesi)

(setf quickproject:*author* "Max Mikhanosha <max@openchat.com>"
      quickproject:*license* "Apache License, Version 2.0")

(defun mm/run (command &optional noerrorp)
  (uiop:run-program command :ignore-error-status noerrorp))

(defun mm/after-quickproject-hook (pathname &key
                                            depends-on
                                            name
                                            &allow-other-keys)
  (declare (ignore depends-on name))
  (let ((readme.txt (make-pathname :name "README" :type "txt" :defaults pathname))
        (readme.md (make-pathname :name "README" :type "md" :defaults pathname))) 
    (log:debug readme.txt readme.md)
    (mm/run (format nil "mv ~S ~S" (princ-to-string readme.txt)
                    (princ-to-string readme.md))) 
    (mm/run "git init")))

(defun :new-project (name &rest depends-on)
  (let* ((name (string-downcase (string name)))
         (path 
           (make-pathname
            :directory
            (append (pathname-directory (user-homedir-pathname)) 
                    `("projects" "lisp" ,name))))
         (depends-on (remove-duplicates
                      `(#:log4cl #:alexandria #:iterate #:bordeaux-threads ,@depends-on)
                      :key 'string :test 'string=
                      :from-end t)))
    (flet ((make-param (x)
             (string-downcase (prin1-to-string
                               (make-symbol 
                                (etypecase x
                                  (string (symbol-name (read-from-string x)))
                                  (symbol (symbol-name x)))))))) 
      (quickproject:make-project
       path :name name
       :template-directory (make-pathname
                            :directory
                            (append (pathname-directory (user-homedir-pathname)) 
                                    `("myconfig" "templates" "lisp-project")))
       :template-parameters
       `(:package-name ,(make-param name)
         :depends-on ,(format nil "~{ ~A~}"
                              (mapcar #'make-param
                                      (remove '#:log4cl depends-on
                                              :key 'string :test 'string=))))
       :depends-on depends-on))))

(pushnew 'mm/after-quickproject-hook quickproject:*after-make-project-hooks*)

(cl:in-package :asdf)
(defun* show-dependencies (system-name &key descriptions?)
  (let ((seen (make-hash-table :test #'equal)))
    (labels ((rec (system-name depth)
               (let ((nm (if (and (listp system-name) (eq (car system-name) ':version))
                             (coerce-name (cadr system-name))
                             (coerce-name system-name))))
                 (if (gethash nm seen)
                     (format t "~v@T[~A]~%" (* depth 2) nm)
                     (progn
                       (let ((sys (cdr (gethash nm *defined-systems*))))
                         (if (null sys)
                             (format t "~v@T~A??~%" (* depth 2) nm)
                             (progn
                               (format t "~v@T~A~@[: ~A~]~%" (* depth 2) nm
                                       (and descriptions?
                                            (slot-boundp sys 'description)
                                            (slot-value sys 'description)))
                               (setf (gethash nm seen) t)
                               (dolist (dep (component-load-dependencies sys))
                                 (rec dep (1+ depth)))))))))))
      (rec system-name 0))))
(cl:in-package :cl-user)

#+sbclno (in-package :sb-impl)
#+sbclno (sb-ext:without-package-locks 
         (defun decode-universal-time (universal-time &optional time-zone)
           "Converts a universal-time to decoded time format returning the following
   nine values: second, minute, hour, date, month, year, day of week (0 =
   Monday), T (daylight savings time) or NIL (standard time), and timezone.
   Completely ignores daylight-savings-time when time-zone is supplied."
           (multiple-value-bind (daylight seconds-west)
               (if time-zone
                   (values nil (* time-zone 60 60))
                   (multiple-value-bind (ignore seconds-west daylight)
                       (let ((tmp (truncate-to-unix-range universal-time))) 
                         (sb-thread::block-deferrable-signals)
                         (multiple-value-prog1 
                             (sb-unix::get-timezone tmp)
                           (sb-unix::unblock-deferrable-signals)))
                     (declare (ignore ignore))
                     (declare (fixnum seconds-west))
                     (values daylight seconds-west)))
             (declare (fixnum seconds-west))
             (multiple-value-bind (weeks secs)
                 (truncate (+ (- universal-time seconds-west) seconds-offset)
                           seconds-in-week)
               (let ((weeks (+ weeks weeks-offset)))
                 (multiple-value-bind (t1 second)
                     (truncate secs 60)
                   (let ((tday (truncate t1 minutes-per-day)))
                     (multiple-value-bind (hour minute)
                         (truncate (- t1 (* tday minutes-per-day)) 60)
                       (let* ((t2 (1- (* (+ (* weeks 7) tday november-17-1858) 4)))
                              (tcent (truncate t2 quarter-days-per-century)))
                         (setq t2 (mod t2 quarter-days-per-century))
                         (setq t2 (+ (- t2 (mod t2 4)) 3))
                         (let* ((year (+ (* tcent 100)
                                         (truncate t2 quarter-days-per-year)))
                                (days-since-mar0
                                  (1+ (truncate (mod t2 quarter-days-per-year) 4)))
                                (day (mod (+ tday weekday-november-17-1858) 7))
                                (t3 (+ (* days-since-mar0 5) 456)))
                           (cond ((>= t3 1989)
                                  (setq t3 (- t3 1836))
                                  (setq year (1+ year))))
                           (multiple-value-bind (month t3)
                               (truncate t3 153)
                             (let ((date (1+ (truncate t3 5))))
                               (values second minute hour date month year day
                                       daylight
                                       (if daylight
                                           (1+ (/ seconds-west 60 60))
                                           (/ seconds-west 60 60)))))))))))))))
