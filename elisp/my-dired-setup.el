(require 'dired)
(require 'dired-x)
(require 'dired+)

(setq dired-deletion-confirmer
      ;; (lambda (&rest args) t)
      'y-or-n-p
      dired-recursive-deletes 'always
      dired-dwim-target       t
      dired-omit-files        "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\."
      dired-find-subdir       t)

(define-key dired-mode-map "\M-g" nil)
(define-key dired-mode-map "z" nil)
(define-key dired-mode-map "Z" nil)
(define-key dired-mode-map "\M-z" 'diredp-compress-this-file)

(defvar mm/dired-no-omit nil)

(add-hook 'dired-mode-hook
          (lambda ()
            (unless mm/dired-no-omit
              (dired-omit-mode 1))))

(dolist (mode '(dired-mode))
  (remove-from-list 'viper-emacs-state-mode-list mode)
  (add-to-list 'viper-vi-state-mode-list mode))

(define-key dired-mode-map "K" 'dired-do-kill-lines)
;; get rid of the epa decrypt shit
(define-key dired-mode-map ":" nil)
(define-key dired-mode-map "s" nil)

(viper-give-back-keys-in-mode 'dired-mode
                              (remove 
                               [?l] 
                               (remove
                                [?h] 
                                viper-give-back-keys-exception)))

(vimpulse-define-key 'dired-mode 'vi-state ";G" 'diredp-do-grep)
(vimpulse-define-key 'dired-mode 'vi-state ";l" 'dired-do-redisplay)

(defvar dired-sort-map (make-sparse-keymap))
(define-key dired-mode-map "s" dired-sort-map)

(defvar dired-gnu-ls-p
  (equal 0 (call-process-shell-command "ls --version | grep -q GNU"))
  "Non-NIL if we have GNU ls, and thus can be fancy with dired
listing switches")

(defun dired-add-switches (orig &rest switches)
  "Add switches to `orig'. Each switch is either a string or
NIL. The NIL switches are ignored 

Long switches should be specified as --switch and short switches
as without the leading dash.

Example (dired-add-switches \"-abc\" \"12\" \"3\"
                            (and dired-gnu-ls-p \"--whatever\")
                            \"cde\")
will return \"-abc123 --whatever -cde"
  (let ((was-long-switch (string-match ".* --[^ ]+$" orig)))
    (with-output-to-string
      (princ orig)
      (dolist (switch switches)
        (when switch
          (cond
           ((string-match "^--" switch)
            (princ " ")
            (princ switch)
            (setq was-long-switch t))
           (t
            (when was-long-switch
              (princ " -")
              (setq was-long-switch nil))
            (princ switch))))))))

(defvar dired-base-listing-switches)

(setq dired-base-listing-switches "-alFh")
(setq dired-listing-switches
      (dired-add-switches
       dired-base-listing-switches
       "X"
       (and dired-gnu-ls-p "--group-directories-first")))

(defadvice recover-session (around fix-recover-session activate)
  (let ((dired-listing-switches dired-base-listing-switches)
        (dired-omit-files nil)
        (mm/dired-no-omit t))
    (setq ad-return-value ad-do-it)))

(define-key dired-sort-map "s"
  (lambda () "sort by Size" (interactive)
    (dired-sort-other (dired-add-switches
                       dired-base-listing-switches "S"
                       (and dired-gnu-ls-p "--group-directories-first")))))
(define-key dired-sort-map "x"
  (lambda () "sort by eXtension" (interactive)
    (dired-sort-other (dired-add-switches
                       dired-base-listing-switches "X"
                       (and dired-gnu-ls-p "--group-directories-first")))))
(define-key dired-sort-map "t"
  (lambda () "sort by Time" (interactive)
    (dired-sort-other (dired-add-switches
                       dired-base-listing-switches "t"
                       (and dired-gnu-ls-p "--group-directories-first")))))
(define-key dired-sort-map "n"
  (lambda () "sort by Name" (interactive)
    (dired-sort-other (dired-add-switches
                       dired-base-listing-switches
                       (and dired-gnu-ls-p "--group-directories-first")))))
(define-key dired-sort-map "?"
  (lambda () "sort help" (interactive)
    (message "s Size; x eXtension; t Time; n Name")))

;; This differs from (setq dired-find-subdir t), in that it moves
;; the point to the right place, and unhides subdir if it was hidden
(defun dired-open-subdir-noselect (dir)
  "Try to find a dired buffer that shows DIR inline as sub-directory,
if several exist, choose the buffer with outermost parent"
  (let* ((dir (expand-file-name
               (file-name-as-directory dir)))
         parent-name
         found-marker)
    (dolist (buf (dired-buffers-for-dir dir))
      (with-current-buffer buf
        (let* ((parent (expand-file-name
                        (if (consp dired-directory)
                            (car dired-directory)
                          dired-directory)))
               (sub (assoc dir dired-subdir-alist)))
          (when (and sub
                     (not (equal parent dir))
                     (or (not parent-name)
                         (< (length parent (length parent-name)))))
            (setq parent-name parent
                  found-marker (cdr sub))))))
    (when found-marker
      (with-current-buffer (marker-buffer found-marker)
        (goto-char found-marker)
        (let* ((cur-dir (dired-current-directory))
               (hidden-p (dired-subdir-hidden-p cur-dir)))
          (when hidden-p
            (dired-hide-subdir 1))
          (marker-buffer found-marker))))))

(add-to-list 'find-directory-functions 'dired-open-subdir-noselect)

(vimpulse-define-key 'dired-mode 'vi-state "s" dired-sort-map)
(vimpulse-define-key 'dired-mode 'vi-state ",o" 'diredp-omit-marked)
(vimpulse-define-key 'dired-mode 'vi-state ";m" 'dired-mark-files-regexp)
(vimpulse-define-key 'dired-mode 'vi-state ";M"
                     (lambda ()
                       (interactive)
                       (let ((current-prefix-arg '(4)))
                         (call-interactively 'dired-mark-files-regexp))))

(vimpulse-define-key 'dired-mode 'vi-state ";o" 'diredp-omit-marked)
(vimpulse-define-key 'dired-mode 'vi-state ";O" 'diredp-omit-unmarked)
(vimpulse-define-key 'dired-mode 'vi-state "I" 'dired-kill-subdir)

(defun mm/dired-start-grep ()
  "Start grep with default dir being current subdir"
  (interactive)
  (let* ((default-directory (dired-current-directory)))
    (call-interactively 'grep)))

(vimpulse-define-key 'dired-mode 'vi-state "sg" 'mm/dired-start-grep)

(defadvice dired-hide-subdir (around dont-move-point activate)
  (save-excursion
    (setq ad-return-value ad-do-it)))

(viper-apply-major-mode-modifiers)

(provide 'my-dired-setup)


