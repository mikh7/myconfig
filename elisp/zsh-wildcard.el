(defvar zsh-wildcard-glob-dot nil
  "If true then match files starting with dot by default, otherwise 
require that leading dot be matched explicitly, just like expension in the
shell")

(defvar zsh-wildcard-use-shell 'auto
  "Use actual shell to do the expansion. Possible values are:

NIL  - Do not use shell, use emacs facilities (slow)
AUTO - Use shell if environment variable SHELL ends with zsh
T    - Use shell always")

(defun zsh-wildcard-expand-all-dirs (dir full follow-symlinks)
  "Return a recursive list of directories in dir"
  (cons dir (mapcan
             (lambda (name) 
               ;; make a relative name by joining with parent 
               ;; if not using full names
               (let ((name (if full name
                             (concat (file-name-as-directory dir) name)))
                     (just-name (file-name-nondirectory name)))
                 (when (and (not (string-match "\\`\\.\\.?\\'" just-name))
                            (or zsh-wildcard-glob-dot 
                                (not (string-match "\\`\\..+\\'" just-name)))
                            (file-directory-p name)
                            (or follow-symlinks
                                (not (file-symlink-p name))))
                   (zsh-wildcard-expand-all-dirs 
                    name full follow-symlinks))))
             (directory-files (or dir ".") full nil))))

(defun zsh-wildcard-expand-emacs (pattern &optional full)
  "Expand wildcard pattern PATTERN.
This returns a list of file names which match the pattern.

If PATTERN is written as an absolute file name,
the values are absolute also.

If PATTERN is written as a relative file name, it is interpreted
relative to the current default directory, `default-directory'.
The file names returned are normally also relative to the current
default directory.  However, if FULL is non-nil, they are absolute.

If any directory part of the PATTERN is ** then it matches a
directory and all its subdirectories. 

For example: **/*.c matches all the C files in current directory and
all its subdiroctories, and ~/**/doc/* matches all files in home 
directory whos parent directory is doc
" 
  (save-match-data
    (let* ((nondir (file-name-nondirectory pattern))
	   (dirpart (file-name-directory pattern))
	   ;; A list of all dirs that DIRPART specifies.
	   ;; This can be more than one dir
	   ;; if DIRPART contains wildcards.
	   (dirs (if (and dirpart (string-match "[[*?]" dirpart))
		     (mapcar 'file-name-as-directory
                             (let* ((dir-path (directory-file-name dirpart))
                                    (dir (file-name-nondirectory dir-path)))
                               (cond ((string= "**" dir)
                                      ;; expand recursively from parent
                                      ;; ignoring symlinks
                                      (zsh-wildcard-expand-all-dirs 
                                       (file-name-directory dir-path) full nil))
                                     ((string= "***"  dir)
                                      ;; expand recursively from parent 
                                      ;; following symlinks
                                      (zsh-wildcard-expand-all-dirs 
                                       (file-name-directory dir-path) full t))
                                     (t
                                     ;; just expand this level
                                      (zsh-wildcard-expand-emacs dir-path)))))
		   (list dirpart)))
	   contents)
      (while dirs
	(when (or (null (car dirs))	; Possible if DIRPART is not wild.
		  (file-directory-p (directory-file-name (car dirs))))
	  (let ((this-dir-contents
		 ;; Filter out "." and ".."
		 (delq nil
		       (mapcar #'(lambda (name)
                                   (let ((just-name (file-name-nondirectory name)))
                                     (when 
                                         (and (not (string-match "\\`\\.\\.?\\'" just-name))
                                              (or zsh-wildcard-glob-dot 
                                                  (not (string-match "\\`\\..+\\'" just-name))))
                                       name)))
			       (directory-files (or (car dirs) ".") full
						(wildcard-to-regexp nondir))))))
	    (setq contents
		  (nconc
		   (if (and (car dirs) (not full))
		       (mapcar (function (lambda (name) (concat (car dirs) name)))
			       this-dir-contents)
		     this-dir-contents)
		   contents))))
	(setq dirs (cdr dirs)))
      contents)))


(defun zsh-wildcard-expand-shell (name full)
  (with-temp-buffer 
    (when (zerop (call-process  
                  (getenv "SHELL") nil (current-buffer) t 
                  "-c" (format "%s print -DN %s" 
                               (if zsh-wildcard-glob-dot
                                   "setopt globdots && " "")
                               name)))
      (let ((files (save-match-data
                     (split-string (buffer-substring (point-min) (point-max))
                                   "\0" t))))
        (if (not full) files
          (mapcar #'expand-file-name files))))))

(defun zsh-wildcard-expand (name &optional full)
  (cond ((or (eq zsh-wildcard-use-shell t)
             (and (eq zsh-wildcard-use-shell 'auto)
                  (string-match "zsh\\'" (getenv "SHELL"))))
         (zsh-wildcard-expand-shell name full))
        (t
         (zsh-wildcard-expand-emacs name full))))

(defadvice file-expand-wildcards (around zsh-wildcard-expand-wildcards activate)
  (let ((method (find-file-name-handler pattern 'directory-files)))
    (cond (method (setq ad-return-value ad-do-it))
          (t (setq ad-return-value 
                   (let ((ret (zsh-wildcard-expand pattern full)))
                     (unless ret
                       (error "No matches for %s" pattern))
                     ret))))))

(provide 'zsh-wildcard)
