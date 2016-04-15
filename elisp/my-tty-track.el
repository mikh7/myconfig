

(when (not (fboundp 'tty-type))
  (defun tty-type (&optional terminal)
    (let ((term (mygetenv "TERM" (selected-frame))))
      (cond ((or (null term) (string= term ""))
             "none")
            (t term)))))

(defvar mtt-last-title "")
(make-variable-frame-local 'mtt-last-title)

(defun mtt-set-last-title (title)
  (if (< emacs-major-version 23)
      (setq mtt-last-title title)
    (set-frame-parameter (selected-frame) 'mtt-last-title title)))

(defun mtt-set-tty-title (title)
  (when (not (string= mtt-last-title title))
    (mtt-set-last-title title)
    (cond ((string-match "^xterm" (tty-type))
           (send-string-to-terminal 
            (format "\e]0;%s %s - Emacs\a" 
                    (mygetenv "PR_HOST" (selected-frame)) title)))
          ((string-match "^screen" (tty-type))
           (send-string-to-terminal 
            (format "\ek%s %s - Emacs\e\\" 
                    (mygetenv "PR_HOST" (selected-frame)) title))))))

(defun mtt-track-title ()
  (ignore-errors
    (mtt-set-tty-title (buffer-name (current-buffer)))))

(add-hook 'post-command-hook 'mtt-track-title)

(provide 'my-tty-track)
