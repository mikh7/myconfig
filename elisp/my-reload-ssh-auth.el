
(defvar my-reload-ssh-auth-mtime 0)
(defvar my-reload-ssh-auth-file "~/.ssh_auth_sock")

(defun my-reload-ssh-auth-file-mtime (file)
  (destructuring-bind (v1 v2)
      (nth 5 (file-attributes file))
    (+ (* v1 65536) v2)))

(defun my-reload-ssh-auth-extract (file)
  (with-temp-buffer
    (insert-file-contents file)
    (when (re-search-forward "=\\(.*\\)$")
      (match-string 1))))

(defun my-reload-ssh-auth-hook ()
  (nth 5 (file-attributes my-reload-ssh-auth-file))
  (when (file-exists-p my-reload-ssh-auth-file)
    (let ((mtime (my-reload-ssh-auth-file-mtime my-reload-ssh-auth-file)))
      (when (> mtime my-reload-ssh-auth-mtime)
        (let ((value (my-reload-ssh-auth-extract my-reload-ssh-auth-file)))
          (setenv "SSH_AUTH_SOCK" value)
          (setq my-reload-ssh-auth-mtime mtime)
          (message "SSH_AUTH_SOCK set to %s" value))))))

(my-reload-ssh-auth-hook)

(add-hook 'server-visit-hook 'my-reload-ssh-auth-hook)
(setq tramp-shell-prompt-pattern "*\\([[0-9;]*[a-zA-Z-:~] *\\)*")

(provide 'my-reload-ssh-auth)