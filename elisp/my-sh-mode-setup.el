
(require 'sh-script)

(vimpulse-define-key 'sh-mode 'insert-state "\\" 'self-insert-command)

(defun mgm-after-sh-mode ()
  (paredit-mode 1))


(provide 'my-sh-mode-setup)




 



