
(require 'sh-script)

(evil-define-key 'normal sh-mode-map "\\" 'self-insert-command)

(defun mgm-after-sh-mode ()
  (paredit-mode 1))


(provide 'my-sh-mode-setup)




 



