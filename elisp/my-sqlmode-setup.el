(require 'my-shell-mode-setup)

(dolist (mode '(sql-interactive-mode))
  (remove-from-list 'viper-emacs-state-mode-list mode)
  (add-to-list 'viper-insert-state-mode-list mode))

;; fix the viper key bindings
(vimpulse-define-key 'sql-interactive-mode 'vi-state 
                    "\C-m" 'viper-comint-enter)
(vimpulse-define-key 'sql-interactive-mode 'insert-state 
                    "\C-m" 'viper-exec-key-in-emacs)
(vimpulse-define-key 'sql-interactive-mode 'vi-state "j" 'viper-comint-j)
(vimpulse-define-key 'sql-interactive-mode 'vi-state "k" 'viper-comint-k)
(vimpulse-define-key 'sql-interactive-mode 'vi-state 
                    "/" 'viper-comint-start-search)
(vimpulse-define-key 'sql-interactive-mode 'vi-state 
                    "n" 'viper-comint-search-next)
(vimpulse-define-key 'sql-interactive-mode 'vi-state 
                    "N" 'viper-comint-search-prev)

(provide 'my-sqlmode-setup)
