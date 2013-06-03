(require 'my-shell-mode-setup)

(dolist (mode '(sql-interactive-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (add-to-list 'evil-insert-state-modes mode))

;; fix the viper key bindings
(evil-define-key 'normal sql-interactive-mode-map 
                    "\C-m" 'viper-comint-enter)
(evil-define-key 'insert sql-interactive-mode-map 
                    "\C-m" 'my-exec-key-in-emacs)
(evil-define-key 'normal sql-interactive-mode-map "j" 'viper-comint-j)
(evil-define-key 'normal sql-interactive-mode-map "k" 'viper-comint-k)
(evil-define-key 'normal sql-interactive-mode-map 
                    "/" 'viper-comint-start-search)
(evil-define-key 'normal sql-interactive-mode-map 
                    "n" 'viper-comint-search-next)
(evil-define-key 'normal sql-interactive-mode-map 
                    "N" 'viper-comint-search-prev)

(provide 'my-sqlmode-setup)
