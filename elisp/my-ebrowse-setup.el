(require 'ebrowse)

;; (dolist (mode '(ebrowse-tree-mode ebrowse-member-mode))
;;   (remove-from-list 'evil-emacs-state-modes mode)
;;   (add-to-list 'evil-normal-state-modes mode))

(evil-give-back-keys-in-mode 'ebrowse-tree-mode)
(evil-give-back-keys-in-mode 'ebrowse-member-mode)

(evil-define-key 'normal ebrowse-tree-mode-map "c"
                    'ebrowse-read-class-name-and-go)

(evil-define-key 'normal ebrowse-member-mode-map ";"
                    (lookup-key ebrowse-member-mode-map "L"))


(provide 'my-ebrowse-setup)
