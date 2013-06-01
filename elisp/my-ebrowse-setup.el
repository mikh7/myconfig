(require 'ebrowse)

(dolist (mode '(ebrowse-tree-mode ebrowse-member-mode))
  (remove-from-list 'viper-emacs-state-mode-list mode)
  (add-to-list 'viper-vi-state-mode-list mode))

(viper-give-back-keys-in-mode 'ebrowse-tree-mode)
(viper-give-back-keys-in-mode 'ebrowse-member-mode)

(vimpulse-define-key 'ebrowse-tree-mode 'vi-state "c"
                    'ebrowse-read-class-name-and-go)

(vimpulse-define-key 'ebrowse-member-mode 'vi-state ";"
                    (lookup-key ebrowse-member-mode-map "L"))


(provide 'my-ebrowse-setup)
