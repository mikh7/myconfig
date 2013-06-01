
(require 'hexrgb)
(require 'icicles)
(require 'imenu+)

(define-key global-map  [?\C-8] nil)
(define-key global-map  [?\C-\M-8] nil)
(define-key global-map  [?\C-9] nil)
(define-key global-map  [?\C-\M-9] nil)
(define-key global-map  [?\C-0] nil)
(define-key global-map  [?\C-\M-0] nil)
(define-key global-map  [?\C-9] 'icicle-switch-to/from-minibuffer)

(defadvice icicle-bind-completion-keys (after my-rebind-icicle-keys activate)
  (define-key map [?\C-8] 'icicle-narrow-candidates)
  (define-key map [?\C-0] 'icicle-widen-candidates))

(setq icicle-cycling-respects-completion-mode-flag t)
(setq icicle-point-position-in-candidate 'input-end)
;; (setq icicle-prefix-complete-keys nil)
;; (setq icicle-apropos-complete-keys '([tab] [(control ?i) ]))
(setq icicle-highlight-lighter-flag nil)
(setq icicle-Completions-text-scale-decrease 0.8)
(setq icicle-default-cycling-mode 'prefix)
(setq icicle-files-ido-like-flag   nil
      icicle-buffers-ido-like-flag t)


(setq icicle-buffer-no-match-regexp "\\.org_archive$"
      confirm-nonexistent-file-or-buffer t)

(defvar first-apropos nil)

;; (defadvice icicle-apropos-complete (around my-auto-next-candidate activate)
;;   (let ((ret ad-do-it)
;;         (was-first-apropos first-apropos))
;;     (setq first-apropos nil)
;;     (setq ad-return-value ret)
;;     (when was-first-apropos
;;       (icicle-next-apropos-candidate))))

(defun icicle-apropos-candidates (input)
  "List of candidate apropos completions for the current partial INPUT.
INPUT is a string.  Each candidate is a string."
  (setq icicle-candidate-nb  nil)
  (when (eq this-command 'icicle-apropos-complete)
    (setq first-apropos t))
  (let ((tmp 
         (let ((cands  (icicle-unsorted-apropos-candidates input)))
           (cond (icicle-abs-file-candidates  (icicle-strip-ignored-files-and-sort cands))
                 (icicle-sort-comparer        (icicle-maybe-sort-maybe-truncate cands))
                 (t                           cands))))
        (prefix-regex (format "^%s" input))
        tmp1 tmp2)
    (save-match-data 
      (loop for candidate in tmp
            if (string-match prefix-regex candidate)
            collect candidate into prefixes
            else collect candidate into apropos
            finally (return (append prefixes apropos))))))

(defun my-icicle-minibuffer-setup-hook ()
  ;; (let ((tab-keys '([(control ?i)] [tab])))
  ;;   (cond ((icicle-file-name-input-p)
  ;;          (dolist (key tab-keys)
  ;;            (define-key minibuffer-local-completion-map key 'icicle-prefix-complete)))
  ;;         (t (dolist (key tab-keys)
  ;;              (define-key minibuffer-local-completion-map key 'icicle-apropos-complete)))))
  )

(add-hook 'icicle-minibuffer-setup-hook 'my-icicle-minibuffer-setup-hook)

;; use customize for this
;; (setq icicle-top-level-key-bindings
;;       (let ((remove-list '(pop-tag-mark icicle-command-abbrev)))
;;         (remove-if (lambda (elem)
;;                      (or
;;                       (member (car elem) remove-list)
;;                       (member (cadr elem) remove-list)))
;;                    icicle-top-level-key-bindings)))

(setq icicle-kmacro-ring-max 10000
      kmacro-ring-max 10000)

(defun mm/destroy-minibuffer-when-switching-windows ()
  "Kill minibuffer if we are not in its window. I hate stuck
minibuffers"
  (when
       (and
        nil
        (active-minibuffer-window)
        (not (eq (selected-window)
                 (active-minibuffer-window))))
     (ignore-errors (abort-recursive-edit))))

;; (defadvice icicle-prefix-complete-no-display (around apropos-when-no-matches activate)
;;   (setq ad-return-value ad-do-it)
;;   ;; (when (null icicle-completion-candidates)
;;   ;;   (setq ad-return-value (icicle-apropos-complete-no-display no-msg-p)))
;;   )

(defun my-quit ()
  (interactive)
  (deactivate-mark)
  (if (fboundp 'kmacro-keyboard-quit)
      (kmacro-keyboard-quit))
  (setq defining-kbd-macro nil)
  (ignore-errors
    (ignore-errors (throw 'exit t))
    (signal 'quit nil)))

(global-set-key "\C-g" 'my-quit)

(add-hook 'post-command-hook 'mm/destroy-minibuffer-when-switching-windows)

(defun mm/apropos-complete-and-exit ()
  (interactive)
  (let ((icicle-current-completion-mode 'apropos))
    (icicle-apropos-complete-and-exit)))

(provide 'my-icicles-setup)
