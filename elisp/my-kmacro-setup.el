;;; my-kmacro-setup.el --- Better keybindings and utilities for Emacs keyboard macros

;; Copyright (C) 2011  Max Mikhanosha

;; Author: Max Mikhanosha <max@momoland.openchat.com>



(require 'kmacro)

;; i like kmacro on K
(define-key viper-vi-basic-map "K" kmacro-keymap)

(defun kmacro-append-macro ()
  (interactive)
  (start-kbd-macro t t))

(define-key viper-vi-basic-map "qv" 'kmacro-view-macro)
(define-key viper-vi-basic-map "qn" 'kmacro-cycle-ring-next)
(define-key viper-vi-basic-map "qN" 'kmacro-cycle-ring-previous)
(define-key viper-vi-basic-map "qp" 'kmacro-cycle-ring-previous)
(define-key viper-vi-basic-map "qd" 'kmacro-delete-ring-head)
(define-key viper-vi-basic-map "qt" 'kmacro-swap-ring)
(define-key viper-vi-basic-map "ql" 'kmacro-call-ring-2nd-repeat)
(define-key viper-vi-basic-map "qe" 'kmacro-end-or-call-macro)
(define-key viper-vi-basic-map "qs" 'kmacro-start-macro-or-insert-counter)
(define-key viper-vi-basic-map "qq" 'kmacro-start-macro-or-insert-counter)
(define-key viper-vi-basic-map "qa" 'kmacro-append-macro)

(defvar mm/macro-undo-list nil)

(defun mm/macro-start-setup ()
  "Setup the undo of entire macro"
  (unless (or executing-kbd-macro defining-kbd-macro)
    (setq mm/macro-undo-list nil)
    (with-current-buffer buffer
      (add-hook 'before-change-functions 'mm/macro-undo-check nil t))
    (add-hook 'kbd-macro-termination-hook 'mm/macro-end-setup)))

(defun mm/macro-end-setup ()
  "Setup the undo for entire macro upon macro finishing"
  (message "Doing macro-end-setup")
  (dolist (buffer (buffer-list))
    (unless (minibufferp buffer)
      (with-current-buffer buffer
        (remove-hook  'before-change-functions 'mm/macro-undo-check t))))
  (remove-hook 'kbd-macro-termination-hook 'mm/macro-undo-check))

(defun mm/macro-undo-check (&optional arg1 arg2)
  "Check and possibyl remember current buffer state in
  `mm/macro-undo-list' variable"
  (if (not (or defining-kbd-macro executing-kbd-macro))
      (ignore-errors
        (mm/macro-end-setup))
    (unless (eq buffer-undo-list t)
      (unless (assoc (current-buffer) mm/macro-undo-list)
        (undo-list-transfer-to-tree)
        (push (cons (current-buffer) (undo-tree-current buffer-undo-tree))
              mm/macro-undo-list)))))

(defun kmacro-undo ()
  "Undo last keyboard macro"
  (interactive)
  (undo-tree-restore-state-from-register ?q))

(defadvice start-kbd-macro (before setup-undo activate)
  (undo-tree-save-state-to-register ?q))

(defadvice execute-kbd-macro (before setup-undo activate))

(defadvice call-last-kbd-macro (around setup-undo activate)
  (setq ad-return-value ad-do-it))

(defadvice kmacro-end-or-call-macro (around setup-undo activate)
  (undo-tree-save-state-to-register ?q)
  (setq ad-return-value ad-do-it))

(define-key viper-vi-basic-map "qu" 'kmacro-undo)

(provide 'my-kmacro-setup)
