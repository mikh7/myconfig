;;; my-ess-setup.el --- my-ess-setup

;; Copyright (C) 2016  max

;; Author: max <max@backtest1.chi1.veliosystems.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(message "My ESS setup loaded")

(load-library "ess-r-d")
(message "Result of loading ess-r-completion is %s"
         (load-library "ess-r-completion"))


;; fix the viper key bindings
(evil-define-key 'normal inferior-ess-mode-map "\C-m" 'viper-comint-enter)
(evil-define-key 'insert inferior-ess-mode-map "\C-m" 'my-exec-key-in-emacs)
(evil-define-key 'normal inferior-ess-mode-map "j" 'viper-comint-j)
(evil-define-key 'normal inferior-ess-mode-map "k" 'viper-comint-k)
(evil-define-key 'normal inferior-ess-mode-map "/" 'viper-comint-start-search)
(evil-define-key 'insert inferior-ess-mode-map "\M-/" 'viper-comint-start-search)
(evil-define-key 'normal inferior-ess-mode-map "\M-/" 'viper-comint-start-search)
(evil-define-key 'normal inferior-ess-mode-map "n" 'viper-comint-search-next)
(evil-define-key 'normal inferior-ess-mode-map "N" 'viper-comint-search-prev)

(evil-define-key 'insert inferior-ess-mode-map "\M--" 'ess-smart-S-assign)
(evil-define-key 'insert ess-mode-map "\M--" 'ess-smart-S-assign)


(defun my-ess-hook ()
  (setq comint-use-prompt-regexp nil)
  (setq inhibit-field-text-motion nil))


(defun mgm-after-inferior-ess-mode ()
  (setq comint-input-ring-file-name "~/.myrhistory")
  (comint-read-input-ring)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'shell-write-history-on-exit)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t)
  (ess--unset-smart-S-assign-key))

(defun mgm-after-ess-mode ()
  (ess--unset-smart-S-assign-key))

(defun mgm-after-ess-help-mode ()
  (evil-motion-state))

(defun my-ess-closing-return (&optional no-newline artificial)
  (interactive)
  ;; Note that the input string does not include its terminal newline.
  (save-restriction 
    (goto-char (point-max))
    (let ((proc (get-buffer-process (current-buffer))))
      (if (not proc) (user-error "Current buffer has no process")
        (widen)
        (let* ((pmark (process-mark proc)))
          (narrow-to-region pmark (point))
          (while (ignore-errors (save-excursion (backward-up-list 1)) t)
            (insert ")"))))))
  (inferior-ess-send-input))

(evil-define-key 'normal inferior-ess-mode-map (kbd "M-RET") 'my-ess-closing-return)
(evil-define-key 'insert inferior-ess-mode-map (kbd "M-RET") 'my-ess-closing-return)

(add-hook 'inferior-ess-mode-hook 'my-ess-hook)

(evil-give-back-keys-in-mode '(ess-help-mode)
                             `([?w] [?y] [?g] [?s] [?z] [?%] ,@evil-give-back-keys-exception))


(provide 'my-ess-setup)


;;; my-ess-setup.el ends here
