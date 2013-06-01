;;; my-irc-setup.el --- ERC setup

;; Copyright (C) 2012  Max Mikhanosha

;; Author: Max Mikhanosha <max@momoland.openchat.com>
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

(require 'erc)
(require 'erc-networks)

(setq erc-header-line-format nil)
;; fix viper enter key
(defun ted-viper-erc-hook ()
  "Make RET DTRT when you use Viper and ERC together."
  (viper-add-local-keys 'insert-state
                        `((,(kbd "RET") . erc-send-current-line)))
  (viper-add-local-keys 'vi-state
                        `((,(kbd "RET") . erc-send-current-line))))

(add-hook 'erc-mode-hook 'ted-viper-erc-hook)

(require 'erc-log)
(require 'erc-truncate)

(erc-spelling-mode t)

(defadvice erc-log-setup-logging (after my-erc-fix-buffer-saveable activate)
  (setq buffer-offer-save nil
        buffer-file-name nil))

(defun irc-freenode ()
  (interactive)
  (erc :server "irc.freenode.net" :nick "maxm" :full-name nil :password "up..node"))

(defun irc-undernet ()
  (interactive)
  (erc :server "194.109.20.90" :port 6666 :nick "Propofol" :full-name nil :password "iliketomooit"))

(defun irc-othernet ()
  (interactive)
  (erc :server "irc.othernet.org" :port 6667 :nick "maxm" :full-name nil :password "up..node"))

(defun irc-othernet-bak ()
  (interactive)
  (erc :server "76.73.101.19" :port 6667 :nick "maxm" :full-name nil :password "up..node"))

(defun irc-financialchat ()
  (interactive)
  (erc :server "seattle.wa.us.financialchat.com" :port 6667 :nick "maxm" :full-name nil))

(defun irc-efnet ()
  (interactive)
  (erc "64.161.255.2" 6666 "maxm" nil t nil))


(add-hook 'emacs-startup-hook
          (lambda ()
            (when (equal server-name "irc")
              (message "Doing it")
              (if (assoc 'name default-frame-alist)
                  (setf (cdr (assoc 'name default-frame-alist))
                        "irc")
                (push (cons 'name "irc")
                      default-frame-alist))
              (if (assoc 'name initial-frame-alist)
                  (setf (cdr (assoc 'name initial-frame-alist))
                        "irc")
                (push (cons 'name "irc")
                      initial-frame-alist)))))

(defun my-erc-refill ()
  "Refill the ERC buffer to the window width. Note its a hacky
code that I pounded into shape until it worked. Only handles last
5000 lines of the buffer, to avoid cpu meltdowns"
  (interactive)
  (let ((inhibit-read-only t)
        (timestamps-were-hidden erc-hide-timestamps))
    ;; `erc-fill' fills the whole buffer, no need to set region
    (when erc-fill-function
      (setq erc-fill-column (- (window-width) 2))
      (save-excursion
        (save-restriction
          (beginning-of-line -5000)
          (beginning-of-line)
          (while (looking-at "^[ \t]*$")
            (forward-line 1))
          (while (and (not (eobp))
                      (looking-at "^[ \t]"))
            (forward-line 1))
          (unless (eobp)
            (while (not (eobp))
              ;; find end of line
              (let ((start (point)))
                (loop do (forward-line)
                      until (or (eobp) (not (looking-at "^[ \t]"))))
                (when (> (point) start)
                  (save-excursion
                    (save-restriction
                      (narrow-to-region start (point))
                      (let* ((len (erc-timestamp-offset))
                             (offset len))
                        (goto-char (point-min))
                        (forward-line)
                        (while (and (not (eobp))
                                    (looking-at "^[ \t]*"))
                          (delete-region (match-beginning 0) (match-end 0))
                          (insert " ")
                          (forward-line))
                        (goto-char (point-min))
                        (when (plusp len)
                          (goto-char (min (point-max)
                                          (+ (point) len))))
                        (when (looking-at "\\(\\S-+\\)")
                          (setq offset (1+ (- (match-end 0)
                                              (point-min)))))
                        (goto-char (point-min))
                        (forward-line)
                        (while (and (not (eobp))
                                    (looking-at "^[ \t]*"))
                          (delete-region (match-beginning 0) (match-end 0))
                          (insert (make-string offset 32))
                          (forward-line))
                        (goto-char (point-min))
                        (goto-char (min (point-max)
                                          (+ (point) offset)))
                        (let ((fill-column erc-fill-column)
                              (fill-prefix (make-string offset 32)))
                          (fill-region (point) (point-max) t)))))))))))
      ;; (unless timestamps-were-hidden
      ;;   (erc-show-timestamps))
      )))

(make-variable-buffer-local 'erc-hide-list)
(setq-default erc-hide-list '("JOIN" "PART" "QUIT" "MODE"))

(defun my-erc-toggle-joins (&optional arg)
  "Toggle hiding of JOIN/QUIT/MODE messages"
  (interactive)
  (or (eq major-mode 'erc-mode) (error "This only works in ERC buffers"))
  (if erc-hide-list
      (progn 
        (setq erc-hide-list nil)
        (message "Join/quit/modes will be shown"))
    (setq erc-hide-list '("JOIN" "PART" "QUIT" "MODE"))
    (message "Join/quit/modes will be hidden")))

(define-key erc-mode-map "\C-c\C-h" 'my-erc-toggle-joins)
(define-key erc-mode-map "\C-ch" 'my-erc-toggle-joins)

(provide 'my-irc-setup)
;;; my-irc-setup.el ends here

