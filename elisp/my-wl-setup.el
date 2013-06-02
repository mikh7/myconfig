;;; my-wl-setup.el --- 

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

;;;; wonderlast setup
(require 'wl)
(require 'wl-draft)
(require 'wl-expire)

(setq wl-folder-use-frame nil)
(setq elmo-archive-treat-file t)
(setq elmo-nntp-default-use-listgroup nil)
(setq wl-summary-lazy-update-mark nil)
(setq wl-summary-lazy-highlight nil)

(defun mm/wl-buffer-p (&optional buffer)
  "Predicate to check if its Wanderlust buffer"
  (with-current-buffer (or buffer (current-buffer))
    (or (string-match "^\\*WL" (buffer-name))
        (member major-mode '(wl-summary-mode wl-folder-mode
                                             mime-view-mode)))))
(defun wl-on-frame (&optional frame)
  "Run Wanderlust in its own frame"
  (interactive)
  (unless frame 
    (setq frame
          (make-frame
           (list
            (cons 'unsplittable t)
            (cons 'buffer-list (cons (get-buffer "*scratch*")
                                     (mm/wl-buffer-list)))
            (cons 'buffer-predicate 'mm/wl-buffer-p)))))
  (set-frame-parameter frame 'wanderlust-frame t)
  (set-frame-parameter frame 'name "Wanderlust")
  (with-selected-frame frame
    (let ((wl-auto-check-folder-name '("%INBOX" "finance")))
      (wl))))

(defun mm/wl-buffer-list ()
  (remove-if-not 'mm/wl-buffer-p (buffer-list)))


(defvar mm/bury-wl-recursion-flag nil)

(defun bury-wl ()
  "Bury all wonderlust buffers"
  (interactive)
  (let ((orig-buffer (current-buffer))
        (was-orig-p nil)
        (frames (loop for buffer in (mm/wl-buffer-list)
                      as windows = (get-buffer-window-list buffer nil t)
                      append (mapcar 'window-frame windows))))
    (dolist (frame frames)
      (when (frame-parameter frame 'wanderlust-frame)
        (let ((mm/bury-wl-recursion-flag t))
          (delete-frame frame t))))
    (dolist (buffer (mm/wl-buffer-list))
      (with-current-buffer buffer
        (bury-buffer)))
    ;; (when was-orig-p
    ;;   (switch-to-buffer (get-buffer "*scratch*")))
    ))

(defun mm/wl-frame-delete-hook (frame)
  "Bury Wanderlust buffers if its wanderlust frame"
  ;; (message "here we are %s %s" (frame-parameter frame 'wanderlust-frame)
  ;;          mm/bury-wl-recursion-flag)
  (when (frame-parameter frame 'wanderlust-frame)
    (set-frame-parameter frame 'wanderlust-frame nil)
    (unless mm/bury-wl-recursion-flag
      (bury-wl))))

(add-hook 'delete-frame-functions 'mm/wl-frame-delete-hook)

;; (defadvice mime-display-message (after switch-to-viper activate)
;;   (evil-change-state-to-vi))

(setq mime-situation-examples-file "~/.mime-example")
(setq mime-play-delete-file-immediately nil)

(setq wl-expire-use-log t)

(setq wl-expire-alist
      '(

        ;; ("^\\+trash$"   (date 14) remove)
        ;; delete
        ;; ("^\\+tmp$"     (date 7) trash)
        ;; re-file to `wl-trash-folder'
        ;; ("^\\+outbox$"  (number 300) "$outbox;lha")
        ;; re-file to the specific folder
        ;; ("^\\+ml/tmp$"  nil)
        ;; do not expire
        ;; ("^\\+ml/wl$"   (number 500 510) wl-expire-archive-number1 t)
        ;; archive by message number (retaining numbers)
        ;; ("^\\+ml/.*"    (number 300 310) wl-expire-archive-number2 t)
        ;; archive by a fixed number (retaining numbers)
        ("^%INBOX.Emacs-Org-Mode$"   (date 365) wl-expire-archive-date)
        ;; archive by year and month (numbers discarded)
        ))

(provide 'my-wl-setup)

;;; my-wl-setup.el ends here

