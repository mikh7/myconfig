;;; my-flyspell-setup.el --- Settings for fly-spell

;; Copyright (C) 2011  Max Mikhanosha

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

;;; Code:


(require 'flyspell)
(require 'ispell)

(defun mm/use-hunspell () 
  (setq ispell-dictionary "american"
        ispell-extra-args '("-a" "-i" "utf-8")
        ispell-silently-savep t
        ispell-dictionary-alist
        '((nil                          ; default
           "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US" "-i"  
                                           "utf-8") nil utf-8)
          ("american"                   ; Yankee English
           "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_US" "-i"  
                                           "utf-8") nil utf-8)
          ("british"                    ; British English
           "[[:alpha:]]" "[^[:alpha:]]" "[']" t ("-d" "en_GB" "-i"  
                                           "utf-8") nil utf-8)
          ("russian"                    ; British English
           "[АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдеёжзийклмнопрстуфхцчшщъыьэюя]"
           "[^АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯабвгдеёжзийклмнопрстуфхцчшщъыьэюя]" "" nil ("-d" "ru_RU" "-i"  
                                                                                           "utf-8") nil utf-8)))
  (setq-default ispell-program-name "hunspell"))

(defadvice ispell-set-spellchecker-params (after use-hunspell activate)
  (mm/use-hunspell))

(eval-after-load "ispell" `(mm/use-hunspell))

(defun my-flyspell-after-major-mode-change-hook ()
  (when (memq major-mode '(text-mode
                           wl-draft-mode org-mode log-edit-mode
                           egg-commit-buffer-mode))
    (flyspell-mode))
  (when (memq major-mode '(lisp-mode emacs-lisp-mode c-mode c++-mode java-mode
                                           python-mode sh-mode))
    (if (get major-mode 'flyspell-mode-predicate)
        (flyspell-mode)
      (flyspell-prog-mode))))

(defun my-flyspell-generic-progmode-verify ()
  "Same as `flyspell-generic-progmode-verify' but handle list of faces"
  (let ((f (get-text-property (point) 'face)))
    (if (atom f) (memq f flyspell-prog-text-faces)
      (let (found) 
        (while (and f (not found))
          (setq found (memq (pop f) flyspell-prog-text-faces)))
        found))))

(defun my-flyspell-lisp-mode-predicate ()
  "Like `flyspell-generic-progmode-verify' but does not
  check all capitalized words"
  (let ((case-fold-search nil))
    (and (my-flyspell-generic-progmode-verify)
         (not (looking-back "[A-Z]+\\('?[a-z]+\\)?")))))

(put 'lisp-mode 'flyspell-mode-predicate 'my-flyspell-lisp-mode-predicate)

(evil-define-key 'normal flyspell-mode-map "\M-z" 'flyspell-auto-correct-previous-word)
(evil-define-key 'normal flyspell-mode-map "\M-z" 'flyspell-auto-correct-previous-word)

(dolist (command '(evil-forward-char
                   evil-backward-char
                   evil-next-line evil-previous-line
                   evil-forward-word evil-forward-Word
                   evil-backward-word evil-backward-Word
                   evil-end-of-word evil-end-of-Word
                   evil-bol-and-skip-white evil-goto-eol
                   evil-goto-char-forward evil-find-char-forward
                   evil-scroll-screen evil-scroll-screen-back))
  (add-to-list 'flyspell-deplacement-commands command))

(add-hook 'after-change-major-mode-hook 'my-flyspell-after-major-mode-change-hook)

(provide 'my-flyspell-setup)
;;; my-flyspell-setup.el ends here

