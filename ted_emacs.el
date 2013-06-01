;;; .emacs --- Edward O'Connor's Emacs configuration -*- emacs-lisp -*-

;; Copyright (C) 1997 -- 2006 Edward O'Connor <ted@oconnor.cx>

;; Author: Edward O'Connor <ted@oconnor.cx>
;; Keywords: local

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Emacs; see the file COPYING, or type `C-h C-c'. If not,
;; write to the Free Software Foundation at this address:

;;   Free Software Foundation
;;   51 Franklin Street, Fifth Floor
;;   Boston, MA 02110-1301
;;   USA

;;; Code:

(require 'cl)

(unless (featurep 'xemacs)
  (provide 'emacs))

;;; Compatibility. Checking for the availability of various functions
;;; which I'll be using later on.

;; Not defined in Emacs.
(if (fboundp 'variable-obsolete-p)
    (defalias 'ted-variable-obsolete-p 'variable-obsolete-p)
  (defsubst ted-variable-obsolete-p (variable)
    "Non-nil if VARIABLE is marked as obsolete."
    (get variable 'byte-obsolete-variable)))

;; Not defined in XEmacs.
(unless (fboundp 'turn-off-auto-fill)
  (defun turn-off-auto-fill ()
    "Unconditionally turn off Auto Fill mode."
    (interactive)
    (auto-fill-mode -1)))

;; Not defined in XEmacs.
(if (fboundp 'propertize)
    (defalias 'ted-propertize 'propertize)
  (defun ted-propertize (string &rest props)
    "Propertize STRING with PROPS.
  Stolen from `erc-propertize'."
    (let ((length (length string)))
      (while props
        (put-text-property 0 length (car props) (cadr props) string)
        (setq props (cddr props))))
    string))

;; Not defined in non-Mule XEmacs.
(if (fboundp 'coding-system-p)
    (defalias 'ted-coding-system-p 'coding-system-p)
  (defalias 'ted-coding-system-p 'ignore))

;; Not defined in older Emacsen.
(unless (fboundp 'custom-autoload)
  (defun custom-autoload (symbol load)
    "Mark SYMBOL as autoloaded custom variable and add dependency LOAD."
    (put symbol 'custom-autoload t)
    (custom-add-load symbol load)))

;; The function `exectuable-find' isn't autoloaded in older XEmacsen.
(unless (fboundp 'executable-find)
  (autoload 'executable-find "executable"))

(cond ((fboundp 'set-frame-parameter)
       (defalias 'ted-set-frame-parameter 'set-frame-parameter))
      ((fboundp 'set-frame-property)
       (defalias 'ted-set-frame-parameter 'set-frame-property)))

;; Not defined in XEmacs.
(if (fboundp 'set-frame-font)
    (defalias 'ted-set-frame-font 'set-frame-font)
    (defun ted-set-frame-font (font &rest ignored)
      "Try to set this frame's font to FONT."
      (ted-set-frame-parameter (selected-frame) 'font font)))

;; evolved from code in mac-win.el
(defun ted-mac-get-fontset-from-font (font)
  "Try to find or make a fontset based on FONT."
  (when font
    (let* ((xlfd (x-decompose-font-name font))
           (resolved (x-resolve-font-name font))
           (resolved-xlfd (x-decompose-font-name resolved))
           (fontset (query-fontset font)))
      (cond (fontset fontset)
            ((string= "fontset" (aref xlfd xlfd-regexp-registry-subnum))
             (new-fontset font (x-complement-fontset-spec xlfd nil)))
            ((and (string= "mac" (aref resolved-xlfd
                                       xlfd-regexp-registry-subnum))
                   (string= "roman" (aref resolved-xlfd
                                          xlfd-regexp-encoding-subnum)))
             (create-fontset-from-mac-roman-font font))
            (t (create-fontset-from-ascii-font font))))))

(defun ted-set-font (font)
  "On Darwin, create a fontset from FONT and install it.
On other systems, pass through args to `set-frame-font'."
  (when (fboundp 'create-fontset-from-mac-roman-font)
    (setq font (ted-mac-get-fontset-from-font font)))
  (ted-set-frame-font font))

;; Not defined in Emacs.
(cond ((fboundp 'list-fonts)
       (defalias 'ted-list-fonts 'list-fonts))
      ((fboundp 'x-list-fonts)
       (defalias 'ted-list-fonts 'x-list-fonts))
      (t (defalias 'ted-list-fonts 'ignore)))

;; Defined in multi-tty Emacs
(if (fboundp 'window-system)
    (defalias 'ted-window-system 'window-system)
  (defun ted-window-system (&optional frame)
    window-system))

(cond ((fboundp 'frame-display) ;; Multi-TTY Emacs
       (defalias 'ted-frame-display 'frame-display))
      ((fboundp 'frame-device) ;; XEmacs
       (defalias 'ted-frame-display 'frame-device))
      (t (defalias 'ted-frame-display 'ignore)))

;; I use `assoc-default' in `ted-find-mode'.
(unless (fboundp 'assoc-default)
  (warn "This Emacs does not define `assoc-default'!")
  (defun assoc-default (key alist &optional test default)
    "Find object KEY in ALIST."
    (let ((thing (assoc* key alist :test (lambda (a b) (funcall test b a)))))
      (if (consp thing)
          (cdr thing)
        default))))

;;; Utilities.

(defsubst ted-alist (list)
  "Given LIST of the form (A B .. Z), create an alist of the form
\((A . A) (B . B) .. (Z . Z)). If LIST is nil, return nil. Useful
for making arguments for `completing-read'."
  (mapcar (lambda (item) (cons item item)) list))

(defun ted-add-to-list* (list-var &rest directories)
  "Add to the value of LIST-VAR each existing directory in DIRECTORIES.
Effectively a multi-argument version of `add-to-list', but custom-made
for variables like `load-path' and `Info-default-directory-list'."
  (mapc (lambda (directory)
          (when (file-directory-p directory)
            (add-to-list list-var directory)))
        directories))
(put 'ted-add-to-list* 'lisp-indent-function 1)

;;; Frob the Emacs command line.

(defvar ted-server-emacs t
  "If non-null, this emacs should run an edit server.
By edit server, I mean the bit that emacsclient or gnuclient talk to.")

(add-to-list 'command-switch-alist
             '("gnus" . (lambda (&rest ignore)
                          (setq ted-server-emacs nil)
                          (add-hook 'emacs-startup-hook 'gnus t))))

(add-to-list 'command-switch-alist
             '("erc" . (lambda (&rest ignore)
                          (setq ted-server-emacs nil)
                          (add-hook 'emacs-startup-hook
                                    (lambda ()
                                      (put 'gnus 'disabled t)
                                      (call-interactively 'erc-select))
                                    t))))


;;; Frobbing `load-path' and checking for any third-party elisp files on
;;; this machine.

(defconst ted-elisp-dir "~/elisp"
  "Where I keep local copies of elisp files, both my own and others'.")

(defconst ted-local-elisp-config
  (expand-file-name "local.el" ted-elisp-dir)
  "Local elisp configuration file for overriding stuff in ~/.emacs.")

(defconst ted-workspace "~/code"
  "Where I check out CVS modules (and other version-controlled things).
So this is where CVS versions of things like ERC and Gnus live.")

(ted-add-to-list* 'load-path
  (expand-file-name ted-elisp-dir)
  (expand-file-name "elisp" ted-workspace)
  (expand-file-name "ljupdate" ted-workspace)
  (expand-file-name "slime" ted-workspace))

;; Ducking the incompatible byte-code issue.
(unless (featurep 'xemacs)
  (ted-add-to-list* 'load-path
    (expand-file-name "auctex" ted-workspace)
    (expand-file-name "bbdb/lisp" ted-workspace)
    (expand-file-name "emacs-w3m" ted-workspace)
    (expand-file-name "erc" ted-workspace)
    (expand-file-name "gnus/lisp" ted-workspace)
    (expand-file-name "nxml" ted-workspace)
    (expand-file-name "url/lisp" ted-workspace)))

(ted-add-to-list* 'Info-default-directory-list
  (expand-file-name "auctex/doc" ted-workspace)
  (expand-file-name "bbdb/texinfo" ted-workspace)
  (expand-file-name "emacs-w3m/doc" ted-workspace)
  (expand-file-name "gnus/texi" ted-workspace)
  (expand-file-name "url/texi" ted-workspace))

(let ((default-directory ted-elisp-dir))
  (load "./subdirs.el" t))

;; Let's make sure we load the right version of Gnus.
(ignore-errors (require 'gnus-load))

;;; Define various constants and predicates that will be used throughout
;;; this .emacs file to conditionalize code. For instance, I define
;;; `ted-tty-p' so that I can specify particular Emacs configuration
;;; bits for TTYs only.

(defvar ted-oort+-flag nil
  "Non-nil means this Emacs is equipped with Oort Gnus (or later).")

(when (or (featurep 'xemacs) (not (fboundp 'display-graphic-p)))
  (defun display-graphic-p (&optional display)
    "Is DISPLAY is a graphical display?"
    (ted-window-system display)))

;; Not defined in XEmacs.
(unless (fboundp 'display-color-p)
  (defun display-color-p (&optional display)
    "Does this display support colors?"
    (or (featurep 'xemacs) (display-graphic-p display))))

(if (fboundp 'console-type)
    (defun ted-tty-p (&rest ignore)
      "Is this a TTY that we're on?"
      (eq (console-type) 'tty))
  (defun ted-tty-p (&optional display)
    "Is this a TTY that we're on?"
    (not (display-graphic-p display))))

(defun ted-xterm-p (&optional display)
  "Non-nil if DISPLAY is an xterm.
If DISPLAY is nil, the current displaly is used."
  (and (ted-tty-p display)
       (let ((TERM (getenv "TERM")))
         (if TERM
             (string-match "^xterm.*\\'" TERM)
           nil))))

(defun ted-w32-window-system-p (&optional frame)
  "Non-nil if FRAME is on a Microsoft Windows display.
If FRAME is nil, the current frame is used."
  (memq (ted-window-system frame) '(w32 win32)))

(defun ted-aqua-p (&optional frame)
  "Non-nil if FRAME is on a Mac OS X display.
If FRAME is nil, the current frame is used."
  (memq (ted-window-system frame) '(mac ns)))

(defun ted-menu-bar-lines (&optional frame)
  "Returns the number of menu bar lines to be used on FRAME."
  (if (ted-aqua-p frame) 1 0))

(defconst ted-w32-flag (eq system-type 'windows-nt)
  "Are we running under Microsoft Windows?")

(defconst ted-mac-flag (eq system-type 'darwin)
  "Are we running on a Macintosh?")

(defconst ted-emacs-name
  (let ((version-int (number-to-string emacs-major-version)))
    (cond ((featurep 'xemacs) (concat "XEmacs-" version-int "."
                                   (number-to-string emacs-minor-version)))
          (t  (concat "Emacs-" version-int))))
  "The name of this Emacs.")

;;; Work-arounds for things I find annoying in their default state, and
;;; basic customizations.

(when (and (eq system-type 'darwin)
           (featurep 'emacs)
           (require 'osx-plist nil t))
  (osx-plist-update-environment))

(add-hook 'emacs-startup-hook
          (lambda ()
            (run-hook-with-args 'after-make-frame-functions
                                (selected-frame))))

;; Make sure that XEmacs doesn't try to move this file somewhere else.
(setq load-home-init-file t)

(setq inhibit-startup-message t)

(setq cd-path '("./" "~/" "~/code/"))
(setenv "CDPATH" (mapconcat 'identity cd-path ":"))
(when ted-w32-flag
  (defun ted-fix-w32-path (path)
    (mapconcat (lambda (dir)
                 (if (string-match "^[\"]\\(.*\\)[\"]/\\'" dir)
                     (match-string 1 dir)
                   dir))
               (parse-colon-path path)
               ";"))

  (setenv "PATH" (ted-fix-w32-path (getenv "PATH"))))
(global-set-key (kbd "C-c l") 'goto-line)

(setq ring-bell-function 'ignore
      visible-bell       nil)

(setq require-final-newline t)

(setq change-log-default-name "ChangeLog")

(when (featurep 'aquamacs)
  (cua-mode 0))
(setq zmacs-regions t)

(when (fboundp 'transient-mark-mode)
  (transient-mark-mode 1)
  (setq highlight-nonselected-windows nil
        mark-even-if-inactive         t))

(mapc (lambda (sym)
        (put sym 'disabled nil))
      '(downcase-region erase-buffer eval-expression narrow-to-page
        narrow-to-region upcase-region))

(setq minibuffer-max-depth         nil ; XEmacs
      enable-recursive-minibuffers t)  ; Emacs

(put 'overwrite-mode 'disabled t)

(setq sentence-end-double-space nil
      sentence-end "[.?!][]\"')]*\\(\\'\\|\t\\| \\)[ \t\n]*")

(defalias 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq truncate-partial-width-windows nil)

(setq-default truncate-lines t)
(let ((foo (lambda () (setq truncate-lines nil))))
  (mapc (lambda (hook)
          (add-hook hook foo))
        '(term-mode-hook eshell-mode-hook html-mode-hook)))

(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(setq visible-cursor nil) ; Emacs 22
(setq-default scroll-step              1
              scroll-conservatively    most-positive-fixnum
              scroll-up-aggressively   0.0
              scroll-down-aggressively 0.0)

(when (require 'multi-region nil t)
  (global-set-key (kbd "C-c 2") multi-region-map))

(when (fboundp 'minibuffer-electric-default-mode)
  (minibuffer-electric-default-mode 1))

(when (fboundp 'temp-buffer-resize-mode)
  (temp-buffer-resize-mode 1))

(cond ((not (ted-variable-obsolete-p 'resize-minibuffer-window-exactly))
       (setq resize-minibuffer-window-exactly t)
       (resize-minibuffer-mode 1))
      (t
       (setq max-mini-window-height 0.30)
       (setq resize-mini-window t)))

(when (require 'uniquify nil t)
  (setq-default uniquify-buffer-name-style 'forward))

(line-number-mode 1)
(when (fboundp 'column-number-mode)
  (column-number-mode 1))

(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) " A")
(setcar (cdr (assq 'auto-fill-function minor-mode-alist)) " F")
(let ((el-hook (lambda () (setq mode-name "el"))))
  (add-hook 'emacs-lisp-mode-hook el-hook)
  (add-hook 'lisp-interaction-mode-hook el-hook))
(add-hook 'sh-mode-hook (lambda () (setq mode-name "sh")))


;; completion

(define-key minibuffer-local-completion-map (kbd "SPC") nil)

(when (locate-library "pcomplete")
  (setq pcomplete-cycle-completions nil))

(setq search-highlight t)
(setq isearch-highlight-all-matches t) ; XEmacs
(setq-default case-fold-search t)
(eval-after-load "isearch"
  '(define-key isearch-mode-map (kbd "C-h") 'isearch-mode-help))

(setq-default abbrev-mode t)
(when (file-exists-p abbrev-file-name)
  (quietly-read-abbrev-file))

(add-hook 'mail-setup-hook 'mail-abbrevs-setup)

(when (fboundp 'show-paren-mode)
  (show-paren-mode 1)
  (make-variable-buffer-local 'show-paren-mode))

;; to make paredit behave.
(setq blink-matching-paren-on-screen nil)
(setq blink-matching-delay 0.125)

(setq-default auto-fill-function 'do-auto-fill)

(mapc (lambda (mode-hook)
         (add-hook mode-hook 'turn-off-auto-fill))
      '(emacs-lisp-mode-hook sh-mode-hook comint-mode-hook
        shell-mode-hook lisp-mode-hook erc-mode-hook ruby-mode-hook))

(setq-default fill-column 72)
(setq emacs-lisp-docstring-fill-column 72)

(if (boundp 'show-trailing-whitespace)
    (progn
      (setq-default show-trailing-whitespace t)

      (defun ted-hide-trailing-whitespace ()
        "Do not highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace nil))

      (defun ted-show-trailing-whitespace ()
        "Highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace t))

      (defun ted-toggle-show-trailing-whitespace ()
        "Highlight trailing whitespace in this buffer."
        (interactive)
        (setq show-trailing-whitespace (not show-trailing-whitespace)))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook
                        'ted-hide-trailing-whitespace))
            '(Buffer-menu-mode-hook custom-mode-hook text-mode-hook
              term-mode-hook Info-mode-hook comint-mode-hook
              buffer-menu-mode-hook apropos-mode-hook
              tooltip-show-hook gnus-article-mode-hook mail-mode-hook
              gnus-summary-mode-hook message-mode-hook
              gnus-group-mode-hook eshell-mode-hook w3-mode-hook
              initial-calendar-window-hook))

      (mapc (lambda (mode-hook)
              (add-hook mode-hook
                        (lambda ()
                          (setq show-trailing-whitespace t))))
            '(latex-mode-hook LaTeX-mode-hook html-mode-hook)))
  (defalias 'ted-hide-trailing-whitespace 'ignore))

(setq user-mail-address "hober0@gmail.com" ;%
      user-full-name    "Edward O'Connor")
(setq message-log-max most-positive-fixnum)

(defun ted-clear (&optional prefix)
  "Move the line containing point to the top of the window.
With PREFIX, move the line containing point to line PREFIX of the window."
  (interactive "P")
  (recenter (or prefix 0)))
(global-set-key (kbd "C-c c") 'ted-clear)

(add-hook 'write-file-hooks 'time-stamp)

(when (fboundp 'goto-address)
  (setq goto-address-fontify-maximum-size most-positive-fixnum)
  (add-hook 'find-file-hooks 'goto-address))

(setq-default indicate-empty-lines       t
              indicate-buffer-boundaries t)

(let ((hook (lambda ()
              (setq indicate-empty-lines       nil
                    indicate-buffer-boundaries nil)))
      (mode-hooks '(shell-mode-hook term-mode-hook gnus-article-mode-hook
                    gnus-summary-mode-hook gnus-group-mode-hook
                    eshell-mode-hook)))
  (mapc (lambda (mode-hook)
          (add-hook mode-hook hook))
        mode-hooks))

(when (fboundp 'auto-insert-mode)
  (auto-insert-mode 1)
  (setq auto-insert-query nil)
  (setq auto-insert-directory "~/templates/")
  (let ((latex-template "template.tex"))
    (when (file-exists-p (expand-file-name latex-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.tex\\'" . "LaTeX") . ,latex-template))))


  (let ((css-template "template.css"))
    (when (file-exists-p (expand-file-name css-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.css\\'" . "CSS") . ,css-template))))

  (let ((html-template "template.html"))
    (when (file-exists-p (expand-file-name html-template
                                           auto-insert-directory))
      (add-to-list 'auto-insert-alist
                   `(("\\.html\\'" . "HTML") . ,html-template)))))

(defun kr-major-mode-p (symbol)
  "Return non-nil if SYMBOL is a major mode.
Used in `interactive' forms to read major mode names from the user."
  (and (fboundp symbol)
       (let ((function-name (symbol-name symbol)))
         (and (string-match "-mode\\'" function-name)
              (not (string-match "\\`turn-\\(on\\|off\\)-"
                                 function-name))))
       (not (assq symbol minor-mode-alist))))

(defun ted-read-major-mode ()
  "Read a major mode from the user, and return it.
Based on Kevin Rogers' `edit-region' interactive spec."
  (intern (completing-read
           (format "Major mode (default `%s'): " major-mode)
           obarray 'kr-major-mode-p t nil nil
           (symbol-name major-mode))))

(defun kr-edit-region (&optional edit-mode)
  "Edit the current region in a separate buffer.
With a prefix arg, change `major-mode' to EDIT-MODE."
  (interactive (list (when current-prefix-arg (ted-read-major-mode))))
  (clone-indirect-buffer nil t)
  (narrow-to-region (region-beginning) (region-end))
  (shrink-window-if-larger-than-buffer)
  (when edit-mode (funcall edit-mode)))

(defun ted-kill-mode-buffers (&optional mode)
  "Kill all buffers of this major mode.
With optional argument MODE, all buffers in major mode MODE are killed
instead."
  (interactive (list (when current-prefix-arg (ted-read-major-mode))))
  (setq mode (or mode major-mode))
  (when (or current-prefix-arg
            (y-or-n-p (format "Really kill all %s buffers? " mode)))
    (mapc (lambda (buffer)
            (when (with-current-buffer buffer
                    (eq major-mode mode))
              (kill-buffer buffer)))
          (buffer-list))))

(defun help-default-arg-highlight (arg)
  "Upcase and fontify ARG for use with `eldoc-mode' and help."
  (ted-propertize (upcase arg)
              'face 'font-lock-variable-name-face))
(defun ted-find-mode (extension &optional interactive)
  "Returns the mode in which a file with EXTENSION would be opened."
  (interactive "sExtension: \np")
  (let ((mode (assoc-default (concat "." extension) auto-mode-alist
                             'string-match default-major-mode)))
    (when interactive
      (message "A file with extension .%s would be opened with mode %s"
               extension mode))
    mode))

;; Customizations particular to XEmacs.
(when (featurep 'xemacs)
  (when (fboundp 'make-indirect-buffer)
    (make-indirect-buffer " *Message-Log*" "*Messages*"))

  (setq shifted-motion-keys-select-region     nil
        unshifted-motion-keys-deselect-region nil)

  (setq apropos-do-all t)
  (setq display-warning-minimum-level 'error
        log-warning-minimal-level     'info)
  (setq paren-mode 'paren))

;; Customizations which we only want to happen when we're using
;; Emacs on a TTY.
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   (when (ted-tty-p (ted-frame-display frame))
     (when (fboundp 'set-terminal-coding-system)
       (set-terminal-coding-system 'iso-8859-1))

     (let ((hober-keymap (getenv "HOBER_KEYMAP"))
           (term (getenv "TERM")))
       (when (and hober-keymap (string-equal hober-keymap "YES")
                  term (string-equal term "cons25"))
         (when (fboundp 'normal-erase-is-backspace-mode)
	   (normal-erase-is-backspace-mode -1))
	 (define-key function-key-map (kbd "ESC [ }") (kbd "<menu>")) ;%
	 (define-key function-key-map (kbd "ESC [ J") 'event-apply-super-modifier)
	 (define-key function-key-map (kbd "ESC [ ~") 'event-apply-hyper-modifier)))

     (setq browse-url-browser-function 'browse-url-lynx-emacs))))

;;; Easy installing of third-party elisp.

;; `locate-file' isn't defined in Emacs 21.
(when (and (locate-library "install") (fboundp 'locate-file))
  ;; install.el `require's em-glob.el (part of Eshell). There's a bug in
  ;; XEmacs' em-glob.el: it depends on esh-util.el but does not
  ;; `require' it. So we `require' it here.
  (require 'esh-util)
  ;; XEmacs doesn't bind `site-run-file' or `load-suffixes' by default,
  ;; and install.el assumes they're both bound.
  (let ((site-run-file (if (boundp 'site-run-file)
                           site-run-file
                         nil))
        (load-suffixes (if (boundp 'load-suffixes)
                           load-suffixes
                         '(".elc" ".elc.gz" ".el" ".el.gz"))))
    (require 'install))
  ;; Under Emacs, install.el figures out a good value of
  ;; `install-home-dir' by itself, but under XEmacs, it doesn't guess
  ;; correctly, so I explicitly set `install-home-dir' now.
  (setq install-home-dir  ted-elisp-dir
        install-home-file ted-local-elisp-config)
  ;; don't automatically byte-compile under XEmacs. FIXME.
  (setq install-byte-compile (featurep 'emacs))
  ;; install.el calls `update-directory-autoloads', which isn't defined
  ;; under XEmacs. So I defalias it here.
  ;; (XEmacs only has `batch-update-directory-autoloads'.)
  (unless (fboundp 'update-directory-autoloads)
    (defalias 'update-directory-autoloads 'ignore))
  (defun ted-install-url (url)
      (interactive "sURL: ")
      (let ((filename (car (last (split-string url "/")))))
        (when (= 0 (shell-command (format "wget -O %s \"%s\""
                                          filename url)))
          (install-file filename))))

  (defun ted-install-url-at-point ()
      (interactive)
      (ted-install-url (thing-at-point-url-at-point))))

;;; Various buffer-switching enhancements.

(setq mouse-buffer-menu-mode-mult 1)

(cond ((fboundp 'iswitchb-mode)
       (iswitchb-mode 1))
      ((fboundp 'iswitchb-default-keybindings)
       (iswitchb-default-keybindings)))

(add-hook 'iswitchb-define-mode-map-hook
          (lambda ()
            (define-key iswitchb-mode-map (kbd "C-k") nil)))

(add-to-list 'iswitchb-buffer-ignore "[*]Completions[*]")

(setq
 ibuffer-fontification-alist
 '(;; read-only buffers
   (10 buffer-read-only eshell-ls-readonly-face)
   ;; emacs' "special" buffers
   (15 (string-match "^*" (buffer-name)) eshell-ls-special-face)
   ;; hidden buffers
   (20 (and (string-match "^ " (buffer-name)) (null buffer-file-name))
       eshell-ls-symlink-face)
   ;; help buffers
   (25 (memq major-mode ibuffer-help-buffer-modes)
       eshell-ls-archive-face)
   ;; IRC buffers
   (30 (eq major-mode 'erc-mode) erc-notice-face)
   ;; dired buffers
   (35 (eq major-mode 'dired-mode) eshell-ls-directory-face)))


;;; gnuclient / emacsclient / remote editing

(cond ((and (not (featurep 'multi-tty)) (locate-library "gnuserv"))
       (when (and (featurep 'emacs) (eq system-type 'berkeley-unix))
         (require 'gnuserv-compat nil t))
       (unless (fboundp 'gnuserv-start)
         (require 'gnuserv))
       (add-hook 'emacs-startup-hook
                 (lambda ()
                   (when ted-server-emacs
                     (gnuserv-start))))
       (add-hook 'emacs-startup-hook
                 (lambda () (setq gnuserv-frame (selected-frame)))))
      ((not ted-w32-flag)
       (setq display-buffer-reuse-frames t)
       (when (or (fboundp 'make-network-process)
                 (file-executable-p (expand-file-name "emacsserver"
                                                      exec-directory)))
         (add-hook 'emacs-startup-hook
                   (lambda ()
                     (when ted-server-emacs
                       (server-start)))))))

(when (locate-library "tramp")
  (setq tramp-default-method "sudo"))


;;; Key bindings.

(when (featurep 'multi-tty)
  (defun ted-delete-frame-or-kill-emacs ()
    (interactive)
    (if (cdr (frame-list)) ; (> (length (frame-list)) 1)
        (delete-frame)
      (save-buffers-kill-emacs)))
  (global-set-key (kbd "C-x C-c") 'ted-delete-frame-or-kill-emacs))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (ted-w32-window-system-p frame)
              (setq w32-pass-lwindow-to-system nil
	            w32-pass-rwindow-to-system nil
	            w32-pass-alt-to-system     nil
	            w32-alt-is-meta            t
	            w32-pass-apps-to-system    nil
	            w32-lwindow-modifier       'super
	            w32-rwindow-modifier       'hyper
	            w32-apps-modifier          nil)
	      (define-key function-key-map (kbd "<apps>") (kbd "<menu>")))
            (when (ted-aqua-p frame)
              (setq mac-pass-command-to-system nil
	            mac-pass-control-to-system nil
	            mac-option-modifier        'super
	            mac-command-modifier       'meta
	            mac-command-key-is-meta    t))))

(mapc (lambda (key)
        (global-set-key key 'bury-buffer))
      (list (kbd "s-z") (kbd "A-z") (kbd "M-z")))
(global-set-key (kbd "<mode-line> <wheel-up>") 'next-buffer) ;%
(global-set-key (kbd "<mode-line> <wheel-up>") 'prev-buffer)

(when (fboundp 'list-text-properties-at)
  (global-set-key (kbd "C-c p") 'list-text-properties-at))

(when (fboundp 'find-function-setup-keys)
  (find-function-setup-keys))

(global-set-key [SunPowerSwitch] 'save-buffers-kill-emacs)
(global-set-key [SunCut]         'clipboard-kill-region)
(global-set-key [SunCopy]        'clipboard-kill-ring-save)
(global-set-key [SunPaste]       'clipboard-yank)
(global-set-key [find]           'apropos)
(global-set-key [SunOpen]        'find-file)
(global-set-key [cancel]         'keyboard-quit)
(global-set-key [SunProps]       'list-text-properties-at)

(global-set-key [f14] 'undo)
(global-set-key [f12] 'repeat)
(global-set-key [f19] 'apropos)
(global-set-key [f17] 'find-file)
(global-set-key [f11] 'keyboard-quit)

(global-set-key [insertchar] 'overwrite-mode)
(when (fboundp 'find-file-at-point)
  (global-set-key (kbd "C-c F") 'find-file-at-point))

(global-set-key (kbd "C-x C-b") 'iswitchb-buffer)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)
(global-set-key (kbd "C-x C-k") 'kill-buffer)
(global-set-key (kbd "C-x f") 'find-file)

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "t") 'string-insert-rectangle)
  (global-set-key (kbd "C-c r") map))

(defun ted-macro-dwim (arg)
  "DWIM keyboard macro recording and executing."
  (interactive "P")
  (if defining-kbd-macro
      (if arg
          (end-kbd-macro arg)
        (end-kbd-macro))
    (if last-kbd-macro
        (call-last-kbd-macro arg)
      (start-kbd-macro arg))))

(defun ted-macro-clear ()
  "Clear out the last keyboard macro."
  (interactive)
  (setq last-kbd-macro nil)
  (message "Last keyboard macro cleared."))

(global-set-key (kbd "<f9>") 'ted-macro-dwim) ;%
(global-set-key (kbd "M-<f9>") 'ted-macro-clear)
(define-key esc-map (kbd "<f9>") 'ted-macro-clear)
(global-set-key (kbd "s-<SPC>") 'just-one-space)
(global-set-key (kbd "s-%") 'query-replace)
(global-set-key (kbd "s-;") 'comment-dwim)
(global-set-key (kbd "s-:") 'eval-expression)
(global-set-key (kbd "s-<") 'beginning-of-buffer)
(global-set-key (kbd "s->") 'end-of-buffer)
(global-set-key (kbd "s-\\") 'delete-horizontal-space)
(global-set-key (kbd "s-b") 'backward-word)
(global-set-key (kbd "s-d") 'kill-word)
(global-set-key (kbd "s-f") 'forward-word)
(global-set-key (kbd "s-l") 'downcase-word)
(global-set-key (kbd "s-q") 'fill-paragraph)
(global-set-key (kbd "s-v") 'scroll-down)
(global-set-key (kbd "s-w") 'kill-ring-save)
(global-set-key (kbd "s-x") 'execute-extended-command)
(global-set-key (kbd "s-~") 'not-modified)
(global-set-key (kbd "s-<backspace>") 'backward-kill-word)


;;; I initialize my *scratch* buffer with a random Emacs haiku drawn
;;; from among these:

(defvar ted-emacs-haiku
  '("Oort is so awesome
     deuglifies Outlook crap
     `W k' rocks
         -- Edward O'Connor"
    "Great clouds overhead
     Tiny black birds rise and fall
     Snow covers Emacs
         -- Alex Schroeder"
    "hacking on Smyrno
     `error in process filter'
     something is b0rken
         -- Edward O'Connor"
    "Swiftly typing. Oh!
     Where would we be without you,
     `self-insert-command'?
         -- Edward O'Connor"
    "treeless quiet field
     sudden bud: EmacsWiki
     now he{ar,re} the birds sing
         -- ttn"
    "an emacs user's
     fingers dance on the keyboard;
     a nerd pianist
         -- Erik Bourget"
    "The file was open.
     flying in a sparrow stole
     a parenthesis
         -- Oliver Scholz"
    "The day went away.
     The file still puts its weight on
     the tired mode-line.
         -- Oliver Scholz"
    "On a cloudy day
     you hear the cons cells whisper:
     'We are lost and gone.'
         -- Oliver Scholz"
    "A message, a string
     remind me of my sweet love.
     Good bye, my buffers.
         -- Oliver Scholz"
    "Hot night in summer:
     Hush, you quibbling characters!
     Do not wake her up!
         -- Oliver Scholz"
    "A bright, busy day.
     The windows watch a thousand
     wild cursors dancing.
         -- Oliver Scholz"
    "Oh, why don't you are
     a lake, a stream, a meadow
     this morning, Emacs?
         -- Oliver Scholz" ;%
    "The friends chat gaily,
     I stand up to join their talk.
     My `save-excursion'.
         -- Oliver Scholz")
  "Haiku taken from the Emacs Wiki's EmacsHaiku page.")

(defun ted-random-emacs-haiku (&optional prefix)
  "Select and format a random haiku from `ted-emacs-haiku'."
  (random t)
  (let* ((prefix (or prefix ";; "))
         (n (random (length ted-emacs-haiku)))
         (haiku (nth n ted-emacs-haiku)))
    (with-temp-buffer
      (insert haiku)
      (goto-char (point-min))
      (while (< (point) (point-max))
        (goto-char (point-at-bol))
        (delete-horizontal-space)
        (insert prefix)
        (when (looking-at "--")
          (insert "    "))
        (forward-line 1))
      (concat (buffer-substring-no-properties (point-min) (point-max))
              "\n\n"))))

(setq initial-scratch-message (ted-random-emacs-haiku))

;;; Major Emacs-based applications, and Emacs interfaces to other major
;;; applications.

(when (locate-library "ledger")
  (autoload 'ledger-mode "ledger" nil t))

(when (require 'emms nil t)
  (global-set-key (kbd "s-n") 'emms-next)
  (global-set-key (kbd "s-p") 'emms-previous)
  (global-set-key (kbd "s-s") 'emms-shuffle)
  (global-set-key (kbd "s-<RET>") 'emms-play-directory-tree)
  (global-set-key (kbd "s-<SPC>") 'emms-stop))

;; Emacs games
(add-hook 'tetris-mode-hook 'ted-hide-trailing-whitespace)

(when (locate-library "chess-auto")
  (load-library "chess-auto"))

(when (locate-library "malyon")
  (autoload 'malyon "malyon" nil t)
  (add-hook 'malyon-mode-hook 'ted-hide-trailing-whitespace))

;; Viper, the VI-like editor in Emacs
(when (featurep 'aquamacs)
  (raise-frame))
;; (setq viper-mode t)
;; (add-hook 'emacs-startup-hook 'viper-mode)

(eval-after-load "viper-init"
  '(progn
     (mapc (lambda (hook)
             (remove-hook hook 'viper-restore-cursor-type))
           '(viper-vi-state-hook viper-replace-state-hook
             viper-emacs-state-hook))
     (remove-hook 'viper-insert-state-hook
                  'viper-set-insert-cursor-type)))


;; Eshell, the Emacs Shell
(when (locate-library "eshell")
  (when (not (fboundp 'eshell))
    (autoload 'eshell "eshell" nil t))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-a") 'eshell-bol)))

  (setq eshell-save-history-on-exit t
        eshell-hist-ignoredups      nil)

  (setq eshell-default-target-is-dot t
        eshell-pushd-tohome          t)

  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-banner-message (ted-random-emacs-haiku ""))

  (when (facep 'header-line) ; Is there a better test for header-line-ness?
    (add-hook 'eshell-mode-hook
            (lambda ()
              (setq header-line-format mode-line-format
                    mode-line-format   nil))))

  (when (memq system-type '(darwin berkeley-unix))
    (defun ted-eshell-C-t ()
      "Request status of the running Eshell command.
  Only works on BSD."
      (interactive)
      ;; An anamorphic `when' would be nice here.
      (let ((proc (eshell-interactive-process)))
        (if proc
            (progn
              (process-send-string proc (string 20)))
          (call-interactively 'transpose-chars))))
    (add-hook 'eshell-mode-hook
              (lambda ()
                (local-set-key (kbd "C-t") 'ted-eshell-C-t))))
  (defun ted-eshell-prompt ()
    (let ((user (or (getenv "USER") (user-login-name) "ted"))
          (host (car (split-string
                      (or (getenv "HOST") (system-name) "unknown")
                      "\\.")))
          (char (if (= (user-uid) 0) "#" ":")))
      (format "\n%s@%s%s " user host char)))
  (setq eshell-prompt-function 'ted-eshell-prompt)
  (setq eshell-prompt-regexp "^[^#:\n]*[#:] ")

  (autoload 'ansi-color-filter-apply "ansi-color")
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "s-p")
                             'eshell-previous-matching-input-from-input)))

  (when (featurep 'xemacs)
    (eval-after-load "esh-cmd"
      '(defun eshell-find-alias-function (name)
         "Check whether a function called `eshell/NAME' exists."
         (let* ((sym (intern-soft (concat "eshell/" name)))
                (file (symbol-file sym)))
           ;; If the function exists, but is defined in an eshell module
           ;; that's not currently enabled, don't report it as found
           (if (and file
                    (string-match "\\(em\\|esh\\)-\\(.*\\)\\(\\.el\\)?\\'" file))
               (let ((module-sym
                      (intern (file-name-sans-extension
                               (file-name-nondirectory
                                (concat "eshell-" (match-string 2 file)))))))
                 (if (and (functionp sym)
                          (or (null module-sym)
                              (eshell-using-module module-sym)
                              (memq module-sym (eshell-subgroups 'eshell))))
                     sym))
             ;; Otherwise, if it's bound, return it.
             (if (functionp sym)
                 sym))))))

  (setq eshell-aliases-file "~/.alias")

  (defun eshell/less (file)
    "Pager view of FILE."
    (view-file file)
    0)
  (defalias 'eshell/more 'eshell/less)

  (defalias 'eshell/clear 'ted-clear)

  (defun eshell/info (subject)
    "Read the Info manual on SUBJECT."
    (let ((buf (current-buffer)))
      (Info-directory)
      (let ((node-exists (ignore-errors (Info-menu subject))))
        (if node-exists
            0
          (switch-to-buffer buf)
          (eshell-print (format "There is no Info manual on %s.\n"
                                subject))
          1))))

  (defun eshell/emacs (&rest args)
    "Open a file in Emacs. Some habits die hard."
    (if (null args)
        (bury-buffer)
      (mapc 'find-file (mapcar 'expand-file-name
                               (eshell-flatten-list args))))
    0)
    (defalias 'eshell/emacsclient 'eshell/emacs)

  (defun eshell/vi (file)
    "Open a file with Viper."
    (with-current-buffer (find-file file)
      (setq viper-mode t)
      (viper-mode))
    0)

  (defalias 'eshell/concat 'eshell/cat)

  (eval-after-load "em-ls"
    '(progn
       (defvar ted-eshell-ls-keymap
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
           (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point) ;%
           (define-key map (if (featurep 'xemacs)
                               (kbd "<button2>")
                             (kbd "<mouse-2>"))
             'pat-eshell-ls-find-file-at-mouse-click)
           map))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, middle-click: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)

       (defun ted-eshell-ls-find-file-at-point (point)
         "RET on Eshell's `ls' output to open files."
         (interactive "d")
         (find-file (buffer-substring-no-properties
                     (previous-single-property-change point 'help-echo)
                     (next-single-property-change point 'help-echo))))

       ;; Not defined in Emacs.
       (unless (fboundp 'event-point)
         (defun event-point (event)
           "Return the character position of mouse EVENT."
           (posn-point (event-end event))))

       (defun pat-eshell-ls-find-file-at-mouse-click (event)
         "Middle click on Eshell's `ls' output to open files.
       From Patrick Anderson via the EmacsWiki."
         (interactive "e")
         (ted-eshell-ls-find-file-at-point (event-point event))))))

;; Emacs web browsing, web searching, and more!
(define-key minibuffer-local-must-match-map (kbd "?") nil)
(define-key minibuffer-local-completion-map (kbd "?") nil)

(when (locate-library "w3m")
  (autoload 'w3m "w3m" nil t)
  (autoload 'w3m-goto-url "w3m" nil t)
  (autoload 'w3m-region "w3m")

  (setq w3m-home-page
        (if (file-readable-p "~/html/home.html")
            (concat "file://" (expand-file-name "~/html/home.html"))
          "http://edward.oconnor.cx/home"))

  (setq w3m-use-toolbar t
        w3m-use-tab     nil
        w3m-key-binding 'info)

  (setq w3m-search-default-engine "google")

  (setq w3m-command-arguments       '("-F" "-cookie")
        w3m-mailto-url-function     'compose-mail
        browse-url-browser-function 'w3m
        mm-text-html-renderer       'w3m)

  (add-hook 'w3m-mode-hook 'ted-hide-trailing-whitespace)

  (defalias 'eshell/w3m 'w3m)

  (setq w3m-use-cookies t)
  (setq w3m-cookie-accept-bad-cookies t)

  (defun ted-w3m-edit-emacswiki-page (url)
    (let ((node (substring (substring w3m-current-url
                                      (string-match "wiki[/?][^/&=]+\\'"
                                                    w3m-current-url))
                           5)))
      (w3m-goto-url (concat "http://www.emacswiki.org/cgi-bin/wiki"
                            "?action=edit;id=" node))))

  (defun ted-delicious-url ()
    "Bookmark this page with del.icio.us."
    (interactive)
    (w3m-goto-url
     (concat "http://del.icio.us/hober?"
             "url="    (w3m-url-encode-string w3m-current-url)
             "&title=" (w3m-url-encode-string w3m-current-title))))

  (eval-after-load "w3m"
    '(progn
       (add-to-list 'w3m-uri-replace-alist
                    '("\\`lj:\\(.+\\)" w3m-pattern-uri-replace
                      "http://www.livejournal.com/users/\\1/"))
       (add-to-list 'w3m-edit-function-alist
                    '(".*emacswiki.org/cgi-bin/wiki.*"
                      . ted-w3m-edit-emacswiki-page))
       (define-key w3m-info-like-map "a" 'ted-delicious-url))))

(when (locate-library "backpack")
  (setq backpack-username "hober")

  (defvar ted-backpack-map (make-sparse-keymap))
  (global-set-key (kbd "C-c b") ted-backpack-map)

  (mapc (lambda (cons)
          (let ((command (car cons)))
            (autoload command "backpack" nil t)
            (define-key ted-backpack-map (cdr cons) command)))
        '((backpack-remind             . "r")
          (backpack-remind-from-region . "R"))))

(when (ignore-errors (require 'google))
  (defun ted-google-word-at-point ()
    "Google the word at point."
    (interactive)
    (google-search (word-at-point)))

  (defun ted-google-sentence-at-point ()
    "Google the text of the sentence at point."
    (interactive)
    (google-search (sentence-at-point)))

  (defvar ted-google-prefix-map (make-sparse-keymap)
    "Keymap for my ted-google* commands.")

  (global-set-key (kbd "C-c g") ted-google-prefix-map)
  (define-key ted-google-prefix-map "g" 'google-search)
  (define-key ted-google-prefix-map (kbd "RET") 'google-search)
  (define-key ted-google-prefix-map "r" 'google-search-region)
  (define-key ted-google-prefix-map "s" 'ted-google-sentence-at-point)
  (define-key ted-google-prefix-map "w" 'ted-google-word-at-point))

(when (locate-library "ell")
  (setq ell-locate t)
  (setq ell-goto-addr t)
  (autoload 'ell-packages "ell" nil t))

(when (locate-library "wikiarea")
  (autoload 'wikiarea "wikiarea" nil t)
  (setq wikiarea-managed-directory
        (expand-file-name "emacs-wiki/" ted-elisp-dir)))

;; Email.
(defalias 'gnw 'gnus)

(when (locate-library "sieve-mode")
  (autoload 'sieve-mode "sieve-mode" nil t)
  (add-to-list 'auto-mode-alist
               '("\\.si\\(eve\\|v\\)\\'" . sieve-mode)))

(setq mail-user-agent   'gnus-user-agent
      read-mail-command 'gnus)

(setq mail-signature t
      mail-yank-prefix "> "
      mail-from-style 'angles)

(defun ted-determine-gnus-version ()
  "Determine the version of Gnus."
  (setq ted-oort+-flag
        (> (gnus-continuum-version (gnus-version)) 5.09)))

(add-hook 'gnus-before-startup-hook 'ted-determine-gnus-version)

(add-to-list 'auto-mode-alist '("\\.SCORE\\'" . gnus-score-mode))

(when (locate-library "boxquote")
  (defvar ted-boxquote-map (make-sparse-keymap))
  (global-set-key (kbd "C-c q") ted-boxquote-map)
  (mapc (lambda (cons)
          (let ((command (car cons))
                (key (cdr cons)))
            (autoload command "boxquote" nil t)
            (define-key ted-boxquote-map key command)))
        '((boxquote-region            . "r")
          (boxquote-buffer            . "b")
          (boxquote-insert-file       . "i")
          (boxquote-yank              . "y")
          (boxquote-defun             . "F")
          (boxquote-paragraph         . "p")
          (boxquote-describe-function . "f")
          (boxquote-describe-variable . "v")
          (boxquote-describe-key      . "k")
          (boxquote-kill              . "K")
          (boxquote-unbox             . "u"))))

(when (require 'bbdb nil t)
  (setq bbdb-default-country nil)
  (setq bbdb-debug nil)
  (setq bbdb-file "~/.bbdb")
  (setq bbdb-completion-display-record nil)

  (when (ted-coding-system-p 'utf-8)
    (setq bbdb-file-coding-system 'utf-8))
  (bbdb-initialize 'sendmail 'gnus 'message)

  (when (featurep 'eshell)
    (defun eshell/bbdb (&optional (regex ".*"))
      (bbdb regex nil)))

  (add-hook 'message-setup-hook 'bbdb-define-all-aliases))

(defconst ted-bbdb-flag (featurep 'bbdb))

;; Dired, the Emacs directory editor.

(setq dired-dwim-target t)

(setq dired-recursive-deletes 'top
      dired-recursive-copies  'top)

(when (locate-library "wdired")
  (autoload 'wdired-change-to-wdired-mode "wdired" nil t)
  (add-hook 'dired-load-hook
            (lambda ()
              (define-key dired-mode-map (kbd "r")
                'wdired-change-to-wdired-mode))))

;; ljupdate, an Emacs LiveJournal client
(when (require 'ljupdate nil t)
  (setq lj-cache-login-information t
        lj-default-username        "hober"
        lj-fill-function           'ignore)
  (global-set-key (kbd "C-c j c") 'lj-compose)
  (global-set-key (kbd "C-c j l") 'lj-login)
  ;; handy for developing / testing in *ielm*
  (setq lj "www.livejournal.com"
        dj "www.deadjournal.com"))

;; ERC, an Emacs IRC client
(when (locate-library "erc")
  (autoload 'erc-select-read-args "erc" nil nil) ; needed for XEmacs
  (autoload 'erc-select "erc" nil t)

  (setq erc-server                         "irc.freenode.net"
        erc-port                           6667
        erc-user-full-name                 "Edward O'Connor"
        erc-email-userid                   "ted"
        erc-nick                           '("hober" "hober2" "hober3")
        erc-password                       nil ; set this in local config
        erc-nickserv-passwords             nil ; set this in local config
        erc-anonymous-login                t
        erc-auto-query                     t
        erc-max-buffer-size                30000
        erc-prompt-for-password            nil
        erc-join-buffer                    'buffer
        erc-command-indicator              "CMD"
        erc-echo-notices-in-current-buffer t
        erc-send-whitespace-lines          nil
        erc-hide-list                      '())

  (setq erc-quit-reason-various-alist
        '(("brb"    "I'll be right back.")
          ("lunch"  "Having lunch.")
          ("dinner" "Having dinner.")
          ("food"   "Getting food.")
          ("sleep"  "Sleeping.")
          ("work"   "Getting work done.")
          (".*"     (yow))))

  (setq erc-part-reason-various-alist erc-quit-reason-various-alist
        erc-part-reason               'erc-part-reason-various
        erc-quit-reason               'erc-quit-reason-various))

;; rcirc
(when (locate-library "rcirc")
  (setq rcirc-nick "hober")
  (add-hook 'rcirc-mode-hook 'ted-hide-trailing-whitespace)
)

(when (locate-library "lisppaste")
  (autoload 'lisppaste "lisppaste" nil t))


;;; Programming and other forms of document preparation.

(require 'paredit nil t)

(setq diff-switches "-u")

(autoload 'diff-context->unified "diff-mode" nil t)
(autoload 'diff-unified->context "diff-mode" nil t)

(setq vc-follow-symlinks t)

(setq skeleton-pair t)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)

(setq glasses-separator           "-"
      glasses-uncapitalize-regexp ".*"
      glasses-uncapitalize-p      t)

(let ((bzr-mode (locate-library "bzr-mode.el")))
  (when bzr-mode
    (load bzr-mode)))


(when (locate-library "psvn")
  (autoload 'svn-status "psvn" nil t))

(when (locate-library "vc-svn")
  (unless (memq 'SVN vc-handled-backends)
    (add-to-list 'vc-handled-backends 'SVN)))

(when (locate-library "pcmpl-darcs")
  (autoload 'pcomplete/darcs "pcmpl-darcs" nil nil))

(when (locate-library "darcsum")
  (autoload 'darcsum-whatsnew "darcsum" nil t))

(when (locate-library "vc-darcs")
  (require 'vc)
  (require 'vc-darcs)
  (when (boundp 'vc-handled-backends)
    (add-to-list 'vc-handled-backends 'DARCS)))

(defun ted-darcs-readonly-current ()
  "Ensure we open files in darcs' current directory read-only."
  (when (string-match ".*/_darcs/current/.*" (buffer-file-name))
    (toggle-read-only 1)))

(add-hook 'find-file-hook 'ted-darcs-readonly-current)

(add-to-list 'auto-mode-alist '("\\.cs\\'" . java-mode))

(defun ted-c-sharp-fix-tab-width ()
  (when (string-match "\\.cs\\'" (buffer-file-name))
    (setq tab-width 2)))

(add-hook 'java-mode-hook 'ted-c-sharp-fix-tab-width)

(defun ted-c-kill-backwards-into-nomenclature ()
  "Delete the CamelCase word before point."
  (interactive)
  (let ((end (point)))
    (c-backward-into-nomenclature 1)
    (kill-region (point) end)))

(defun ted-c-kill-forwards-into-nomenclature ()
  "Delete the CamelCase word after point."
  (interactive)
  (let ((beg (point)))
    (c-forward-into-nomenclature 1)
    (kill-region beg (point))))

(add-hook 'java-mode-hook
          (lambda ()
            (local-set-key (kbd "M-DEL")
                           'ted-c-kill-backwards-into-nomenclature)
            (local-set-key (kbd "M-d")
                           'ted-c-kill-forwards-into-nomenclature)))

(when (boundp 'auto-coding-alist)
  (add-to-list 'auto-coding-alist '("\\.[jw]ar\\'" . no-conversion))
  (add-to-list 'auto-coding-alist '("\\.[JW]AR\\'" . no-conversion)))
(add-to-list 'auto-mode-alist '("\\.[jw]ar\\'" . archive-mode))
(add-to-list 'auto-mode-alist '("\\.[JW]AR\\'" . archive-mode))


(when (locate-library "haskell-mode")
  (add-to-list 'auto-mode-alist '("\\.\\([hg]s\\|hi\\)\\'" . haskell-mode))
  (add-to-list 'auto-mode-alist '("\\.l[hg]s\\'" . literate-haskell-mode))

  (autoload 'haskell-mode "haskell-mode" nil t)
  (autoload 'literate-haskell-mode "haskell-mode" nil t)

  (mapc (lambda (hook)
          (add-hook 'haskell-mode-hook hook))
        '(turn-on-haskell-font-lock turn-on-haskell-decl-scan
          turn-on-haskell-doc-mode turn-on-haskell-indent
          turn-on-haskell-hugs)))

;; Moved load of generic-x up before we look for javascript.el: both put
;; entries in `auto-mode-alist' for "\\.js\\'", and I want the entry
;; from javascript.el.
(ignore-errors (require 'generic-x nil t))


(add-to-list 'auto-mode-alist '("\\.\\(jsp\\|tpl\\|tag\\)\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.\\(wsd[dl]\\|tld\\|xslt\\)\\'" . xml-mode))

(defvar ted-html4-entity-map
  '(;; latin-1
    ("nbsp" . 160) ("iexcl" . 161) ("cent" . 162) ("pound" . 163)
    ("curren" . 164) ("yen" . 165) ("brvbar" . 166) ("sect" . 167)
    ("uml" . 168) ("copy" . 169) ("ordf" . 170) ("laquo" . 171)
    ("not" . 172) ("shy" . 173) ("reg" . 174) ("macr" . 175)
    ("deg" . 176) ("plusmn" . 177) ("sup2" . 178) ("sup3" . 179)
    ("acute" . 180) ("micro" . 181) ("para" . 182) ("middot" . 183)
    ("cedil" . 184) ("sup1 #185") ("ordm" . 186) ("raquo" . 187)
    ("frac14" . 188) ("frac12" . 189) ("frac34" . 190) ("iquest" . 191)
    ("Agrave" . 192) ("Aacute" . 193) ("Acirc" . 194) ("Atilde" . 195)
    ("Auml" . 196) ("Aring" . 197) ("AElig" . 198) ("Ccedil" . 199)
    ("Egrave" . 200) ("Eacute" . 201) ("Ecirc" . 202) ("Euml" . 203)
    ("Igrave" . 204) ("Iacute" . 205) ("Icirc" . 206) ("Iuml" . 207)
    ("ETH" . 208) ("Ntilde" . 209) ("Ograve" . 210) ("Oacute" . 211)
    ("Ocirc" . 212) ("Otilde" . 213) ("Ouml" . 214) ("times" . 215)
    ("Oslash" . 216) ("Ugrave" . 217) ("Uacute" . 218) ("Ucirc" . 219)
    ("Uuml" . 220) ("Yacute" . 221) ("THORN" . 222) ("szlig" . 223)
    ("agrave" . 224) ("aacute" . 225) ("acirc" . 226) ("atilde" . 227)
    ("auml" . 228) ("aring" . 229) ("aelig" . 230) ("ccedil" . 231)
    ("egrave" . 232) ("eacute" . 233) ("ecirc" . 234) ("euml" . 235)
    ("igrave" . 236) ("iacute" . 237) ("icirc" . 238) ("iuml" . 239)
    ("eth" . 240) ("ntilde" . 241) ("ograve" . 242) ("oacute" . 243)
    ("ocirc" . 244) ("otilde" . 245) ("ouml" . 246) ("divide" . 247)
    ("oslash" . 248) ("ugrave" . 249) ("uacute" . 250) ("ucirc" . 251)
    ("uuml" . 252) ("yacute" . 253) ("thorn" . 254) ("yuml" . 255)
    ;; special
    ; ("quot" . 34) ("amp" . 38) ("lt" . 60) ("gt" . 62)
    ("OElig" . 338) ("oelig" . 339) ("Scaron" . 352) ("scaron" . 353)
    ("Yuml" . 376) ("circ" . 710) ("tilde" . 732) ("ensp" . 8194)
    ("emsp" . 8195) ("thinsp" . 8201) ("zwnj" . 8204) ("zwj" . 8205)
    ("lrm" . 8206) ("rlm" . 8207) ("ndash" . 8211) ("mdash" . 8212)
    ("lsquo" . 8216) ("rsquo" . 8217) ("sbquo" . 8218) ("ldquo" . 8220)
    ("rdquo" . 8221) ("bdquo" . 8222) ("dagger" . 8224) ("Dagger" . 8225)
    ("permil" . 8240) ("lsaquo" . 8249) ("rsaquo" . 8250) ("euro" . 8364)
    ;; symbol
    ("fnof" . 402) ("Alpha" . 913) ("Beta" . 914) ("Gamma" . 915)
    ("Delta" . 916) ("Epsilon" . 917) ("Zeta" . 918) ("Eta" . 919)
    ("Theta" . 920) ("Iota" . 921) ("Kappa" . 922) ("Lambda" . 923)
    ("Mu" . 924) ("Nu" . 925) ("Xi" . 926) ("Omicron" . 927)
    ("Pi" . 928) ("Rho" . 929) ("Sigma" . 931) ("Tau" . 932)
    ("Upsilon" . 933) ("Phi" . 934) ("Chi" . 935) ("Psi" . 936)
    ("Omega" . 937) ("alpha" . 945) ("beta" . 946) ("gamma" . 947)
    ("delta" . 948) ("epsilon" . 949) ("zeta" . 950) ("eta" . 951)
    ("theta" . 952) ("iota" . 953) ("kappa" . 954) ("lambda" . 955)
    ("mu" . 956) ("nu" . 957) ("xi" . 958) ("omicron" . 959)
    ("pi" . 960) ("rho" . 961) ("sigmaf" . 962) ("sigma" . 963)
    ("tau" . 964) ("upsilon" . 965) ("phi" . 966) ("chi" . 967)
    ("psi" . 968) ("omega" . 969) ("thetasym" . 977) ("upsih" . 978)
    ("piv" . 982) ("bull" . 8226) ("hellip" . 8230) ("prime" . 8242)
    ("Prime" . 8243) ("oline" . 8254) ("frasl" . 8260) ("weierp" . 8472)
    ("image" . 8465) ("real" . 8476) ("trade" . 8482) ("alefsym" . 8501)
    ("larr" . 8592) ("uarr" . 8593) ("rarr" . 8594) ("darr" . 8595)
    ("harr" . 8596) ("crarr" . 8629) ("lArr" . 8656) ("uArr" . 8657)
    ("rArr" . 8658) ("dArr" . 8659) ("hArr" . 8660) ("forall" . 8704)
    ("part" . 8706) ("exist" . 8707) ("empty" . 8709) ("nabla" . 8711)
    ("isin" . 8712) ("notin" . 8713) ("ni" . 8715) ("prod" . 8719)
    ("sum" . 8721) ("minus" . 8722) ("lowast" . 8727) ("radic" . 8730)
    ("prop" . 8733) ("infin" . 8734) ("ang" . 8736) ("and" . 8743)
    ("or" . 8744) ("cap" . 8745) ("cup" . 8746) ("int" . 8747)
    ("there4" . 8756) ("sim" . 8764) ("cong" . 8773) ("asymp" . 8776)
    ("ne" . 8800) ("equiv" . 8801) ("le" . 8804) ("ge" . 8805)
    ("sub" . 8834) ("sup" . 8835) ("nsub" . 8836) ("sube" . 8838)
    ("supe" . 8839) ("oplus" . 8853) ("otimes" . 8855) ("perp" . 8869)
    ("sdot" . 8901) ("lceil" . 8968) ("rceil" . 8969) ("lfloor" . 8970)
    ("rfloor" . 8971) ("lang" . 9001) ("rang" . 9002) ("loz" . 9674)
    ("spades" . 9824) ("clubs" . 9827) ("hearts" . 9829) ("diams" . 9830)
    )
  "Alist mapping HTML 4.01 named character entity references to their
  numerical counterparts. Taken from the HTML 4.01 specification:
    http://www.w3.org/TR/html401/")


(defvar ted-html4-intrinsic-events
  '("load" "unload" "click" "dblclick" "mousedown" "mouseup" "mouseover"
    "mousemove" "mouseout" "focus" "blur" "keypress" "keydown" "keyup"
    "submit" "reset" "select" "change")
  "HTML4 intrinsic events.")

(defun ted-numericalize-entity ()
  "Replace the named character entity reference at point with its
numerical equivalent, if known."
  (interactive)
  (let ((end (point))
        (start (search-backward "&" (- (point) 7) t)))
    (when start
      (if (looking-at "&\\([a-z][a-z0-9]+\\)")
          (let* ((name (match-string 1))
                 (num (cdr (assoc name ted-html4-entity-map))))
            (if num
                (progn
                  (delete-region start end)
                  (insert (format "&#%s;" num))
                  t)
              (goto-char end)
              nil))
        (goto-char end)
        nil))))

;;; nxml
(when (and (featurep 'emacs) (load "rng-auto" t))
  ;; Use nxml instead of Emacs' default HTML and XML editing modes.
  (defalias 'html-mode 'nxml-mode)
  (defalias 'xml-mode 'nxml-mode)

  ;; Hack `;' in nxml mode to automatically fix named character entity
  ;; references.
  (defun ted-nxml-semicolon-dwim (&rest ignore)
    "If we've just typed an HTML 4 named character entity reference,
replace it with its numerical equivalent. Otherwise, just insert `;'."
    (interactive)
    (or (ted-numericalize-entity) (insert ";")))

  (eval-after-load "nxml-mode"
    '(progn
       (define-key nxml-mode-map (kbd "RET") 'newline-and-indent)
       (define-key nxml-mode-map (kbd "C-c C-t") 'sgml-tag)
       ;; Install my `;' hack.
       (define-key nxml-mode-map (kbd ";") 'ted-nxml-semicolon-dwim))))

(when (locate-library "javascript")
  (setq js-indent-level 2)
  (autoload 'javascript-mode "javascript" nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
  (when (boundp 'javascript-mode-abbrev-table)
    (add-hook 'javascript-mode-hook
              (lambda ()
                (setq local-abbrev-table javascript-mode-abbrev-table)))))

(when (locate-library "css-mode")
  (autoload 'css-mode "css-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
  (eval-after-load "css-mode"
    '(cond ((boundp 'cssm-indent-function) ; larsga's css-mode.el
              (add-hook 'css-mode-hook
	                (lambda ()
	                  (setq cssm-mirror-mode nil
	                        cssm-newline-before-closing-bracket nil
	                        cssm-indent-function 'cssm-c-style-indenter))))
             ((fboundp 'css-extract-keyword-list) ; monnier's css-mode.el
              (setq css-basic-offset 2))
      (t nil))))

(add-to-list 'auto-mode-alist '("\\.\\(rdf\\|rss\\|atom\\)\\'" . xml-mode))

(defun ted-insert-cfhp-uuid ()
  (interactive)
  (insert "http://cfhp.org/id/"
          (substring (shell-command-to-string "uuidgen") 0 -1)))

(defun ted-insert-w3c-current-time (gmt)
  (interactive "P")
  (let ((iso8601 (format-time-string "%Y-%m-%dT%T%z" (current-time) gmt)))
    (let ((len (length iso8601)))
      (if gmt
          (insert (substring iso8601 0 (- len 5)) "Z")
        (let ((len-2 (- len 2)))
          (insert (substring iso8601 0 len-2) ":"
                  (substring iso8601 len-2)))))))

(defun ted-insert-char-entity-maybe (char entity)
  (if (equal (preceding-char) char)
      (progn
        (backward-delete-char 1)
        (insert entity))
    (insert char)))

(eval-after-load "sgml-mode"
  '(mapc (lambda (char-entity-alist)
           (let ((cmd `(lambda ()
                         (interactive)
                         (ted-insert-char-entity-maybe
                          ,(car char-entity-alist)
                          ,(cdr char-entity-alist)))))
             (define-key html-mode-map (string (car char-entity-alist))
               cmd)))
         '((?< . "&lt;")
           (?> . "&gt;")
           (?& . "&amp;")
           (?\" . "&quot;")
           (?\' . "&apos;"))))

;; completion in M-:
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(defun ted-install-lispy-bindings (map bind-ret)
  "FIXME"
  (define-key map (kbd "\"")
    (find-if 'commandp '(paredit-doublequote skeleton-pair-insert-maybe)))
  (when bind-ret
    (define-key map (kbd "RET")
      (find-if 'commandp '(paredit-newline newline-and-indent))))
  (define-key map (kbd "(")
    (find-if 'commandp '(paredit-open-parenthesis
                         paredit-open-list
                         insert-parentheses)))
  (define-key map (kbd ")")
    (find-if 'commandp '(paredit-close-parenthesis-and-newline
                         ;; paredit-close-list-and-newline
                         move-past-close-and-reindent))))
(ted-install-lispy-bindings
 (cond ((boundp 'lisp-mode-shared-map) lisp-mode-shared-map)
       ((boundp 'shared-lisp-mode-map) shared-lisp-mode-map)
       (t emacs-lisp-mode-map))
 t)

(eval-after-load "ielm"
  '(ted-install-lispy-bindings ielm-map nil))

(add-to-list 'auto-mode-alist '("\\.elc\\'" . emacs-lisp-mode))

(defun ted-dedangle-parens-in-region (start end)
  "De-dangle close parens between START and END."
  (interactive "r")
  (goto-char start)
  (while (re-search-forward "[ \t\n]+)" end t)
    (replace-match ")")))

(defun ted-make-lisp-idiomatic-in-region (start end)
  "Make the Lisp code from START to END a bit more idiomatic.
You might consider running `checkdoc' as well."
  (interactive "r\nP")
  (save-restriction
    (widen)
    (narrow-to-region start end)
    (setq start (point-min-marker)
          end   (point-max-marker))
    (ted-dedangle-parens-in-region start end)
    (indent-region start end)))

(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p (ted-frame-display frame))
              (require 'parenface nil t))))

(when (locate-library "iuscheme")
  (autoload 'scheme-mode "iuscheme" nil t)
  (add-to-list 'auto-mode-alist
               '("\\.s\\(s\\|cm\\)\\'" . scheme-mode))
  (autoload 'run-scheme "iuscheme" nil t))

(define-skeleton ted-elisp-when-locate-library-skeleton
  "Skeleton for (when (locate-library \"foo\") ... ) forms."
  ;; This `completing-read' form based on `find-library's `interactive'
  ;; spec, but generalized to work under different Emacsen.
  (completing-read "Library name: "
                   (when (fboundp 'locate-file-completion)
                     'locate-file-completion)
                   (cons (if (boundp 'find-function-source-path)
                             find-function-source-path
                           load-path)
                         ;; (find-library-suffixes)
                         '(".el" ".el.gz" ".gz")))
  "when (locate-library \"" str "\")")

(when (locate-library "eldoc")
  (mapc (lambda (mode-hook)
          (add-hook mode-hook 'turn-on-eldoc-mode))
        '(emacs-lisp-mode-hook lisp-interaction-mode-hook
          ielm-mode-hook))

  (setq eldoc-argument-case 'help-default-arg-highlight))

(defun ted-macroexpand-sexp-at-point ()
  "Replace the s-expresion at point with its macroexpansion."
  (interactive)

  (let (pre start end)
    (save-excursion
      (up-list -1)
      (setq start (point))
      (setq pre (sexp-at-point))
      (forward-sexp 1)
      (setq end (point)))

    (goto-char start)
    (kill-region start end)

    (pp (macroexpand pre) (current-buffer))))

(defun ted-indent-containing-sexp ()
  "Fix the indentation of the sexp containing point."
  (interactive)
  (save-excursion
    (up-list -1)
    (indent-sexp)))

(global-set-key (kbd "C-c i") 'ted-indent-containing-sexp)

(setq ielm-prompt "* ")

(add-to-list 'auto-mode-alist '("\\.asd" . lisp-mode))

(add-to-list 'auto-mode-alist '("\\.lisp-expr" . lisp-mode))

(mapc (lambda (hook)
        (add-hook hook
                  (lambda ()
                    (set (make-local-variable 'lisp-indent-function)
                         'common-lisp-indent-function))))
      '(lisp-mode-hook inferior-lisp-mode-hook))

(setq inferior-lisp-program
      (or (executable-find "sbcl")
          (executable-find "lisp")
          (executable-find "openmcl")
          (executable-find "clisp")))

(when (locate-library "slime")
  (autoload 'slime-mode "slime" nil t)
  (add-hook 'lisp-mode-hook (lambda nil (slime-mode 1)))
  (autoload 'inferior-slime-mode "slime" nil t)
  (add-hook 'inferior-lisp-mode-hook
            (lambda nil (inferior-slime-mode 1)))
  (add-hook 'slime-repl-mode-hook 'ted-hide-trailing-whitespace))

(cond ((commandp 'find-library)
       (defalias 'ted-find-library 'find-library))
      ((fboundp 'find-library)
       (defun ted-find-library (library)
         "Open LIBRARY."
         (interactive "sLibrary: ")
         (find-library library)))
      (t
       (defun ted-find-library (library)
         "Open LIBRARY."
         (interactive "sLibrary: ")
         (let ((filename (locate-library (concat library ".el"))))
           (if (stringp filename)
               (find-file filename)
             (message "Library %s not found." library))))))

(global-set-key (kbd "C-c L") 'ted-find-library)

(when (locate-library "rpm-spec-mode")
  (unless (fboundp 'user-mail-address)
    (defun user-mail-address ()
      "Returns the value of the `user-mail-address' variable."
      user-mail-address))

  (autoload 'rpm-spec-mode "rpm-spec-mode" nil t)
  ;; RPM specfiles are .spec, EVDB specfiles are .pkgspec
  (add-to-list 'auto-mode-alist '("\\.\\(pkg\\)?spec\\'" . rpm-spec-mode)))

(when (locate-library "rfcview")
  (autoload 'rfcview-mode "rfcview" nil t)
  (add-to-list 'auto-mode-alist '("rfc[0-9]+\\.txt" . rfcview-mode)))

(when (locate-library "cfengine")
  (autoload 'cfengine-mode "cfengine" nil t)
  (add-to-list 'auto-mode-alist
               '("cf\\(\\.\\|agent\\.conf\\)" . cfengine-mode))
  (defalias 'cfengine-beginning-of-line 'beginning-of-line)
  (setq cfengine-indent 4))

(defvar ted-ports-tree-root
  (cond ((eq system-type 'darwin)
         (concat "/opt/local/var/db/dports/sources/"
                 "rsync.rsync.opendarwin.org_dpupdate_dports"))
        ((eq system-type 'berkeley-unix)
         "/usr/ports")
        (t nil))
  "Directory under which this system's ports tree lives.")

(when (and (stringp ted-ports-tree-root)
           (file-directory-p ted-ports-tree-root))
  (defun ted-list-ports-tree-categories ()
    (let* ((default-directory ted-ports-tree-root)
           (subdirs (directory-files default-directory nil "[a-z].*"))
           (case-fold-search nil)
           retval)
      (mapc (lambda (file)
              (when (and (file-directory-p file)
                         (string-match "^[a-z]" file))
                (push file retval)))
            subdirs)
      (nreverse retval)))

  (defun ted-list-ports-in-category (category)
    (let* ((default-directory (expand-file-name category ted-ports-tree-root))
           (listing (directory-files default-directory))
           subdirs)
      (mapc (lambda (file)
              (when (and (file-directory-p file)
                         (string-match "^[a-z]" file))
                (push file subdirs)))
            listing)
      (nreverse subdirs)))

  (defun ted-read-port ()
    (let* ((categories (ted-list-ports-tree-categories))
           (category (completing-read "Port category: "
                                      (mapcar (lambda (entry)
                                                (cons entry entry))
                                              categories)
                                      nil t))
           (ports (ted-list-ports-in-category category))
           (port (completing-read "Port: " (mapcar (lambda (entry)
                                                     (cons entry entry))
                                                   ports)
                                  nil t)))
      (concat category "/" port))))

(when (locate-library "ruby-mode")
  ;; Autoloads
  (autoload 'ruby-mode "ruby-mode" nil t)

  ;; File associations, etc.
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
  ;; fixme: use two-mode-mode when possible
  (add-to-list 'auto-mode-alist '("\\.rhtml\\'" . html-mode))

  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

  ;; Key bindings
  (eval-after-load "ruby-mode"
    '(define-key ruby-mode-map (kbd "RET")
       'ruby-reindent-then-newline-and-indent))

  ;; Skeletons

  (define-skeleton ted-rails-migrate-create-table
    "Skeleton for creating a table in a rails migration."
    "Table name: "
    > "create_table \"" str "\" do |t|" \n
    _ \n
    -2 "end" \n)

  (define-skeleton ted-rails-migrate-drop-table
    "Skeleton for dropping a table in a rails migration."
    "Table name: "
    > "drop_table \"" str "\"" \n)

  (define-skeleton ted-rails-migrate-table-column
    "Skeleton for adding a column in a rails migration."
    "Column name: "
    > "t.column \"" str "\", :" (skeleton-read "Column type: " "string"))

  (define-skeleton ted-rails-migrate-add-column
    "Skeleton for adding a column in a rails migration."
    "Table name: "
    > "add_column \"" str
    "\", \"" (skeleton-read "Column name: ")
    "\", :" (skeleton-read "Column type: " "string"))

  (define-skeleton ted-rails-migrate-remove-column
    "Skeleton for adding a column in a rails migration."
    "Table name: "
    > "remove_column \"" str
    "\", \"" (skeleton-read "Column name: ") "\"")

  ;; Install key bindings for running an inferior Ruby in `ruby-mode'.
  (when (locate-library "inf-ruby")
    (autoload 'run-ruby "inf-ruby" nil t)
    (autoload 'inf-ruby-keys "inf-ruby" nil)
    (add-hook 'ruby-mode-hook 'inf-ruby-keys)))

(when (locate-library "php-mode")
  (define-skeleton ted-php-smarty-assign-skeleton
    "$smarty->assign skeleton."
    "Variable: "
    > "$smarty->assign(\"" str "\", $" str ");") ; $

  (define-skeleton ted-php-cgi-arg-handler-skeleton
    "Skeleton for handling CGI arguments."
    "CGI parameter: "
    "if (array_key_exists('" str "', $_REQUEST)) {" \n ; $
    > "$" str " = $_REQUEST['" str "'];" \n
    > "$smarty->assign(\"" str "\", $" str ");" \n
    -2 "}")

  (defvar ted-evdb-php-includes nil
    "List of EVDB PHP include files.")

  (defun ted-evdb-php-includes ()
    (or ted-evdb-php-includes
        (setq ted-evdb-php-includes
              (directory-files "~/dev/trunk/evdb/site/include"
                               nil "[.]php\\'"))))

  (define-skeleton ted-php-require_once-skeleton
    "EVDB require_once skeleton"
    (completing-read "Include: " (ted-alist (ted-evdb-php-includes)))
    "require_once '" str "';\n")

  (define-skeleton ted-js-events-addhandler-skeleton
    "EVDB Javascript Events.addHandler() skeleton"
    "Element: "
    > "Events.addHandler(" str ", \""
    (completing-read "Event: " (ted-alist ted-html4-intrinsic-events))
    "\", " (skeleton-read "Function: ") ");" \n >)

  (defvar ted-evdb-api-methods nil
    "List of EVDB API methods.")

  (autoload 'eshell-under-windows-p "esh-util")
  (autoload 'eshell-extended-glob "em-glob")

  (defun ted-eshell-glob-broken-p ()
    "Non-nil if this version of eshell allows us to glob in other buffers.
  Some older version of eshell require you to be in the *eshell* buffer
  for its globbing functions to work."
    (with-current-buffer "*scratch*"
      (let ((default-directory "~/"))
        (equal ".e*" (eshell-extended-glob ".e*")))))

  (defun ted-evdb-api-methods-1 ()
    "Returns a list of EVDB API method names.
  Pulls the method names out of my sandbox of the EVDB codebase."
    (let ((external
           (let ((default-directory "~/dev/trunk/evdb/site/api/rest/"))
             (eshell-extended-glob "**/*.pl")))
          (internal
           (let ((default-directory "~/dev/trunk/evdb/site/api/"))
             (eshell-extended-glob "internal/**/*.pl"))))
      ;; The `eshell-extended-glob' in Emacs 21.3 breaks, so let's
      ;; try to avoid that.
      (when (and (listp external) (listp internal))
        (nconc external internal))))

  (defun ted-evdb-api-methods ()
    "Ensure that `ted-evdb-api-methods' has a value."
    (or ted-evdb-api-methods
        (setq
         ted-evdb-api-methods
         (mapcar
          (lambda (s)
            (substring s 0 -3))
          ;; Only put Emacs in *eshell* if we have to.
          (if (ted-eshell-glob-broken-p)
              (with-current-buffer (eshell)
                (ted-evdb-api-methods-1))
            (ted-evdb-api-methods-1))))))

  (defun ted-php-evdb-api-guess-var-name (method)
    (cond
     ((string-match "/\\(search\\)\\'" method)
      (match-string 1 method))
     ((string-match "\\([^/]+\\)/\\(list\\)\\'" method)
      (match-string 1 method))
     ((string-match "\\([^/]+\\)s/\\(get\\|new\\)\\'" method)
      (match-string 1 method))
     (t "result")))

  (define-skeleton ted-php-evdb-api-skeleton
    "EVDB API call skeleton."
    (completing-read "API method: " (ted-alist (ted-evdb-api-methods)))
    `(progn
      (setq v1 (skeleton-read "Result variable: "
                              (ted-php-evdb-api-guess-var-name ,str)))
      "")
    > "$" v1 " = $api->call(\"" str "\", array("
    ("Key: " "\"" str "\" => " (skeleton-read "Value: " "1") ", ")
    & -2
    "));" \n
    > "check_api_error($" v1 ", \"" str "\");") ; $
  (autoload 'php-mode "php-mode" "PHP editing mode" t)
  (add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . php-mode))
  (define-skeleton ted-php-new-file-skeleton
    "New PHP file skeleton."
    nil
    "<?php" \n \n _ \n \n
    "?>")

  (when (fboundp 'auto-insert-mode)
    (add-to-list 'auto-insert-alist
                 `(("\\.php\\'" . "PHP") . ted-php-new-file-skeleton)))

  (add-to-list 'auto-mode-alist '("wp-content/themes/.*/.*\\.php\\'" . html-mode)))

(when (locate-library "cperl-mode")
  (autoload 'cperl-mode "cperl-mode" nil t)
  (add-to-list 'auto-mode-alist
               '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
  (fset 'perl-mode 'cperl-mode)
  (add-hook 'cperl-mode-hook 'turn-off-auto-fill)
  (setq cperl-hairy t))

(unless (locate-library "python")
  (when (locate-library "python-mode")
    (autoload 'python-mode "python-mode" nil t)
    (add-to-list 'auto-mode-alist
                 '("\\.\\(py\\|tac\\)\\'" . python-mode))
    (add-to-list 'interpreter-mode-alist
                 '("python" . python-mode))))

(when (featurep 'emacs)
  (when (locate-library "tiger")
    (autoload 'tiger-mode "tiger" nil t)
    (add-to-list 'auto-mode-alist '("\\.tig\\'" . tiger-mode))))

(add-to-list 'auto-mode-alist '("\\.view\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.psql\\'" . sql-mode))

(when (locate-library "maxima")
  (autoload 'maxima "maxima" nil t)
  (add-to-list 'auto-mode-alist '("\\.max\\'" . maxima))
  (autoload 'maxima-mode "maxima" nil t))

(when (locate-library "imaxima")
  (autoload 'imaxima "imaxima" nil t)
  (setq imaxima-pt-size 12
        imaxima-fnt-size "Huge"
        imaxima-image-type 'ps
        imaxima-use-maxima-mode-flag (locate-library "maxima")))

(when (require 'tex-site nil t)
  (setq-default TeX-auto-untabify nil)
  (setq TeX-auto-untabify nil))

(eval-after-load "tex-mode"
  '(defun tex-font-lock-suscript (pos)
     '(face default)))

(when (locate-library "table")
  (autoload 'table-insert "table" nil t)
  (global-set-key (kbd "C-c t") 'table-insert))

(when (and (not (fboundp 'org-mode))
           (locate-library "org"))
  (autoload 'org-mode "org" nil t))

(when (fboundp 'org-mode)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (global-set-key (kbd "C-c o l") 'org-store-link)
  (global-set-key (kbd "C-c o a") 'org-agenda))

(when (locate-library "noweb-mode")
  (autoload 'noweb-mode "noweb-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.nw\\'" . noweb-mode))
  (setq noweb-mode-prefix (kbd "<f5>")) ;%

  (defadvice noweb-update-chunk-vector (around ted-noweb-redisplay activate)
    (let ((inhibit-redisplay t)
          (ted-noweb-redisplay-message
           "Updating noweb's chunk vector"))
      (message "%s..." ted-noweb-redisplay-message)
      ad-do-it
      (message "%s...done" ted-noweb-redisplay-message)))

  (eval-after-load "noweb-mode"
    '(fset 'noweb-fill-paragraph-chunk 'fill-paragraph))

  (when (fboundp 'font-lock-add-keywords)
    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (when (and (boundp 'noweb-mode) noweb-mode)
                  (font-lock-add-keywords
                   nil
                   '(("\\([<][<][^>]*[>][>]\\)" ;%
                      (1 font-lock-string-face))))))))

  (setq-default noweb-default-code-mode 'emacs-lisp-mode
                noweb-code-mode         'emacs-lisp-mode)

  (when (featurep 'xemacs)
    (unless (fboundp 'ess-write-to-dribble-buffer)
      (defalias 'ess-write-to-dribble-buffer 'ignore))

    (add-hook 'noweb-mode-hook
              (lambda ()
                (require 'noweb-font-lock-mode)
                (noweb-font-lock-mode 1)))))

;;; Customizations which are specific to using Emacs under the various
;;; windowing systems.

(setq default-frame-alist '())

(defun ted-frob-xterm (frame)
  (when (and (ted-xterm-p) (require 'xterm-frobs nil t))
    (mapc
     (lambda (pair)
       (let ((key (car pair))
             (value (cdr pair)))
         (cond
          ((eq key 'foreground-color) (xterm-set-background-color value))
          ((eq key 'background-color) (xterm-set-foreground-color value))
          ((eq key 'mouse-color) (xterm-set-mouse-foreground-color value))
          ((eq key 'cursor-color) (xterm-set-cursor-color value)))))
     default-frame-alist)))

(add-hook 'after-make-frame-functions 'ted-frob-xterm)

(defvar ted-font (if (ted-tty-p) nil 'unknown)
  "The font Emacs should use.")

(setq mac-allow-anti-aliasing t)

(defun ted-configure-font ()
  "Set up which font I should use.
Should be called from `after-init-hook'."
  (setq ted-font
        (or (when (eq window-system 'ns)
	      "Bitstream Vera Sans Mono")
            (find-if
	     (lambda (font)
	       (or (ignore-errors (ted-list-fonts font))
	           (eq (ignore-errors (ted-font-sizes font)) ted-ttf-sizes)))
	     '("-*-bitstream vera sans mono-medium-r-*-*-14-*-*-*-*-*-*-*"
	       "-*-andale mono-medium-r-*--14-*-*-*-*-*-*-*"
	       "-*-screen-medium-r-*-*-14-*-*-*-m-*-*-*"
	       "-*-Lucida Console-normal-r-*-*-14-*-*-*-c-*-iso8859-1"))
            ;; An acceptable default that works everywhere.
            "-adobe-courier-medium-r-*-*-14-*-*-*-*-*-*-*"))
  (ted-set-font ted-font)
  (add-to-list 'default-frame-alist (cons 'font ted-font)))

(unless (ted-tty-p)
  (add-hook 'after-init-hook 'ted-configure-font))
(defconst ted-ttf-sizes
  (let (sizes)
    (dotimes (n 16)
      (push (+ n 8) sizes))
    (nreverse sizes))
  "Sizes to use for TrueType fonts.
Range from 8 to 24.")

(defun ted-font-sizes (&optional font)
  "Retuns a list of valid sizes of FONT (`ted-font' by default)."
  (setq font (or font ted-font))
  (let* ((xlfd (x-decompose-font-name font))
         (family (aref xlfd xlfd-regexp-family-subnum))
         ;; Like `font', but with pixel size unspecified.
         (spec (progn
                 (aset xlfd xlfd-regexp-pixelsize-subnum "*")
                 (x-compose-font-name xlfd)))
         ;; Find list of valid font sizes given spec
         (sizes (delete-duplicates
                 (mapcar (lambda (spec)
                           (string-to-number
                            (aref (x-decompose-font-name spec)
                                  xlfd-regexp-pixelsize-subnum)))
                         (ted-list-fonts spec)))))
    (and (find-if (lambda (spec)
                    (string-equal (car spec) family))
                  (x-font-family-list))
         (if (or (null sizes)
                 (and (= (car sizes) 0)
                      (eq (cdr sizes) nil)))
             ;; Questionable heuristic: I think we're looking at a
             ;; TrueType font, so let's return a canned response with
             ;; nice sizes.
             ted-ttf-sizes
           sizes))))

(when (fboundp 'x-decompose-font-name)
  (defun ted-font-size ()
    "Returns the size of the currently-used font, as an integer."
    (let* ((frame-font (cdr (assq 'font (frame-parameters))))
           (font (when frame-font
                   (x-decompose-font-name frame-font))))
      (when font
        (read (aref font 6)))))

  (defun ted-font-set-size (size)
    "Set the current font to `ted-font' at SIZE."
    (let ((font (when ted-font
                  (x-decompose-font-name ted-font))))
      (when font
        (aset font 6 (format "%d" size))
        (ted-set-font (x-compose-font-name font)))))

  (defmacro save-ted-font-size (&rest body)
    "Save the font size; execute BODY; restore the font size.
Executes BODY just like `progn'."
    (let ((fontvar (make-symbol "fontsize")))
      `(let ((,fontvar (ted-font-size)))
         ,@body
         (ted-font-set-size ,fontvar))))
  (put 'save-ted-font-size 'lisp-indent-function 0)

  (defmacro with-ted-font-size (size &rest body)
    "Execute the forms in BODY with font size SIZE
The value returned is the value of the last form in BODY."
    `(save-ted-font-size
       (ted-font-set-size ,size)
       ,@body))
  (put 'with-ted-font-size 'lisp-indent-function 1))

(when (fboundp 'ted-font-size)
  (defun ted-font-alter-size-with-spec (spec)
    "Set the current font size to be the size following it in SPEC."
    (let ((next (cadr (memq (ted-font-size) spec))))
      (when next
        (ted-font-set-size next)))))

(defvar ted-font-size 18)

(when (fboundp 'x-find-larger-font)
  (defun ted-font-increase-size (&optional frame)
    "Increase the current font size by one step."
    (interactive)
    (let ((next (cadr (memq ted-font-size (ted-font-sizes)))))
      (when next
        (setq ted-font-size next)
        (font-menu-set-font nil nil next)))))

(when (fboundp 'x-find-smaller-font)
  (defun ted-font-decrease-size (&optional frame)
    "Decrease the current font size by one step."
    (interactive)
    (let ((next (cadr (memq ted-font-size (reverse (ted-font-sizes))))))
      (when next
        (setq ted-font-size next)
        (font-menu-set-font nil nil next)))))

(when (fboundp 'ted-font-alter-size-with-spec)
  (defun ted-font-increase-size ()
    "Increase the current font size by one step.
The steps are generated by `ted-font-sizes'."
    (interactive)
    (ted-font-alter-size-with-spec (ted-font-sizes)))

  (defun ted-font-decrease-size ()
    "Decrease the current font size by one step.
The steps are generated by `ted-font-sizes'."
    (interactive)
    (ted-font-alter-size-with-spec (reverse (ted-font-sizes)))))

(when (fboundp 'ted-font-increase-size)
  (mapc (lambda (key)
          (global-set-key key 'ted-font-increase-size))
        (list (kbd "C-=") (kbd "M-=")
              (kbd "C-+") (kbd "M-+"))))

(when (fboundp 'ted-font-decrease-size)
  (global-set-key (kbd "C--") 'ted-font-decrease-size)
  (global-set-key (kbd "M--") 'ted-font-decrease-size))

(when (and (display-graphic-p)
           (not (member "--fullscreen" command-line-args)))
  (add-to-list 'default-frame-alist '(width . 82))
  (add-to-list 'default-frame-alist '(height . 43))
  (when (and (eq system-type 'darwin) (featurep 'emacs))
    (add-to-list 'default-frame-alist '(top . 0))))

(when (featurep 'emacs)
  (add-to-list 'default-frame-alist
               '(wait-for-wm . nil))
  (add-to-list 'default-frame-alist
               (cons 'menu-bar-lines (ted-menu-bar-lines))))

(setq initial-frame-alist default-frame-alist)
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode (ted-menu-bar-lines)))

(when (featurep 'xemacs)
  (set-specifier menubar-visible-p nil)
  (defun menu-bar-mode ()
    (interactive)
    (set-specifier menubar-visible-p
                   (not (specifier-instance menubar-visible-p)))))

(when (display-graphic-p)
  (setq frame-title-format
        (concat (if (string-equal (user-login-name) "root")
                    "SU: "
                  "")
                "%b (" ted-emacs-name
                "@" (or (getenv "HOST") (system-name) "unknown")
                ")"))

  (cond ((featurep 'xemacs)
         (setq scrollbars-visible-p nil)
         (defun scroll-bar-mode ()
           (interactive)
           (set-specifier
            vertical-scrollbar-visible-p
            (not (specifier-instance vertical-scrollbar-visible-p))))
         (set-specifier horizontal-scrollbar-visible-p nil)
         (set-specifier vertical-scrollbar-visible-p nil))
        ((fboundp 'scroll-bar-mode)
         (scroll-bar-mode -1)))

  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (when (ted-w32-window-system-p frame)
                (scroll-bar-mode -1))))

  (cond ((featurep 'xemacs)
         (customize-set-variable 'toolbar-visible-p nil)
         (setq toolbar-visible-p nil)
         (defun tool-bar-mode ()
           (interactive)
           (set-specifier
            default-toolbar-visible-p
            (not (specifier-instance default-toolbar-visible-p))))
         (set-specifier default-toolbar-visible-p nil))
        ((fboundp 'tool-bar-mode)
         (tool-bar-mode -1)
         (add-to-list 'default-frame-alist '(tool-bar-lines . 0))))

  (when (featurep 'xemacs)
    (when (boundp 'default-gutter-visible-p)
      (set-specifier default-gutter-visible-p nil))

    (setq progress-feedback-use-echo-area t))

  (when (featurep 'tooltip)
    (setq tooltip-gud-tips-p t)))

(when (display-graphic-p)
  (setq focus-follows-mouse (eq (ted-window-system) 'x)
        mouse-autoselect-window t)

  (setq-default mouse-yank-at-point t)

  (cond ((fboundp 'mouse-wheel-mode)
         (mouse-wheel-mode 1))
        ((locate-library "mwheel")
         (unless (fboundp 'mwheel-install)
	   (autoload 'mwheel-install "mwheel" nil nil))
	 (setq mwheel-follow-mouse t)
	 (setq mwheel-scroll-amount '(4 . 1))
	 (mwheel-install))))
(when (fboundp 'one-buffer-one-frame-mode)
  (setq one-buffer-one-frame nil) ; Aquamacs is dumb.
  (one-buffer-one-frame-mode 0))
(when (featurep 'aquamacs)
  (setq aquamacs-buffer-specific-frame-themes nil
        aquamacs-mode-specific-default-themes nil
        aquamacs-auto-frame-parameters-flag   nil
        special-display-buffer-names          nil
        special-display-regexps               nil))
(when (fboundp 'smart-frame-positioning-mode)
  (smart-frame-positioning-mode 0))


;;; Customizing Emacs' colors.

(when (locate-library "face-list")
  (autoload 'customize-face-at "face-list" nil t)
  (global-set-key (kbd "C-c f c") 'customize-face-at)

  (autoload 'describe-face-at "face-list" nil t)
  (global-set-key (kbd "C-c f d") 'describe-face-at))

(when (display-color-p)
  (when (locate-library "htmlize")
    (autoload 'htmlize-buffer "htmlize" nil t))

  (when (require 'color-theme nil t)
    (require 'color-theme-hober2 nil t)
    (let ((theme (find-if 'fboundp '(color-theme-hober2 color-theme-resolve color-theme-hober))))
      (when theme
        (add-hook 'emacs-startup-hook theme))))

  (setq ansi-term-color-vector
    [unspecified "gray4" "orange red"
     "sea green" "sandy brown" "dark slate blue"
     "pale violet red" "cadet blue" "gray"]) ;%

  (setq font-lock-maximum-size most-positive-fixnum)
  (add-hook 'after-init-hook
            (cond ((featurep 'xemacs)
                   (require 'font-lock)
		   (setq font-lock-auto-fontify t)
                   (setq font-lock-support-mode 'lazy-lock-mode))
                  ((fboundp 'global-font-lock-mode)
                   (lambda ()
                     (global-font-lock-mode 1)))
                  ((fboundp 'toggle-global-lazy-font-lock-mode)
                   (lambda ()
                     (toggle-global-lazy-font-lock-mode)))))

  (when (featurep 'xemacs)
    (mapc (lambda (hook) (add-hook hook (lambda () (font-lock-mode 1))))
          '(lisp-interaction-mode-hook ielm-mode-hook latex-mode-hook)))

  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))

;;; Host-specific customizations.

(load ted-local-elisp-config t)

(setq custom-unlispify-tag-names       nil
      custom-unlispify-menu-entries    nil
      custom-unlispify-remove-prefixes nil)

(setq custom-file
      (expand-file-name (concat ted-emacs-name "-custom.el")
                        ted-elisp-dir))
(load custom-file t)

;;; .emacs ends here
