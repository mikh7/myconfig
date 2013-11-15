;; let's define 'require-if-available, so we won't fail if the
;; package is not there...
;; (from some japanese website with too long url...)

(require 'server)

(defmacro log-sexp (&rest exprs)
  (let* ((first t)
         (format
          (with-output-to-string
            (dolist (e exprs)
              (if (not first) (princ " ") (setq first nil))
              (princ e)
              (princ "=")
              (princ "%S")))))
    `(message ,format ,@exprs)))

(defalias 'log-expr 'log-sexp)

(defun mm/one-window-p (&optional nomini all-frames)
  "Return non-nil if the selected window is the only window.
Optional arg NOMINI non-nil means don't count the minibuffer
even if it is active.  Otherwise, the minibuffer is counted
when it is active.

The optional arg ALL-FRAMES t means count windows on all frames.
If it is `visible', count windows on all visible frames.
ALL-FRAMES nil or omitted means count only the selected frame,
plus the minibuffer it uses (which may be on another frame).
ALL-FRAMES 0 means count all windows in all visible or iconified frames.
If ALL-FRAMES is anything else, count only the selected frame."
  (let ((base-window (selected-window)))
    (if (and nomini (eq base-window (minibuffer-window)))
	(setq base-window (next-window base-window)))
    (eq base-window
	(next-window base-window (if nomini 'arg) all-frames))))

(defun require-if-available (&rest args)
  "require symbols, load-library strings, fail silently if 
   some aren't available"
  (let (lib)
    (condition-case err
        (mapc (lambda (e)
                (setq lib e)
                (message "Loading %S" lib)
                (cond
                 ((stringp e) (load-library e))
                 ((symbolp e) (require e)))) args)
      (quit (progn (message "Error while loading extension %S: %S"
                            lib err) nil))
      (error  (progn (message "Error while loading extension %S: %S"
                              lib err) nil)))))


(require 'view)
(require 'help-mode)

(defun frame-buffer-list ()
  "Return buffer list on selected frame"
  (let ((buffers '()))
    (dolist (window (window-list nil 'no-minibuff) buffers)
      (push (window-buffer window) buffers))))

;;; Allow buffers to have a say how they are displayed
(defvar local-display-buffer-function nil
  "Buffer local equivalent `display-buffer-function'. ")

(make-variable-buffer-local 'local-display-buffer-function)

(defun my-display-buffer-function (buffer &optional not-this-window)
  (let ((function (with-current-buffer buffer local-display-buffer-function)))
    (if function (funcall function buffer not-this-window)
      (let ((display-buffer-function nil))
        (display-buffer buffer not-this-window frame)))))

(setq display-buffer-function 'my-display-buffer-function)

;;;
;;; fix customize overwriting my ~/.emacs symlink
;;;
(when (require-if-available 'cus-edit)
  (defun custom-save-all ()
    "Save all customizations in `custom-file'."
    (let* ((filename (custom-file))
           (recentf-exclude (if recentf-mode
                                (cons (concat "\\`"
                                              (regexp-quote (custom-file))
                                              "\\'")
                                      recentf-exclude)))
           (old-buffer (find-buffer-visiting filename)))
      (with-current-buffer (or old-buffer (find-file-noselect filename))
        (let ((inhibit-read-only t))
          (custom-save-variables)
          (custom-save-faces))
        (let ((file-precious-flag nil)
              (backup-by-copying-when-linked t))
          (save-buffer))
        (unless old-buffer
          (kill-buffer (current-buffer)))))))

(defvar my-ctl-x-map (make-sparse-keymap)
  "Replacement \C-x keymap")

;; these two keys I want to use myself
(define-key global-map [C-l] nil)
(define-key global-map [C-h] nil)
(setq-default indent-tabs-mode nil)    ;; Tabs as spaces
(setq-default tab-always-indent t)
;; need this here because it barfs on null help-char
;; (require-if-available 'ehelp)
(setq-default help-char 0)      ;; Allow using Ctrl-H as normal key
(setq help-char 0)                ;; Allow using Ctrl-H as normal key
(setq help-event-list '(f1))
(setq interprogram-cut-function 'x-select-text)
(setq x-select-enable-clipboard t)

;; ok someone keeps resetting this shit
(setq auto-hscroll-mode t)
(setq-default auto-hscroll-mode t)

;;; dont want menu bar on TTY

(setq window-system-default-frame-alist
       '(
         ;; (x (menu-bar-lines . 0) (tool-bar-lines . 0))
         (x (tool-bar-lines . 0))
         (nil (menu-bar-lines . 0) (tool-bar-lines . 0))))

(setq inhibit-splash-screen t)
;;
;; terminal fixes
;;

(setenv "TT" "/bars/lala")

;;; myconfig/elisp/site-lisp contains subdirs that will automatically go to load path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((my-lisp-dir "~/myconfig/elisp/site-lisp")
         (default-directory my-lisp-dir))
    (setq load-path (cons my-lisp-dir load-path))
    (normal-top-level-add-subdirs-to-load-path)))
;;; myconfig/elisp subdirs won't be added to the load path automatically
(add-to-list 'load-path "~/myconfig/elisp")

(require-if-available 'elisp-binary-locations)

(global-set-key (kbd "C-m") 'newline-and-indent)
(global-set-key (kbd "M-9") 'insert-parentheses)
(global-set-key [C-prior] 'scroll-other-window-down)
(global-set-key [C-next] 'scroll-other-window)
(global-set-key (kbd "M-v") 'scroll-other-window)
(global-set-key (kbd "M-n") 'scroll-other-window-down)
(global-set-key (kbd "M-V") 'scroll-other-window-down)

;; dont want emacs stalled under heavy io
(setq write-region-inhibit-fsync t)

;; keys
;; Up Down Right Left
;; Insert Delete Home End PgUp PgDown
;;
;; Combinations: 
;;
;; Alt-Shift sequences
;; also
;;   should be possible to generate XTerm translations from the above
;; Keys:
;;  left/right/up/down
;;  insert/del/home/end/pgup/pgdown
;;  tab
;;  f1..f12
;;  print-screen, scroll-lock


;;(kbd "<delete>")
;;(define-key local-function-key-map "\eO" nil)
;;(defun define-tty-key-with-modifiers (normal c s m c-s c-m s-m c-s-m )
;;  )
;;
;;(define-tty-key-with-modifiers insert "\e[1~" "\e[1;2H" 3)



(unless (fboundp 'window-system)
  (defun window-system ()
    nil))



(defun mygetenv (var &optional frame)
  (if (>= emacs-major-version 23)
      (getenv var frame)
    (getenv var)))

(defun get-input-decode-map ()
  (cond ((boundp 'input-decode-map) input-decode-map) ;; emacs 23
        ((boundp 'local-function-key-map) local-function-key-map) ;; emacs 22
        (t function-key-map)))  ;; emacs 21

(defun get-local-function-key-map ()
  (cond ((boundp 'local-function-key-map) local-function-key-map) ;; emacs 22
        (t function-key-map)))

(defun change-modifier (escape-seq 2nd-to-last-char)
  (let ((len (length escape-seq)))
    (concat (substring escape-seq 0 (- len 2))
            (vector 2nd-to-last-char)
            (vector (aref escape-seq (1- len))))))

(defun my-define-tty-key (key command)
  (define-key (get-input-decode-map) key command))

(defun define-tty-key-with-modifiers (key normal shift)
  (let ((s-key (intern (format "S-%s" key)))
        (m-key (intern (format "M-%s" key)))
        (s-m-key (intern (format "S-M-%s" key)))
        (c-key (intern (format "C-%s" key)))
        (c-m-key (intern (format "C-M-%s" key)))
        (c-s-key (intern (format "C-S-%s" key)))
        (c-s-m-key (intern (format "C-S-M-%s" key))))
    (my-define-tty-key normal `[,key])
    (my-define-tty-key (change-modifier shift ?2) `[,s-key])
    (my-define-tty-key (change-modifier shift ?3) `[,m-key])
    (my-define-tty-key (change-modifier shift ?4) `[,s-m-key])
    (my-define-tty-key (change-modifier shift ?5) `[,c-key])
    (my-define-tty-key (change-modifier shift ?6) `[,c-s-key])
    (my-define-tty-key (change-modifier shift ?7) `[,c-m-key])
    (my-define-tty-key (change-modifier shift ?8) `[,c-s-m-key])))

(defun fix-keys ()
  (unless 
      (or
       ;; (>= emacs-major-version 23)
       (member (window-system) '(x)))
    ;; make shift tab [S-tab] instead of [backtab] rebinding of one
    ;; key to another need to be done in local-function-keymap
    (define-key (get-local-function-key-map) [backtab] [S-tab])
    ;; arrow keys
    (define-tty-key-with-modifiers 'up    "\e[A" "\e[1;?A")
    (define-tty-key-with-modifiers 'down  "\e[B" "\e[1;?B")
    (define-tty-key-with-modifiers 'right "\e[C" "\e[1;?C")
    (define-tty-key-with-modifiers 'left  "\e[D" "\e[1;?D")


    (define-tty-key-with-modifiers 'insert  "\e[2~" "\e[2;?~")
    ;; delete is tricky. The regular Delete key should be deletechar
    ;; but the modified key should be S-delete M-delete etc
    (define-tty-key-with-modifiers 'delete  "\e[3~" "\e[3;?~")
    (my-define-tty-key "\e[3~" [deletechar])
    ;; home and end need 2 variants, one is what xterm uses and one is what 
    ;; screen uses
    (define-tty-key-with-modifiers 'home  "\e[H" "\e[1;?H")
    (define-tty-key-with-modifiers 'end  "\e[F" "\e[1;?F")
    (define-tty-key-with-modifiers 'home  "\e[1~" "\e[1;?H")
    (define-tty-key-with-modifiers 'end  "\e[4~" "\e[1;?F")
    (define-tty-key-with-modifiers 'prior  "\e[5~" "\e[5;?~")
    (define-tty-key-with-modifiers 'next  "\e[6~" "\e[6;?~")
    ;; old style xterm function keys (so that they do not start with ESC O
    (define-tty-key-with-modifiers 'f1 "\e[11~" "\e[11;2~")
    (define-tty-key-with-modifiers 'f2 "\e[12~" "\e[12;2~")
    (define-tty-key-with-modifiers 'f3 "\e[13~" "\e[13;2~")
    (define-tty-key-with-modifiers 'f4 "\e[14~" "\e[14;2~")
    (define-tty-key-with-modifiers 'f5 "\e[15~" "\e[15;2~")
    (define-tty-key-with-modifiers 'f6 "\e[17~" "\e[17;2~")
    (define-tty-key-with-modifiers 'f7 "\e[18~" "\e[18;2~")
    (define-tty-key-with-modifiers 'f8 "\e[19~" "\e[19;2~")
    (define-tty-key-with-modifiers 'f9 "\e[20~" "\e[20;2~")
    (define-tty-key-with-modifiers 'f10 "\e[21~" "\e[21;2~")
    (define-tty-key-with-modifiers 'f11 "\e[23~" "\e[23;2~")
    (define-tty-key-with-modifiers 'f12 "\e[24~" "\e[24;2~")
    ;; ESC O really sucks in VI mode, therefore switch App keys mode off
    ;; and delete that map
    (send-string-to-terminal "\e[?1l")
    (define-key (get-input-decode-map) "\eO" nil)
    (cond ((fboundp 'set-input-meta-mode)
           (set-input-meta-mode t nil))
          (t (set-input-mode t nil t)))))

;; instead we redefine evil-esc not to use sit-for
;; (define-key (current-global-map) (kbd "ESC") nil)
;; (define-key (current-global-map) (kbd "M-x") 'execute-extended-command)

(load (locate-library "term/xterm"))

(let ((map xterm-function-map))
  ;; add new style xterm bindings for Ctrl-numbers etc
  (define-key map "\e[27;5;48~"  [?\C-0])
  (define-key map "\e[27;5;49~"  [?\C-1])
  (define-key map "\e[27;5;50~"  [?\C-2])
  (define-key map "\e[27;5;51~"  [?\C-3])
  (define-key map "\e[27;5;52~"  [?\C-4])
  (define-key map "\e[27;5;53~"  [?\C-5])
  (define-key map "\e[27;5;54~"  [?\C-6])
  (define-key map "\e[27;5;55~"  [?\C-7])
  (define-key map "\e[27;5;56~"  [?\C-8])
  (define-key map "\e[27;5;57~"  [?\C-9]))

;; TODO get to work with modifyOtherKeys xterm thing
(when (>= emacs-major-version 23)
  (defun terminal-init-xterm ()
    "Terminal initialization function for xterm."
    ;; rxvt terminals sometimes set the TERM variable to "xterm", but
    ;; rxvt's keybindings are incompatible with xterm's. It is
    ;; better in that case to use rxvt's initializion function.
    (if (and (mygetenv "COLORTERM" (selected-frame))
             (string-match "\\`rxvt" (mygetenv "COLORTERM" (selected-frame))))
        (tty-run-terminal-initialization (selected-frame) "rxvt")
      (let ((map (copy-keymap xterm-alternatives-map)))
        (set-keymap-parent map (keymap-parent local-function-key-map))
        (set-keymap-parent local-function-key-map map))
      (let ((map (copy-keymap xterm-function-map)))
        ;; Use inheritance to let the main keymap override those defaults.
        ;; This way we don't override terminfo-derived settings or settings
        ;; made in the .emacs file.
        (set-keymap-parent map (keymap-parent input-decode-map))
        (set-keymap-parent input-decode-map map)))
    (xterm-register-default-colors)
    ;; This recomputes all the default faces given the colors we've just set up.
    (tty-set-up-initial-frame-faces)
    ;; Try to turn on the modifyOtherKeys feature on modern xterms.
    ;; When it is turned on many more key bindings work: things like
    ;; C-. C-, etc.
    ;; To do that we need to find out if the current terminal supports
    ;; modifyOtherKeys. At this time only xterm does.
    (run-hooks 'terminal-init-xterm-hook)))

(defun terminal-init-screen ()
  (message "Initializing screen as xterm")
  (terminal-init-xterm))

;; remove facemenu binding, as it conflics with ESC o
(define-key global-map "\M-o" nil)
    
(fix-keys) ; need this for emacs 22

(when (require-if-available 'my-tty-color)

  (add-hook 'term-setup-hook 
            (lambda ()
              (fix-keys)
	      (my-define-tty-colors)))

  (defadvice xterm-register-default-colors (around use-my-tty-colors activate)
    (when t
      (fix-keys)
      (my-define-tty-colors))))


;;;
;;; Speedup tty frame creation, cache some values
;;; per tty type
;;;
(when (and (featurep 'multi-tty)
           (= emacs-major-version 22))
  (defvar tty-color-desc-by-type-cache (make-hash-table :test #'equal))

  (defun tty-color-desc-cache-clear (&optional arg)
    (interactive)
    (setq tty-color-desc-by-type-cache
          (make-hash-table :test #'equal)))

  (tty-color-desc-cache-clear)

  (defvar tty-color-desc-needs-clear nil)

  (defadvice tty-color-clear (after reset-tty-desc-cache activate)
             (setq tty-color-desc-needs-clear t))

  (defadvice tty-color-define (after reset-tty-desc-cache activate)
             (setq tty-color-desc-needs-clear t))

  (defun tty-hash-for-tty-type (type)
    (let ((hash (gethash type tty-color-desc-by-type-cache)))
      (unless hash
        (setq hash (make-hash-table :size 513 :test #'equal))
        (puthash type hash tty-color-desc-by-type-cache))
      hash))

  ;;; XXX only for multi-tty emacs, crashes on regular one
  (defadvice tty-color-desc (around my-speedup activate)
             (when tty-color-desc-needs-clear
               (setq tty-color-desc-needs-clear nil)
               (tty-color-desc-cache-clear))
             (let* ((hash (tty-hash-for-tty-type (tty-type frame)))
                    (desc (gethash color hash)))
               (unless desc
                 (setq desc ad-do-it)
                 (puthash color desc hash))
               (setq ad-return-value desc))))

(defvar no-face-recalc nil)

(defadvice face-spec-recalc (around no-recalc-on-tty activate)
  (unless (and no-face-recalc
               (not (eq face 'default)))
    (setq ad-return-value ad-do-it)))

(defadvice tty-create-frame-with-faces (around no-recalc-on-tty activate)
  (let ((no-face-recalc t))
    (setq ad-return-value ad-do-it)))
;;;
;;; End of speedup tty creation
;;;


(require-if-available 'my-tty-track)

(setq frame-title-format
      '((:eval (or server-name invocation-name))
        " %b"))

;;; Emacs lisp mode customizations
(require 'etags)
(require 'thingatpt)

(defun my-find-tag (&optional arg)
  "Find tag or function at point"
  (interactive)
  (let* ((tag (funcall (or find-tag-default-function
                           (get major-mode 'find-tag-default-function)
                           'find-tag-default)))
         (symb (when tag (intern-soft tag))))
    (cond ((and symb (fboundp symb))
           (ring-insert find-tag-marker-ring (point-marker))
           (find-function symb))
          (symb (find-tag (symbol-name symb)))
          (tag (find-tag tag))
          (t
           (find-tag (find-tag-interactive "Find tag: "))))))

(define-key global-map "\M-." 'my-find-tag)
(define-key global-map "\M-," 'pop-tag-mark)

(require 'edebug)
(define-key edebug-mode-map [f6] 'edebug-next-mode)

(defun mgm-after-emacs-lisp-mode ()
  (when (fboundp 'paredit-mode)
    (paredit-mode))
  (when (fboundp 'eldoc-mode)
    (eldoc-mode))
  (when (fboundp 'paredit-magic-mode)
    (paredit-magic-mode)))

(define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-defun)
(define-key lisp-interaction-mode-map "\C-c\C-c" 'eval-defun)

(defun mgm-after-lisp-interaction-mode ()
  (when (fboundp 'paredit-mode)
    (paredit-mode))
  (when (fboundp 'eldoc-mode)
    (eldoc-mode))
  (when (fboundp 'paredit-magic-mode)
    (paredit-magic-mode 1)))

(unless (fboundp 'ignore-errors)
  (defmacro ignore-errors (body)
    (condition-case err
                    (progn ,@body)
      (error nil))))

;;;; w3m setup
(ignore-errors
  (when (file-accessible-directory-p "/usr/share/emacs/site-lisp/w3m")
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
    (setq w3m-icon-directory "/usr/share/emacs/site-lisp/w3m")
    (setq w3m-use-cookies t)
    (setq w3m-display-inline-image t)
    (setq w3m-use-mule-ucs t)
    (setq w3m-key-binding 'info)
    (setq w3m-goto-article-function 'browse-url-default-browser)
    (require-if-available 'w3m 'mime-w3m)))

;;; CUA mode setup
(when (= emacs-major-version 21)
  (require-if-available 'cua)
  (CUA-mode t))
(when (>= emacs-major-version 22)
  (cua-mode t))

;; Remove C-Return keybinding
(define-key cua-global-keymap cua-rectangle-mark-key nil)
;; as well as M-v
(define-key cua--cua-keys-keymap [(meta v)] nil)
;;;
;;; My custom font-lock settings
;;;
(defface font-lock-storage-keyword-face
    '((((class color))
            (:bold t :foreground "seagreen")))
      "Face used for the header."
        :group 'font-lock-faces)

(defvar font-lock-storage-keyword-face 'font-lock-storage-keyword-face)

;; Highlight java keywords more like vim
 (font-lock-add-keywords 'java-mode
  '(
    ;;("\\<\\(FIXME\\):" 1 font-lock-warning-face prepend)
    ("\\<\\(package\\|import\\)\\>" . font-lock-builtin-face)
    ("\\<\\(synchronized\\|final\\|private\\|public\\|protected\\|static\\)\\>" . font-lock-storage-keyword-face)))
;;;
;;; End of my custom font lock settings
;;;

;(max-color-theme)
;(color-theme-vim-colors)


(require 'hexrgb)
(require 'my-icicles-setup)

;; lua mode
(require-if-available 'lua-mode)

;; php mode
(require-if-available 'php-mode)

;; fix INFO mode mouse wheel scrolling
(require 'info)
(require 'help)
(require 'mwheel)
(require 'speedbar)

(define-key Info-mode-map [mouse-4] 'mwheel-scroll)
(define-key Info-mode-map [mouse-5] 'mwheel-scroll)
(define-key Info-mode-map "\M-," 'Info-history-back)
(define-key help-mode-map "\M-," 'help-go-back)

(require-if-available 'newpaste)

;;;;
;;;; EVIL setup
;;;;
;;;; We assume that viper is always available in our Emacs
;;;;

(unless (fboundp 'minibufferp)
  (defun minibufferp (buffer)
    "Return t if buffer is a minibuffer"
    (string-match "Minibuf" (buffer-name buffer))))


(require 'evil)

(defvar evil-Zopen-map (make-sparse-keymap)
  "Keymap for ZO command in Evil mode")
(defvar evil-z-map (make-sparse-keymap)
  "Keymap for z key")
(defvar evil-S-visual-map (make-sparse-keymap)
  "Keymap for S command in Evil visual mode")
(defvar evil-s-map (make-sparse-keymap)
  "Keymap for s dispatch command in Evil mode")
(defvar evil-toggle-map (make-sparse-keymap)
  "Keymap for toggling various things command in Evil mode")

(setq
 evil-normal-state-cursor '("black") 
 evil-motion-state-cursor '("black")
 evil-visual-state-cursor '("darkred")
 evil-insert-state-cursor '((bar . 2) "black")
 evil-replace-state-cursor '((hbar . 3) "black")
 evil-emacs-state-cursor '"Magenta")

(defun my-exec-key-in-emacs (&optional prefix)
  "Execute last command key in emacs state"
  (interactive "P")
  (let ((key last-command-event)
        com) 
    (setq
     unread-command-events
     (let ((new-events
            (cond ((eventp key) (list key))
                  ((listp key) key)
                  ((sequencep key)
                   (listify-key-sequence key))
                  (t (error
                      "exec-key-in-emacs: Invalid argument, %S"
                      key)))))
       (if (not (eventp nil))
           (setq new-events (delq nil new-events)))
       (append new-events unread-command-events)))
    (condition-case nil
        (evil-with-state emacs 
          (setq com
                (key-binding (setq key (read-key-sequence nil))))
          (while (vectorp com) (setq com (key-binding com)))
          ;; this-command, last-command-char, last-command-event
         (setq this-command com)
         (setq last-command-event key)
          (when (commandp com)
            (setq prefix-arg (or prefix-arg prefix))
            (command-execute com)))
      (quit (ding))
      (error (beep 1)))))

(when (require-if-available 'undo-tree)
  ;; Undo its own bindings, I use u and C-a-z 
  (define-key undo-tree-map (kbd "C-/") nil)
  (define-key undo-tree-map "\C-_" nil)
  (define-key undo-tree-map (kbd "C-?") nil)
  (define-key undo-tree-map (kbd "M-_") nil)
  ;; set my own
  (global-set-key "\C-z" 'undo-tree-undo)
  (global-set-key "\C-\M-z" 'undo-tree-redo)
  (global-undo-tree-mode t)
  (setq undo-tree-mode-lighter nil))

(global-set-key (kbd "C-/") nil)
(evil-define-key 'motion undo-tree-visualizer-map "q" 'undo-tree-visualizer-quit)

(define-key evil-normal-state-map ";" (make-sparse-keymap))
(define-key evil-normal-state-map "z" (make-sparse-keymap))
(define-key evil-normal-state-map "Z" nil)
(define-key evil-motion-state-map ";" (make-sparse-keymap))
(define-key evil-motion-state-map "z" (make-sparse-keymap))
(define-key evil-motion-state-map "Z" nil)
(define-key evil-normal-state-map "\M-." nil)

(define-key evil-motion-state-map "q" (make-sparse-keymap))
(define-key evil-motion-state-map "Q" 'undefined)
(define-key evil-normal-state-map "q" (make-sparse-keymap))
(define-key evil-motion-state-map "Q" 'undefined)
(define-key evil-normal-state-map "," (make-sparse-keymap))
(define-key evil-normal-state-map ";c" 'comment-dwim)
(define-key evil-visual-state-map ";c" 'comment-dwim)
(define-key evil-normal-state-map "zz" nil)
(define-key evil-normal-state-map "\C-c\C-g" nil)
(define-key evil-motion-state-map (kbd "TAB") 'my-exec-key-in-emacs)
(define-key evil-insert-state-map (kbd "TAB") 'my-exec-key-in-emacs)
(define-key evil-insert-state-map (kbd "RET") 'my-exec-key-in-emacs)
(define-key evil-motion-state-map (kbd "TAB") 'my-exec-key-in-emacs)
;; I like C-r to do search
(define-key evil-normal-state-map "\C-r" nil)
;; and C-d to be lisp thingy

(evil-mode 1)

;; fix quit

(defadvice evil-quit (around my-ex-quit-replacement activate)
  (let ((force nil)
        (kill-buffer-query-functions 
         (remove 'server-kill-buffer-query-function kill-buffer-query-functions)))
    (when bang (set-buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(defun fix-emulation-mode-map-alists-for-cua ()
  (let ((tmp (memq 'cua--keymap-alist emulation-mode-map-alists))) 
    (when (and tmp (not (eq tmp emulation-mode-map-alists))) 
      (setq emulation-mode-map-alists
            (cons 'cua--keymap-alist
                  (remq 'cua--keymap-alist
                        emulation-mode-map-alists))))))

(defadvice evil-local-mode (after fix-cua-copy-and-cut activate)
  (fix-emulation-mode-map-alists-for-cua))

;; (setq viper-vi-kbd-map (make-sparse-keymap))
;; (setq search-invisible 'open)
;; (setq vimpulse-flash-delay 5)

;; (global-set-key "\C-x" 'Control-X-prefix)
(global-set-key "\C-q" 'Control-X-prefix)

;; get rid of C-x map

(define-key ctl-x-map "e" 'eval-last-sexp)
(define-key ctl-x-map "f" 'find-file)

(dolist (key '("\C-f" "\C-e" "\C-s" "\C-c" "s" "k" "5" "4"))
  (define-key my-ctl-x-map key (lookup-key ctl-x-map key)))

(define-key ctl-x-5-map "h" (lambda ()
                              "Open new frame with name \"sw-emacs\""
                              (interactive)
                              (make-frame '((name . "emacs-small-agenda")))))

(define-key (current-global-map) (kbd "C-/") (make-sparse-keymap))
(define-key evil-normal-state-map "s" evil-s-map)
(define-key evil-motion-state-map "s" evil-s-map)
;; T is move until char backward, which I don't use anyway
(define-key evil-s-map "t" evil-toggle-map)

;; rather hten alternate meta key, use it for visual mode
(define-key evil-motion-state-map "\C-\\" 'evil-visual-block)

;; I like C-z to undo, screw switching to emacs mode
;; (define-key viper-vi-intercept-map "\C-z" nil)
;; (define-key viper-emacs-intercept-map "\C-z" nil)
; I like my C-r to stay on reverse search
;; (define-key evil-normal-state-map "\C-r" nil)
;; I never use gg
;; (define-key evil-normal-state-map "gg" 'jump-to-register)
;; also sg (for start grep)
(define-key evil-s-map "g" 'grep)
(define-key evil-s-map "p" 'describe-text-properties)
(define-key (current-global-map) (kbd "C-/ C-p") 'describe-text-properties)

(define-key evil-S-visual-map "p" 'newpaste)
(define-key evil-motion-state-map ";b" 'switch-to-buffer)

;; I like my C-w to do same thing in insert mode
(define-key evil-insert-state-map "\C-w" evil-window-map)
(global-set-key "\C-w" evil-window-map)
(when (require-if-available 'hexl)
  (define-key hexl-mode-map "\C-w" evil-window-map))
(define-key evil-window-map "T" 'transpose-windows)
;; C-r belongs to reverse search
(define-key evil-insert-state-map "\C-r" nil)


;; visual mode
(define-key evil-visual-state-map "u" 'undo-tree-undo)
;; delete these
(define-key evil-visual-state-map "s" nil)
(define-key evil-visual-state-map "S" evil-S-visual-map)

(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(define-key ctl-x-4-map (kbd "t") 'transpose-windows)
;; (define-key vimpulse-window-map "\C-s" 'transpose-windows)
;; (define-key vimpulse-window-map "t" 'transpose-windows)

;; (viper-harness-minor-mode "mime-edit")
;; (viper-harness-minor-mode "view-mode")

(defun remove-from-list (var element)
  (do ((head (symbol-value var) (cdr head))
        (prev nil head))
      ((null head) (symbol-value var))
    (when (eq (car head) element)
      (if prev (setcdr prev (cdr head))
        (set var (cdr head))))))

(require 'man)
(require 'info)
(require 'grep)
(require 'compile)

(require-if-available 'wgrep)

;; need to put "in file included from" regexp last
;; otherwise it gets borked by others after it
;; 
;; Fixes the problem with in file included from error
;; having incorrect file name
(remove-from-list 'compilation-error-regexp-alist 'gcc-include)
(add-to-list 'compilation-error-regexp-alist 'gcc-include t)

(setcdr (assoc 'gcc-include compilation-error-regexp-alist-alist)
        '("^\\(?:In file included \\|                 \\|\t\\)from \
\\([0-9]*[^0-9\n]\\(?:[^\n :]\\| [^-/\n]\\|:[^ \n]\\)*?\\):\
\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?\\(?:\\(:\\)\\|\\(,\\|$\\)\\)?"
          1 2 3 (4 . 5)))

;; (remove-hook 'grep-mode-hook 
;;           (lambda ()
;;             (hl-line-mode)))

(add-hook 'compilation-mode-hook 
          (lambda ()
            (hl-line-mode 1)))

;; force these modes to start in Evil insert mode
(dolist (mode '(erc-mode eshell-mode inferior-python-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (add-to-list 'evil-insert-state-modes mode))

;; Force going into Evil mode
(dolist (mode '(Info-mode help-mode Man-mode 
                          grep-mode  compilation-mode
                          org-agenda-mode
                          speedbar-mode
                          occur-mode
                          mime-view-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (remove-from-list 'evil-insert-state-modes mode)
  (remove-from-list 'evil-normal-state-modes mode)
  (add-to-list 'evil-motion-state-modes mode))

(dolist (mode '(Custom-mode fundamental-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (remove-from-list 'evil-insert-state-modes mode)
  (remove-from-list 'evil-motion-state-modes mode)
  (add-to-list 'evil-normal-state-modes mode))

(evil-define-key 'normal custom-mode-map "Q" 'Custom-buffer-done)

;; Prevent from going Evil mode TODO re-check this
;; (dolist (mode '(gdb-inferior-io-mode gud-mode))
;;   (remove-from-list 'evil-normal-state-modes mode)
;;   (add-to-list 'evil-emacs-state-modes mode))

(defun all-keysequences-in-keymap (keymap &optional base so-far)
  (let ((base (or base (copy-sequence [0]))))
    
    (map-keymap (lambda (a b)
                  (aset base (1- (length base)) a)
                  (push (cons (copy-sequence base) b) so-far))
                keymap)
    (nreverse so-far)))


(all-keysequences-in-keymap help-mode-map)
(all-keysequences-in-keymap view-mode-map)
(all-keysequences-in-keymap grep-mode-map)
(all-keysequences-in-keymap compilation-mode-map)

(defvar evil-give-back-keys-exception 
  `([?k] [?j] [?l] [?h] 
    ,(kbd "\C-f")
    ,(kbd "\C-b")
    [?/]
    [??]
    [?G]
    [?n]
    [?N])
  "Default list of keys to retain in the `evil-give-back-keys-in-mode'")

(defvar evil-give-back-keys-updown-only 
  `([?k] [?j] 
    ,(kbd "\C-f")
    ,(kbd "\C-b")
    [?/]
    [??]
    [?G]
    [?n]
    [?N])
  "Minimum list of keys to for
  `evil-give-back-keys-in-mode'. Only vertical movement
  commands")

(defun evil-give-back-keys-in-mode (mode &optional exception)
  "Binds all the keys that are defined to something other then
self-insert in the major mode mode to
`my-exec-key-in-emacs' Exception is a list of exceptions which
defaults to jklh C-f C-b /?GnN

If mode is a list, then the 1st element is a major mode and the
rest of the elements are the additional mode names, keybindings
in which will be used in that major mode. Please note that it
will not check if minor mode is active or not, using an
additional mode name here will use that mode bindings
unconditinally, regardless if minor mode is actually active.

Example usage would be '(help-mode view-mode).

"
  (or exception (setq exception evil-give-back-keys-exception))
  (let (;; (vi-keys (remove-duplicates
        ;;           (append 
        ;;            (all-keysequences-in-keymap evil-motion-state-map)
        ;;            (all-keysequences-in-keymap evil-normal-state-map))))
        modes bind-mode)
    (if (listp mode)
        (setq modes mode bind-mode (car mode))
      (setq modes (list mode) bind-mode mode))
    (dolist (m modes)
      (let ((map (if (keymapp m) m
                   (let ((mode-map-symb (intern (format "%s-map" m))))
                     (when (boundp mode-map-symb)
                       (symbol-value mode-map-symb)))))
            (bind-map (if (keymapp bind-mode) bind-mode
                        (let ((mode-map-symb (intern (format "%s-map" bind-mode))))
                          (when (boundp mode-map-symb)
                            (symbol-value mode-map-symb))))))
        (when map
          (loop for (c . b) in (all-keysequences-in-keymap map)
                ;; so that q gets bound because its not in vi keys
                if (and (vectorp c)
                        (characterp (elt c 0))
                        (or (commandp b) (symbolp b)
                            (keymapp b)))
                do (if (and ;; (assoc c vi-keys)
                        (not (member c exception)))
                       (progn 
                         ;; (log-expr c b)
                         (evil-define-key 'normal bind-map c b)
                         (evil-define-key 'motion bind-map c b))
                     (progn 
                       ;; (log-expr c)
                       (evil-define-key 'normal bind-map c nil) 
                       (evil-define-key 'motion bind-map c nil)))))))))

(evil-give-back-keys-in-mode '(help-mode view-mode help-mode)
                              `([?w] [?y] [?g] [?s] [?z] [?%] ,@evil-give-back-keys-exception))
(evil-give-back-keys-in-mode '(Info-mode)
                              `([?w] [?z] [?%] ,@evil-give-back-keys-exception))
(evil-give-back-keys-in-mode '(Man-mode) nil)
(evil-give-back-keys-in-mode '(grep-mode) nil)
(evil-give-back-keys-in-mode '(compilation-mode) nil)
(evil-give-back-keys-in-mode '(occur-mode))
(evil-give-back-keys-in-mode '(special-mode))

(defun my-reconfigure-speedbar-hook ()
  (ignore-errors
    (evil-give-back-keys-in-mode (list 'speedbar-mode (current-local-map)))))

(add-hook 'speedbar-reconfigure-keymaps-hook 'my-reconfigure-speedbar-hook)

;; I like my backspace just the way it is
(define-key evil-insert-state-map [backspace] 'my-exec-key-in-emacs)
(define-key evil-insert-state-map (kbd "DEL") 'my-exec-key-in-emacs)

;; simularly for del key
(define-key evil-insert-state-map (kbd "<delete>") 'my-exec-key-in-emacs)
(define-key evil-insert-state-map (kbd "<deletechar>") 'my-exec-key-in-emacs)

;; I hate it when I switch to some buffer and its
;; accidently left in the insert mode. In vim
;; buffers are always left in the command mode when
;; you switch buffers, therefore simulatio the
;; same thing in Viper
(defun mgm-reset-all-evil-buffers-to-vi-state ()
  "Reset all the buffers where evil is active VI state"
  (dolist (buffer (frame-buffer-list))
    (when (and (not (eq (selected-window)
                        (get-buffer-window buffer nil))))
      (with-current-buffer buffer
        (when (and (evil-insert-state-p) 
                   (not (member major-mode evil-insert-state-modes)))
          (setq evil-repeat-info nil
                evil-insert-vcount nil
                evil-insert-vcount nil)
          (evil-normal-state))))))

(defun my-window-configuration-change-hook ()
  (mgm-reset-all-evil-buffers-to-vi-state))

(add-hook 'window-configuration-change-hook 
          'my-window-configuration-change-hook)

;; fix x in comint derived modes at the end of line not doing
;; same thing as backspace 
;; (defadvice viper-delete-char (before fix-viper-x-at-eob activate)
;;   (when (eobp)
;;     (backward-char)))

;; Make minibuffer history behave like vim, which is 
;; put cursor to the end of line

;; (defvar viper-history-move-to-eol-didit nil)

;; (defadvice next-history-element (after viper-history-move-to-eol activate)
;;   (when (or viper-insert-minibuffer-minor-mode viper-vi-minibuffer-minor-mode)
;;     (unless (or (memq last-command '(next-history-element
;;                                       previous-history-element))
;;                 viper-history-move-to-eol-didit)
;;       (set (make-local-variable 'viper-history-move-to-eol-didit) t)
;;       (goto-char (point-max))
;;       (setq minibuffer-temporary-goal-position nil))))

;; remap C-c C-<?> keys to z?

;;
;; Global toggles
;; 
(require 'refill)

(define-key evil-toggle-map "t" 'toggle-truncate-lines)
(define-key evil-toggle-map "d" 'toggle-debug-on-error)
(define-key evil-toggle-map "h" 'hl-line-mode)
(define-key evil-toggle-map "g" 'toggle-gud-popups)
(define-key evil-toggle-map "r" 'refill-mode)
(define-key evil-toggle-map "c" 'toggle-case-fold-search)
(define-key evil-toggle-map "p" 'show-point-mode)

(define-key evil-normal-state-map ";m" 'imenu)
;; make sure C-v pastes in insert mode
(define-key evil-insert-state-map "\C-v" nil)

;; make Y key in visual mode paste
;; (defadvice viper-yank-line (around fix-Yank-with-region-active activate)
;;   (if (region-active-p)
;;       (call-interactively 'vimpulse-yank)
;;     (setq ad-return-value ad-do-it)))

(defun mm/find-file-home ()
  "Find file starting at home dir"
    (interactive)
    (let ((default-directory "~/"))
      (if icicle-mode (call-interactively 'icicle-file)
        (call-interactively 'find-file))))

(defun mm/find-file-elisp ()
  "Find file starting at ~/myconfig/elisp"
    (interactive)
    (let ((default-directory "~/myconfig/elisp/"))
      (if icicle-mode (call-interactively 'icicle-file)
        (call-interactively 'find-file))))

(define-key evil-normal-state-map "gh" 'mm/find-file-home)


(define-key evil-normal-state-map "z" evil-z-map)
(define-key evil-motion-state-map "z" evil-z-map)

(define-key evil-z-map "o" 'find-file)

(defun mm/zk ()
  "Calls quit-restore-window with 'kill argument"
  (interactive)
  (quit-restore-window nil 'kill))

(define-key evil-z-map "k" 'mm/zk)
(define-key evil-z-map "b" 'bury-buffer)

(define-key evil-normal-state-map "Z" evil-Zopen-map)
(define-key evil-motion-state-map "Z" evil-Zopen-map)

(define-key evil-Zopen-map "o" 'find-file)
(define-key evil-Zopen-map "O" 'find-file-other-window)

(define-key evil-Zopen-map "h" 'mm/find-file-home)
(define-key evil-Zopen-map "e" 'mm/find-file-elisp)
;;;
;;; End of Viper setup
;;;

(require-if-available 'my-kmacro-setup)
(require-if-available 'my-wl-setup)

;; (require 'autoinsert)
;; (auto-insert-mode nil)
;; (setq auto-insert-query nil)

(ignore-errors
  (when (require-if-available 'org)
    (require 'my-orgmode-setup)))

(ignore-errors
  (when (require-if-available 'paredit)
    (require 'my-paredit-setup)))

(when (require-if-available 'artbollocks-mode)
  (setq weasel-words-regex
        (concat "\\b" (regexp-opt
                       '("one of the"
                         "should"
                         "just"
                         "sort of"
                         "a lot"
                         "heh"
                         "man"
                         "doh"
                         "coz"
                         "probably"
                         "maybe"
                         "perhaps"
                         "I think"
                         "really"
                         "pretty"
                         "maybe"
                         "nice"
                         "action"
                         "utilize"
                         "leverage") t) "\\b"))
  ;; Fix a bug in the regular expression to catch repeated words
  (setq lexical-illusions-regex "\\b\\(\\w+\\)\\W+\\(\\1\\)\\b")
  ;; Don't show the art critic words, or at least until I figure
  ;; out my own jargon
  (setq artbollocks t)
  (defun my-turn-on-artbollocs ()
    (artbollocks-mode 1))
  (add-hook 'org-capture-mode-hook 'my-turn-on-artbollocs))

;;; Shell mode setup

(require-if-available 'my-shell-mode-setup)
(require-if-available 'my-sh-mode-setup)
;;; 
;;; E-Shell setup
;;;
(when (and
       (require-if-available 'eshell)
       (require-if-available 'em-prompt))
  ;; fix the e-shell prompt to mark its prompt as a field
  ;; that makes Home/End etc respect the prompt and makes viper
  ;; happier too
  (defun eshell-emit-prompt ()
    "Emit a prompt if eshell is being used interactively."
    (run-hooks 'eshell-before-prompt-hook)
    (if (not eshell-prompt-function)
        (set-marker eshell-last-output-end (point))
      (let ((prompt (funcall eshell-prompt-function)))
        (and eshell-highlight-prompt
             (add-text-properties 0 (length prompt)
                                  '(read-only t
                                              face eshell-prompt
                                              field 'input
                                              ;;rear-nonsticky (face read-only)
                                              rear-nonsticky t
                                              front-sticky '(read-only))
                                  prompt))
        (eshell-interactive-print prompt)))
    (run-hooks 'eshell-after-prompt-hook))

  ;; j/k in the eshell mode
  (defun viper-eshell-k (arg)
    "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
    (interactive "P")
    (if (we-are-at-last-line-p)
        (progn
          (kill-region eshell-last-output-end (point-max))
          (call-interactively 'eshell-previous-input)
          (beginning-of-line))
      (call-interactively 'evil-previous-line)))

  (defun viper-eshell-j (arg)
    "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
    (interactive "P")
    (if (we-are-at-last-line-p)
        (progn
          (kill-region eshell-last-output-end (point-max))
          (call-interactively 'eshell-next-input)
          (beginning-of-line))
      (call-interactively 'evil-next-line)))

  ;; Esc / search in eshell mode
  (defvar viper-eshell-search-idx 0)
  (defvar viper-eshell-search-regexp nil)

  (defun viper-eshell-start-search ()
    (interactive)
    (setq viper-eshell-search-regexp 
          (read-from-minibuffer "Match (regexp): " nil nil nil
                                'minibuffer-history-search-history))
    (setq viper-eshell-search-idx 0)
    (viper-eshell-search-next))

  (defun viper-eshell-search-next ()
    (interactive)
    (if (null viper-eshell-search-regexp)
        (error "")
      (eshell-previous-matching-input 
       viper-eshell-search-regexp viper-eshell-search-idx)
      (setq viper-eshell-search-idx (1+ viper-eshell-search-idx))))

  (defun viper-eshell-search-prev ()
    (interactive)
    (if (null viper-eshell-search-regexp)
        (error "")
      (eshell-previous-matching-input 
       viper-eshell-search-regexp viper-eshell-search-idx)
      (setq viper-eshell-search-idx (1- viper-eshell-search-idx))))

  ;; keys
  (evil-define-key 'normal eshell-mode-map "\C-m" 'viper-comint-enter)
  (evil-define-key 'normal eshell-mode-map "\C-m" 'my-exec-key-in-emacs)
  (evil-define-key 'normal eshell-mode-map "j" 'viper-eshell-j)
  (evil-define-key 'normal eshell-mode-map "k" 'viper-eshell-k)
  (evil-define-key 'normal eshell-mode-map "/" 'viper-eshell-start-search)
  (evil-define-key 'normal eshell-mode-map "n" 'viper-eshell-search-next)
  (evil-define-key 'normal eshell-mode-map "N" 'viper-eshell-search-prev)

  ;; global binding to switch to the eshell buffer
  )

;;;
;;; End of E-Shell setup
;;;


(ignore-errors
  (require 'my-tempo-setup))

(ignore-errors
  (require 'my-ccmode-setup))

(ignore-errors
  (require 'my-java-setup))

(ignore-errors
  (require 'my-ebrowse-setup))

;;;; switch on minor modes that I always like
;;(pc-selection-mode)
(show-paren-mode t)

;;;
;;; VC setup
;;;
(require-if-available 'my-vc-setup)

;; enable font lock globally
(cond ((fboundp 'global-font-lock-mode)
       ;; Load the font-lock package.
       (require 'font-lock)
       ;; Maximum colors
       (setq font-lock-maximum-decoration t)
       ;; Lazy Mode
       ;;(setq font-lock-support-mode 'lazy-lock-mode)
       ;; Turn on font-lock in all modes that support it
       (global-font-lock-mode t)))

(require-if-available 'my-lisp-mode-setup)

(require-if-available 'my-slime-setup)

(require-if-available 'my-irc-setup)

(defun create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;;; Buffer Cycling
(when (require-if-available 'cycle-buffer)
  (require-if-available 'my-cycle-buffer-setup))

(setq comint-input-ring-size 200000)
(setq comint-prompt-read-only t)

(require-if-available 'my-dired-setup)
(require-if-available 'zsh-wildcard)

(require-if-available 'my-python-setup)

(when (require-if-available 'cmake-mode)
  (add-to-list 'auto-mode-alist (cons "CMakeLists\\.txt\\'" 'cmake-mode))
  (add-to-list 'auto-mode-alist (cons "\\.cmake\\'" 'cmake-mode)))


;;; next error highlight customization - cancel highlight if any other
;;; command is done
(defun mgm-delete-next-error-overlay ()
  (ignore-errors 
      (when (overlay-buffer compilation-highlight-overlay)
        (delete-overlay compilation-highlight-overlay)))
  (remove-hook 'pre-command-hook 'mgm-delete-next-error-overlay))

(defun mgm-on-next-error-hook ()
  (when (and compilation-highlight-overlay
             (overlay-buffer compilation-highlight-overlay)
             (< emacs-major-version 23))
    (add-hook 'pre-command-hook 'mgm-delete-next-error-overlay))
  (with-current-buffer next-error-last-buffer
    (when (and (boundp 'hl-line-mode)
               hl-line-mode)
      (hl-line-highlight))))

(add-hook 'next-error-hook 'mgm-on-next-error-hook)

(setq next-error-highlight 2.0
      next-error-highlight-no-select 2.0)

;; 
;; Editing file without extension but with zsh in their name
;; is shell mode
(add-to-list 'auto-mode-alist (cons "zsh[^.]*$" 'shell-script-mode))
(add-to-list 'auto-mode-alist (cons "zalias$" 'shell-script-mode))
(add-to-list 'auto-mode-alist (cons "/X11.+app-override/" 'conf-xdefaults-mode))
(add-to-list 'auto-mode-alist (cons "\\.\\(ofx\\|qfx\\)$" 'nxml-mode))
(setq auto-mode-case-fold t)

(require-if-available 'my-nxml-mode-setup)

(when (featurep 'multi-tty)
  (require 'show-point-mode))

(column-number-mode t)

(require-if-available 'my-flyspell-setup)

;;; Configure comment 
(require 'newcomment)
;; (define-key global-map "\M-c" 'comment-indent)
;; (defadvice comment-indent (after viper-go-insert activate)
;;   (when (and (interactive-p) (evil-normal-state-p))
;;     (evil-insert-state 1)))

;; (define-key evil-normal-state-map ";c" 'comment-dwim)
  
;; (when (boundp 'viper-visual-line-map)
;;   (define-key viper-visual-line-map ";" nil) ;
;;   (define-key viper-visual-line-map ";c" 'comment-dwim)

;;   (defadvice comment-region (around viper-visual-region-fix activate)
;;     (if (not (viper-visual-state-p viper-current-state))
;;         (setq ad-return-value ad-do-it)
;;       (setq beg (viper-overlay-start viper-visual-overlay)
;;             end (viper-overlay-end viper-visual-overlay))
;;       (setq ad-return-value ad-do-it)))

;;   (defadvice comment-dwim (around viper-visual-region-fix activate)
;;     (if (not (viper-visual-state-p viper-current-state))
;;         (progn 
;;           (if (and mark-active transient-mark-mode)
;;               (setq ad-return-value ad-do-it)
;;             (setq ad-return-value ad-do-it)
;;             (evil-insert-state 1)))
;;       (comment-or-uncomment-region (viper-overlay-start viper-visual-overlay)
;;                                    (viper-overlay-end viper-visual-overlay))
;;       (viper-visual-end))))

;; Prevent interactive switch-to-buffer from creating new buffers, unless argument is given
;; (defun my-switch-to-buffer ()
;;   "switch to buffer, using completion to prevent bogus buffer names from
;; being given"
;;   (interactive)
;;   (call-interactively icicle-buffer nil))

(global-set-key "\C-xb" 'switch-to-buffer)

;; track the current column
(defvar default-column nil
 "The column to track in functions registered with `track-column'.
This variable is buffer local")
(make-variable-buffer-local 'default-column)

(defvar default-column-tracking-functions nil
 "List of functions which track the current column")

(defun default-column ()
 "Return the default column to track in cursor motion functions
which are advised by `track-column'"
 (if (memq last-command default-column-tracking-functions)
     (or default-column 0)
   (setq default-column (current-column))))

(defmacro track-column (function)
 "Advise FUNCTION to keep track of the default column.
All functions so advised will strive to maintain the same column."
 (add-to-list 'default-column-tracking-functions function)
 (let ((col (gensym))
       (line (gensym)))
   `(defadvice ,function (around ,(track-column-advice-name function) first
                                 activate)
      "Keep cursor in the same column."
      (let ((,col (default-column))
            (,line (line-number-at-pos)))
        ad-do-it
        ;; do not preserve column 
        (unless (eql ,line (line-number-at-pos))
          (move-to-column ,col))))))

(defun track-column-advice-name (function)
 (make-symbol (format "track-column-in-%s" function)))

;; Make subsequently opened frames offset from the first one
(defvar ewd-frame-offset 25
 "*Amount to offset each subsequently created frame")
;; (defadvice x-create-frame-with-faces (before ewd-create-frame activate)
;;  "Make subsequent frames open at an offset"
;;  (let* ((topelt (assoc 'top default-frame-alist))
;;         (leftelt (assoc 'left default-frame-alist))
;;         (top (if (null topelt)
;;                  (car (setq default-frame-alist
;;                             (cons '(top . 0) default-frame-alist)))
;;                topelt))
;;         (left (if (null leftelt)
;;                   (car (setq default-frame-alist
;;                              (cons '(left . 0) default-frame-alist)))
;;                 leftelt))
;;         (toppos (cdr top))
;;         (leftpos (cdr left)))
;;    (setcdr top (+ toppos ewd-frame-offset))
;;    (setcdr left (+ leftpos ewd-frame-offset))))

(track-column scroll-up)
(track-column scroll-down)
(track-column previous-line)
(track-column next-line)
(track-column cua-scroll-up)
(track-column cua-scroll-down)
(track-column org-metaup)
(track-column org-metadown)

(require 'saveplace)
;; fix :wq from emacsclient not recording stuff
;; no need with the fix to server.el
;; (add-hook 'server-done-hook 'save-place-to-alist)

(require-if-available 'linkd)

(ignore-errors
  (require-if-available 'my-gud-setup))

(ignore-errors
  (defadvice speedbar-frame-mode (after enable-hscroll activate)
    (setq auto-hscroll-mode t)))

(require 'compile)
(setq compilation-ask-about-save nil)

(defvar mm/last-compile-type nil)
(defvar mm/last-slime-system-name nil)

(defun mm/slime-save-some-lisp-buffers ()
  (if slime-repl-only-save-lisp-buffers
      (save-some-buffers t (lambda ()
                               (and (memq major-mode slime-lisp-modes)
                                    (not (null buffer-file-name)))))
      (save-some-buffers t)))

(defun my-compile (&optional arg)
  (interactive "P")
  (cond ((or (member major-mode '(lisp-mode slime-repl-mode slime-xref-mode))
             (and (not arg)
                  (eq mm/last-compile-type :lisp)))
         (setq mm/last-compile-type :lisp)
         (mm/slime-save-some-lisp-buffers)
         (let ((system (if (or arg (not mm/last-slime-system-name))
                           (slime-read-system-name)
                         mm/last-slime-system-name)))
           (setq mm/last-slime-system-name system)
           (slime-oos system 'load-op)))
        (t
         (setq mm/last-compile-type nil)
         (if arg
             (call-interactively 'compile)
           (compile compile-command)))))

(global-set-key [(f7)] 'my-compile)

(global-set-key "\C-cf" nil)
(global-set-key "\C-c\C-k" nil)

;; kill matching-buffers without-asking
(defun kill-matching-buffers (regexp &optional internal-too)
  "Kill buffers whose name matches the specified REGEXP.
The optional second argument indicates whether to kill internal buffers too."
  (interactive "sKill buffers matching this regular expression: \nP")
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (not (string-equal name ""))
                 (or internal-too (/= (aref name 0) ?\s))
                 (string-match regexp name))
        (kill-buffer buffer)))))

(defun kill-matching-lines (arg)
  "Kill matching lines in the buffer"
  (interactive "M")
  (let (start end line-start line-end)
    (cond ((and mark-active transient-mark-mode)
           (if (> (point) (mark)) 
               (setq start (mark) 
                     end (point))
             (setq start (point) 
                   end (mark))))
          (t (setq start (point-min) 
                   end (point-max))))
    (save-excursion
      (goto-char start)
      (beginning-of-line)
      (setq line-start (point))
      (while (not (eobp))
        (let ((line-end (save-excursion
                          (end-of-line)
                          (if (eobp) (point)
                            (1+ (point))))))
          (cond ((re-search-forward arg line-end t)
                 (delete-region line-start line-end)
                 (goto-char line-start))
                (t
                 (goto-char line-end)
                 (setq line-start line-end))))))))


(defun kill-other-terminals (arg)
  (interactive "P")
  (let ((kill-list))
    (dolist (terminal (terminal-list))
      (unless (eq terminal (selected-terminal))
        (push terminal kill-list)))
    (setq kill-list (nreverse kill-list))
    (when (y-or-n-p (format "%S Kill these terminals?" kill-list))
      (dolist (frame (frame-list))
        (when (and (member (frame-terminal frame) kill-list)
                   (frame-parameter frame 'client))
          (server-delete-client (frame-parameter frame 'client)))))))

(when (require-if-available 'increment-number)
  (define-key evil-normal-state-map "z+" 'my-increment-number-decimal)
  (define-key evil-normal-state-map "z-" 'my-decrement-number-decimal))

(ignore-errors
  (require-if-available 'my-browse-url))

;; Get rid of the minor mode descriptions
(let ((modes '(paredit-mode
               icicle-mode eldoc-mode compilation-in-progress
               flyspell-mode slime-mode egg-minor-mode)))
  (setq minor-mode-alist (remove-if (lambda (mode)
                                      (memq mode modes))
                                    minor-mode-alist :key #'car)))


(defun insert-file-name-at-point (arg)
  "Insert current file name (without the directory part) at
point. With C-u argument insert it without extension. With double
C-u argument surround it by double-quotes"
  (interactive "p")
  (let ((name (file-name-nondirectory (buffer-file-name))))
    (when (>= arg 4)
      (setq name (file-name-sans-extension name)))
    (when (>= arg 16)
      (setq name (format "%S" name)))
    (insert name)))

(require-if-available 'my-doremi-setup)

(when (require-if-available 'markdown-mode)
  (add-to-list 'auto-mode-alist (cons "\\.\\(markdown\\|md\\)\\'" 'markdown-mode)))

(defun mm/ring-xbell ()
  (if (equal window-system 'x)
      (call-process "xkbbell")
    (ding)))

;;; Make bell use XkbBell
(setq ring-bell-function 'mm/ring-xbell)

(when (require-if-available 'color-theme)
  (load "my-color-theme")
  (my-color-theme))

(require-if-available 'my-sqlmode-setup)
(require-if-available 'my-ediff-setup)

(setq special-display-regexps
      '(("^\\*.*Agenda.*\\*" (name . "emacs-small-agenda"))))

(defadvice quit-window (around mm/quit-window-kills-window activate)
  (if (and (null window)
           (not (one-window-p t)))
      (setq window (selected-window)))
  (setq ad-return-value ad-do-it))

(defun mgm-after-major-mode-hook (&optional arg)
  (let ((symbol (intern (format "mgm-after-%s" major-mode))))
    (when (fboundp symbol)
      (funcall symbol)))
  (evil-normalize-keymaps))

(add-hook 'emacs-startup-hook
          (lambda ()
            (when (fboundp 'icy-mode)
              (icy-mode))
            (add-hook 'after-change-major-mode-hook 'mgm-after-major-mode-hook)
            (kill-buffer (get-buffer "*scratch*"))
            (create-scratch-buffer)))

(when (require-if-available 'zoom-frm)
  (global-set-key (if (boundp 'mouse-wheel-down-event) ; Emacs 22+
                      (vector (list 'control mouse-wheel-down-event))
                    [C-mouse-wheel])    ; Emacs 20, 21
                  'zoom-in)
  (when (boundp 'mouse-wheel-up-event)  ; Emacs 22+
    (global-set-key (vector (list 'control mouse-wheel-up-event))
                    'zoom-out))
  
  (global-set-key [S-mouse-1]    'zoom-in)
  (global-set-key [C-S-mouse-1]  'zoom-out)
  ;; Get rid of `mouse-set-font' or `mouse-appearance-menu':
  (global-set-key [S-down-mouse-1] nil)
  
  ;; The first two of these mean that in Emacs 22 or later you can hold
  ;; the Control key and rotate the wheel to zoom in and out, just as
  ;; you do in your Web browser.  In Emacs 20 and 21, however, Control
  ;; plus wheeling zooms in, but to zoom out you need to use `C--'
  ;; before wheeling with Control.  This is because Emacs 20 and 21 do
  ;; not have separate events for the mouse wheel directions, and it is
  ;; the prefix arg, not the wheel direction, that determines the
  ;; effect.
  
  ;; Note: You can bind the zooming commands to keyboard events, as
  ;; well as to mouse events.  You could do this, for instance:
  
  (global-set-key [(control shift ?z)]  'zoom-in)  ; `C-S-z'
  (global-set-key [(control ?z)]        'zoom-out) ; `C-z'
  

  (setq zoom-frame/buffer 'buffer))

(add-to-list 'auto-mode-alist (cons "\\.pyc_dis\\'" 'python-mode))

(random t)

