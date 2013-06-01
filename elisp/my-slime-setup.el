;;
;; SLIME setup
;;
;(add-to-list 'load-path "/home/max/cvs/slime.patched")
(add-to-list 'load-path "/home/max/cvs/slime")
(add-to-list 'load-path "/home/max/cvs/slime/contrib")

;;; testing log4slime
(condition-case err
    (progn 
      (load "~/quicklisp/log4slime-setup.el")
      (global-set-key "\C-c\C-g" 'log4slime-level-selection)
      (global-log4slime-mode 1))
  (error  (progn (message "Error while loading log4cl %s" err) nil)))

(require 'slime)

(setq slime-protocol-version 'ignore)
(slime-setup '(slime-fancy
               slime-asdf slime-compiler-notes-tree
               slime-repl-pictures
               slime-indentation
               slime-sprof
               slime-snapshot))

(font-lock-remove-keywords 
    'lisp-mode slime-additional-font-lock-keywords)

(defvar my-additional-font-lock-keywords
 '(("(\\(\\(\\s_\\|\\w\\)*:\\(define-\\|do-\\|with-\\|without-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face) 
   ("(\\(\\(define-\\|do-\\|with-\\)\\(\\s_\\|\\w\\)*\\)" 1 font-lock-keyword-face)))

(font-lock-add-keywords 
    'lisp-mode my-additional-font-lock-keywords)

;; newer slime Slime have presentatinos separately
(when (require-if-available 'slime-presentations)
  (defun slime-read-object (prompt)
    "Read a Common Lisp expression from the minibuffer, providing
    defaults from the s-expression at point.  If point is within a
    presentation, don't prompt, just return the presentation."
    (multiple-value-bind (presentation start end)
        (slime-presentation-around-point (point))
      (if presentation
          (slime-presentation-expression presentation)
        (slime-read-from-minibuffer prompt
                                    (slime-sexp-at-point)))))

  (defun slime-inspect (form &optional no-reset)
    "Eval an expression and inspect the result."
    (interactive (list (slime-read-object "Inspect value (evaluated): ")))
    (slime-eval-async `(swank:init-inspector ,form)
      'slime-open-inspector))

  (define-key slime-repl-mode-map "\C-ci" 'slime-inspect)
  (define-key slime-repl-mode-map "\C-cI" 'slime-inspect))

;; (when (require-if-available 'slime-presentation-streams)
;;     ;; fix the inspect
;;     (defun slime-read-object (prompt)
;;       "Read a Common Lisp expression from the minibuffer, providing
;; defaults from the s-expression at point.  If point is within a
;; presentation, don't prompt, just return the presentation."
;;       (multiple-value-bind (presentation start end)
;;           (slime-presentation-around-point (point))
;;         (if presentation
;;             (slime-presentation-expression presentation)
;;           (slime-read-from-minibuffer prompt
;;                                       (slime-sexp-at-point)))))
;;     (defun slime-inspect (form &optional no-reset)
;;       "Eval an expression and inspect the result."
;;       (interactive (list (slime-read-object "Inspect value (evaluated): ")))
;;       (slime-eval-async `(swank:init-inspector ,form)
;;                         'slime-open-inspector)))

;; not needed after cvs updated slime
(when (require-if-available 'slime-asdf)
  (remove-hook 'slime-connected-hook 'slime-asdf-on-connect)
  (when (fboundp 'slime-asdf-on-connect)
    (add-hook 'slime-connected-hook 'slime-asdf-on-connect)))

;;(setq inferior-lisp-program "/usr/local/bin/sbcl")
;;(setq inferior-lisp-program "/home/max/cvs/sbcl/run-sbcl")
(setq inferior-lisp-program "/home/max/cvs/sbcl3/run-sbcl.sh")
;;(setq inferior-lisp-program "/usr/pkgs/cmucl/bin/lisp")
;; get CUA shift keys to work nicely with slime
(define-key slime-repl-mode-map [C-S-Home] 'slime-repl-bol)
(define-key slime-repl-mode-map [S-Home] 'slime-repl-bol)
(put 'slime-repl-bol 'CUA 'move)

;; No idea what the fuck this was for

;; (let ((tmp
;;        (find "\C-c\C-u" slime-editing-keys :key #'car 
;;              :test #'equal)))
;;   (when tmp
;;     (setq slime-editing-map nil)
;;     (setcdr tmp (list 'slime-repl-kill-input))
;;     (slime-init-keymaps)
;;     (define-minor-mode slime-editing-mode
;;       "Minor mode which makes slime-editing-map available.
;; \\{slime-editing-map}"
;;       nil
;;       nil
;;       slime-editing-map)))

;; (define-key slime-repl-mode-map [C-c C-u] 'slime-repl-kill-input)

;; configure slime-quit-lisp to simply disconnect instead
;; of quitting, if its connected to lisp started externally
;; from emacs

(defun slime-quit-lisp (&optional keep-buffers)
  "Quit lisp, kill the inferior process and associated buffers."
  (interactive)
  (let ((connection (slime-connection)))
    (when connection
      ;; Only send quit if it was our own inferior lisp,
      ;; do not send quit if it was external
      (cond ((slime-inferior-process connection)
             (slime-eval-async '(swank:quit-lisp)))
            (t (slime-net-close connection)))
      (kill-buffer (slime-output-buffer))
      (set-process-filter connection nil)
      (set-process-sentinel connection 'slime-quit-sentinel))))

(defun my-slime-switch-to-output-buffer (&optional connection)
  "Like slime-switch-to-output-buffer but does not do pop-to-buffer"
  (interactive (list (if prefix-arg (slime-choose-connection))))
  (let ((slime-dispatching-connection (or connection 
                                          slime-dispatching-connection)))
    (pop-to-buffer (slime-output-buffer))
    (goto-char (point-max))))

(global-set-key "\C-cr" 'my-slime-switch-to-output-buffer)
;; do not scroll to the bottom if point is not at the end of the buffer

(defvar slime-maximum-output-dont nil)

(defadvice slime-repl-show-maximum-output (around my-dont-show-maximum-output activate)
  (when (not slime-maximum-output-dont)
    (goto-char (point-max))
    (setq ad-return-value ad-do-it)))

(defun point-visible-in-buffer (buffer point)
  (let ((windows (get-buffer-window-list buffer)))
    (and (= 1 (length windows))
         (with-selected-window (car windows)
           (pos-visible-in-window-p point)))))

(defadvice slime-repl-insert-result (around my-stop-slime-scroll activate)
  (with-current-buffer (slime-output-buffer)
    (let ((visible (point-visible-in-buffer (current-buffer) (point-max))))
      (if (or (eq last-command 'slime-repl-return) visible)
          (progn
            ;; (message "It was visible last-command=%s" last-command)
            (setq ad-return-value ad-do-it))
        (let ((slime-maximum-output-dont t))
          ;; (message "It was NOT visible last-command=%s" last-command)
          (save-excursion 
            (setq ad-return-value ad-do-it)))))))

(when (require-if-available 'slime-presentations)
  (defadvice slime-presentation-write (around my-stop-slime-scroll activate)
    (with-current-buffer (slime-output-buffer)
      (let ((visible (point-visible-in-buffer (current-buffer) (point-max))))
        (if (or (eq last-command 'slime-repl-return) visible)
            (progn
              ;; (message "2 It was visible last-command=%s" last-command)
              (setq ad-return-value ad-do-it))
          (let ((slime-maximum-output-dont t))
            ;; (message "2 It was NOT visible last-command=%s" last-command)
            (save-excursion 
              (setq ad-return-value ad-do-it))))))))


(when (require-if-available 'mgm-highlight)
  (defun sldb-highlight-sexp (&optional start end)
    "Highlight the first sexp after point."
    (let ((start (or start (point)))
          (end (or end (save-excursion (ignore-errors (forward-sexp))
                                       (point)))))
      (mgm-highlight-region 'secondary-selection start end (current-buffer) nil))))


(defun run-clisp (&optional args)
  "Start SLIME with CLISP an inferior lisp"
  (interactive)
  (let ((inferior-lisp-program "clisp")
        (vars (copy-tree '(("LANG" "en_US")))))
    (slime args)))

(defun run-clisp-cvs (&optional args)
  "Start SLIME with CLISP an inferior lisp"
  (interactive)
  (let ((inferior-lisp-program "/usr/pkgs/clisp-cvs/bin/clisp")
        (vars (copy-tree '(("LANG" "en_US")))))
    (slime args)))

(defun run-sbcl2 (&optional args)
  "Start SLIME with SBCL an inferior lisp"
  (interactive)
  (let ((inferior-lisp-program "/home/max/cvs/sbcl2/run-sbcl"))
    (slime args)))

(defun run-sbcl (&optional args)
  "Start SLIME with SBCL an inferior lisp"
  (interactive)
  (let ((inferior-lisp-program "/home/max/cvs/sbcl/run-sbcl.sh")
        (slime-net-coding-system 'utf-8-unix))
    (slime args)))

(defun run-sbcl-tip (&optional args)
  "Start SLIME with SBCL an inferior lisp"
  (interactive)
  (let ((inferior-lisp-program "/home/max/cvs/sbcl-tip/run-sbcl.sh")
        (slime-net-coding-system 'utf-8-unix))
    (slime args)))

(defun run-ccl (&optional args)
  "Start SLIME with CCL an inferior lisp"
  (interactive)
  (let ((inferior-lisp-program "ccl")
        (slime-net-coding-system 'utf-8-unix))
    (slime args)))

(defun connect-sbcl-stump (&optional args)
  "Connect to StumpWM"
  (interactive)
  (slime-connect "localhost" 4105 'utf-8-unix))

;; fix the system name having text properties in it
(defadvice slime-read-system-name (around strip-properties activate)
  (let ((system ad-do-it))
    (setq ad-return-value (substring-no-properties system))))

(defun sldb-next ()
  "Step over call."
  (interactive)
  (slime-eval-async `(swank:sldb-next 0)))

(defun sldb-step ()
  "Step to next basic-block boundary."
  (interactive)
  (slime-eval-async `(swank:sldb-step 0)))

(defvar my-sldb-window-configuraton nil
  "Window configuration on sldb entry")


(defadvice slime-close-popup-window (after my-sldb-restore-window-configuration activate)
  (when my-sldb-window-configuraton
    (set-window-configuration my-sldb-window-configuraton)
    (setq my-sldb-window-configuraton nil)))

(defvar my-sldb-nosource-regexp nil)

(setq my-sldb-nosource-regexp
      (concat "^"
              (regexp-opt
               '("("
                 "((FLET "
                 "((LABELS "
                 "(LAMBDA () :IN "))
              (regexp-opt
               '("SWANK"
                 "SWANK-BACKEND"
                 "STEFIL::"
                 "COMMON-LISP:"
                 "SB-"))))

(defun my-sldb-process-buffer (buffer)
  (when t
    (with-current-buffer buffer
      (setq slime-popup-restore-data nil)
      (goto-char (point-min))
      (goto-char (goto-char (next-single-char-property-change (point) 'frame)))
      (destructuring-bind (start end frame locals catches) (sldb-frame-details)
        (let ((str (substring-no-properties (second frame))))
          ;; (log-sexp str)
          (cond ((string-match 
                  my-sldb-nosource-regexp
                  str)
                 ;; show all restarts and do not auto-pop source
                 (save-excursion
                   (let ((fn (get-text-property sldb-restart-list-start-marker 
                                                'sldb-default-action)))
                     (when fn (funcall fn)))))
                (t 
                 ;; show no restarts and do auto-pop-source
                 ;; (sldb-show-source)
                 (let ((fnum (sldb-frame-number-at-point))) 
                   ;; (log-sexp fnum)
                   (slime-eval-async
                       `(cl:let ((swank:*backtrace-printer-bindings*
                                  (cl:cons (cl:cons 'cl:*package*
                                                    (cl:find-package :keyword))
                                           swank:*backtrace-printer-bindings*)))
                                (cl:list (cl:first (swank:backtrace ,fnum ,(1+ fnum)))
                                         (swank:frame-source-location ,fnum)))
                     (lambda (result)
                       (when result
                         (let ((frame (first result))
                               (source-location (second result)))
                           (when (and (consp frame)
                                      (stringp (second frame))
                                      (not (string-match my-sldb-nosource-regexp 
                                                         (second frame))))
                             (destructure-case source-location
                               ((:error message)
                                (message "%s" message)
                                (ding))
                               (t
                                (slime-show-source-location source-location)))))))))
                 (sldb-show-frame-details))))))))

(defun my-sldb-hook ()
  (set (make-local-variable 'my-sldb-window-configuraton) (current-window-configuration))
  (run-at-time nil nil 'my-sldb-process-buffer (current-buffer)))

(add-hook 'sldb-hook 'my-sldb-hook)
(setq sldb-initial-restart-limit 0)

(defvar slime-C-h-map (make-sparse-keymap))

(global-set-key "\C-c\C-h" slime-C-h-map)
(global-set-key "\C-ch" slime-C-h-map)

(define-key slime-C-h-map "a" 'slime-apropos)
(define-key slime-C-h-map "A" 'slime-apropos-all)
(define-key slime-C-h-map "c" (lambda ()
                                (interactive)
                                (slime-list-connections)
                                (switch-to-buffer-other-window (get-buffer "*slime-connections*"))))
(define-key slime-C-h-map "t" (lambda ()
                                (interactive)
                                (slime-list-threads)
                                (switch-to-buffer-other-window (get-buffer "*slime-threads*"))))

(defvar mm/slime-restart-directory nil)
(defvar mm/start-lisp-force-dir nil)

(defadvice slime-restart-inferior-lisp (around preserve-current-dir activate)
  (let ((mm/slime-restart-directory
         (with-current-buffer (slime-output-buffer)
           default-directory)))
    (setq ad-return-value ad-do-it)))

(defadvice slime-quit-lisp-internal (around preserve-current-dir activate)
  (if (or (not mm/slime-restart-directory)
          (not (eq sentinel 'slime-restart-sentinel)))
      (setq ad-return-value ad-do-it)
    (let ((sentinel `(lambda (process _message)
                       (let ((mm/start-lisp-force-dir
                              ,mm/slime-restart-directory))
                         (funcall (function ,sentinel) process _message)))))
      (setq ad-return-value ad-do-it))))

(defadvice slime-start-lisp (around preserve-current-dir activate)
  (or directory (setq directory mm/start-lisp-force-dir))
  (setq ad-return-value ad-do-it))

(defadvice slime-flash-region (around change-default-timeout activate)
  (unless timeout (setq timeout 0.9))
  (setq ad-return-value ad-do-it))

;; force these modes to start in viper insert mode
(dolist (mode '(slime-repl-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (remove-from-list 'evil-normal-state-modes mode)
  (add-to-list 'evil-insert-state-modes mode))

(dolist (mode '(slime-xref-mode slime-compiler-notes-mode slime-connection-list-mode
                                slime-thread-control-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (remove-from-list 'evil-insert-state-modes mode)
  (add-to-list 'evil-normal-state-modes mode)
  (evil-give-back-keys-in-mode mode))

(evil-define-key 'normal slime-thread-control-mode-map "\C-k" 'slime-thread-kill)
(evil-define-key 'normal slime-connection-list-mode-map "D" 'slime-disconnect)

(defun slime-comint-k (arg)
  "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
  (interactive "P")
  (if (we-are-at-last-line-p)
      (progn
	(call-interactively 'slime-repl-previous-input)
        (slime-repl-bol))
    (call-interactively 'viper-previous-line)))

(defun slime-comint-j (arg)
  "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
  (interactive "P")
  (if (we-are-at-last-line-p)
      (progn
	(call-interactively 'slime-repl-next-input)
	(slime-repl-bol))
    (call-interactively 'viper-next-line)))


(defun slime-comint-n (arg)
  "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
  (interactive "P")
  (if (we-are-at-last-line-p)
      (progn
	(call-interactively 'slime-repl-previous-input)
	(move-end-of-line nil))
    (call-interactively 'viper-search-next)))

(defun slime-comint-N (arg)
  "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
  (interactive "P")
  (if (we-are-at-last-line-p)
      (progn
	(call-interactively 'slime-repl-next-input)
	(move-end-of-line nil))
    (call-interactively 'viper-search-Next)))

(evil-define-key 'normal slime-repl-mode-map "j" 'slime-comint-j)
(evil-define-key 'normal slime-repl-mode-map "k" 'slime-comint-k)
(evil-define-key 'normal slime-repl-mode-map "\C-m" 'evil-execute-in-emacs-state)
(evil-define-key 'normal slime-repl-mode-map "/" 'slime-repl-previous-matching-input)
(evil-define-key 'normal slime-repl-mode-map "n" 'slime-comint-n)
(evil-define-key 'normal slime-repl-mode-map "N" 'slime-comint-N)
(evil-define-key 'insert slime-repl-mode-map (kbd "DEL") 'evil-execute-in-emacs-state)
(evil-define-key 'normal slime-repl-mode-map "\C-m" 'viper-comint-enter)
(evil-define-key 'normal slime-repl-mode-map (kbd "<return>") 'viper-comint-enter)
(evil-define-key 'normal slime-repl-mode-map (kbd "RET") 'viper-comint-enter)
(evil-define-key 'insert slime-repl-mode-map (kbd "RET") 'slime-repl-return)


(defun mm/setup-slime-viper-stuff ()
  (set (make-local-variable 'viper-ex-style-motion) nil)
  (set (make-local-variable 'viper-ESC-moves-cursor-back) nil)
  (set (make-local-variable 'require-final-newline) nil))

(defadvice slime-repl-replace-input (after delete-trailing-whitespace activate)
  (when (looking-back "[ \t\n]+"  slime-repl-input-start-mark t)
    (delete-region (match-beginning 0) (match-end 0))))

(defvar repl-window-height nil
  "When set, will become REPL window height for the REPL
buffer. Only applied when existing window is split to get to
REPL")

(setq repl-window-height 20)

(defun mm/remember-repl-window-height ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'slime-repl-mode)
        (let ((win (get-buffer-window buf)))
          (when (and win (not (eq (frame-first-window)
                                  (next-window (frame-first-window)))))
            (set (make-local-variable 'repl-window-height)
                 (window-height win))))))))

(remove-hook 'window-configuration-change-hook 'mm/remember-repl-window-height)

(defun mm/repl-display-buffer-function (buffer &optional not-this-window)
  "Display repl in the window on the bottom of the current frame"
  (let* ((w1 (frame-first-window))
         (w2 (unless (eq w1 (next-window w1))
               (next-window w1)))
         (w3 (unless (or (null w2)
                         (eq w1 (next-window w2)))
               (next-window w2)))
         (height (with-current-buffer buffer repl-window-height))
         (win  
          (cond
           ((null w2)
            ;; just one window
            
            (split-window w1 (and height (< (window-height) height)
                                  (- (window-height) height))))
           ((null w3)
            ;; two windows, show it in the bottom 
            (unless (eq w1 (selected-window))
              ;; swap them
              (let ((old-buf (window-buffer w2)))
                (set-window-buffer w1 old-buf)
                w2))))))
    (if (null win)
        (let ((display-buffer-function nil))
          (display-buffer buffer not-this-window frame))
      (set-window-buffer win buffer)
      (when height
        (with-selected-window win
          (shrink-window (- (window-height win) height))))
      (select-window win))))


(add-hook 'slime-repl-mode-hook 'mm/setup-slime-viper-stuff)
(add-hook 'slime-repl-mode-hook
          (lambda ()
            (setq local-display-buffer-function 'mm/repl-display-buffer-function)))

(defun mm/next-xref-error (&optional how-many reset)
  (interactive)
  (when reset
    (goto-char (point-min)))
  (let ((backward (minusp how-many)))
    (multiple-value-bind (location pos)
        (with-current-buffer slime-xref-last-buffer
          (let (loc)
            (dotimes (cnt (abs how-many))
              (setq loc (slime-search-property 'slime-location backward)))
            (values loc (point))))
      (cond ((slime-location-p location)
             (slime-pop-to-location location)
             (slime-highlight-sexp)
             ;; We do this here because changing the location can take
             ;; a while when Emacs needs to read a file from disk.
             (with-current-buffer slime-xref-last-buffer
               (let ((window (display-buffer (current-buffer) t)))
                 (save-selected-window
                   (select-window window)
                   (goto-char pos)
                   (unless (pos-visible-in-window-p)
                     (cond ((= (current-column) 0) (recenter 1))
                           (t (recenter)))))
                 (slime-highlight-line 2.0))))
            ((null location)
             (message (if backward "No previous xref" "No next xref.")))
            (t                          ; error location
             (mm/next-xref-error how-many))))))

(defun mm/make-slime-xref-next-error-capable ()
  (setq next-error-function 'mm/next-xref-error))

(add-hook 'slime-xref-mode-hook 'mm/make-slime-xref-next-error-capable)

(defvar mm/last-compile-xref-buffer nil)

(defun mm/slime-show-xrefs-for-notes (notes)
  (let ((xrefs (slime-xrefs-for-notes notes)))
    (cond ((slime-length> xrefs 0)
           (slime-show-xrefs
            xrefs 'definition "Compiler notes" (slime-current-package))
           (setq mm/last-compile-xref-buffer slime-xref-last-buffer)
           (setq next-error-last-buffer slime-xref-last-buffer))
          (t (when mm/last-compile-xref-buffer
               (kill-buffer mm/last-compile-xref-buffer)
               (setq mm/last-compile-xref-buffer nil))))))

(remove-hook 'slime-compilation-finished-hook 'slime-maybe-show-compilation-log)
(add-hook 'slime-compilation-finished-hook 'mm/slime-show-xrefs-for-notes)

;;
;; Remind C-c C-k in Slime to a command that with a prefix argument
;; does the compile and load synchroniously and triggers error when
;; compilation failed
;;
;; Suitable for recording a keyboard macro that needs to do C-c C-k then
;; do something in REPL
;;
;; Without the prefix key, original C-c C-k is called.
;;
(defun mm/slime-compilation-finished-synchroniously (result)
  "Cut-n-paste of `slime-compilation-finished' but use
synchronous call to load the file.

Returns SUCCESS flag"
  (with-struct (slime-compilation-result. notes duration successp
                                          loadp faslfile) result
    (setf slime-last-compilation-result result)
    (slime-show-note-counts notes duration (cond ((not loadp) successp)
                                                 (t (and faslfile successp))))
    (when slime-highlight-compiler-notes
      (slime-highlight-notes notes))
    (run-hook-with-args 'slime-compilation-finished-hook notes)
    (when (and loadp faslfile 
               (or successp (slime-load-failed-fasl-p)))
      (slime-eval `(swank:load-file ,faslfile)))
    successp))

(defun mm/slime-compile-file-synchroniously (&optional load policy)
  "Cut-n-paste of `slime-compile-file' but do the call
synchroniously. Returns success flag"
  (interactive)
  (unless buffer-file-name
    (error "Buffer %s is not associated with a file." (buffer-name)))
  (check-parens)
  (when (buffer-modified-p)
    (save-buffer))
  (run-hook-with-args 'slime-before-compile-functions (point-min) (point-max))
  (let ((file (slime-to-lisp-filename (buffer-file-name)))
        (options (slime-simplify-plist `(,@slime-compile-file-options
                                         :policy ,policy)))
        (slime-load-failed-fasl 'never))
    (message "Compiling %s..." file)
    (mm/slime-compilation-finished-synchroniously
     (slime-eval
      `(swank:compile-file-for-emacs ,file ,(if load t nil) 
                                     . ,(slime-hack-quotes options))))))

(defun mm/compile-and-load-file (&optional arg)
  "Without the prefix argument calls `slime-compile-and-load-file'.

With prefix argument unconditionally saves the current file, then
synchroniously compiles and loads it, triggering error if not
successful. Suitable for using in keyboard macros"
  (interactive "P")
  (if (not arg)
      (slime-compile-and-load-file)
    (let ((slime-load-failed-fasl 'never))
      (or 
       (mm/slime-compile-file-synchroniously t)
       (error "Compilation failed")))))


(define-key slime-mode-map "\C-c\C-k" 'mm/compile-and-load-file)

(defadvice end-of-buffer (after slime-go-insert-mode activate)
  (when (and (eq major-mode 'slime-repl-mode)
             viper-mode)
    (evil-insert-state 1)))

;; fix it so that M-. correctly records buffer order
(defun slime-pop-to-location (location &optional where)
  (let* ((old-buffer (current-buffer))
         new-buffer
         pos)
    (save-excursion (slime-goto-source-location location)
                    (setq new-buffer (current-buffer))
                    (setq pos (point)))
    (with-current-buffer new-buffer
      (goto-char pos))
    (ecase where
      ((nil)
       (switch-to-buffer old-buffer)
       (switch-to-buffer new-buffer))
      (window    (pop-to-buffer new-buffer t))
      (frame     (let ((pop-up-frames t)) (pop-to-buffer new-buffer t))))))

(setq slime-inhibit-pipelining nil)
(setq slime-autodoc-use-multiline-p t)
(setq slime-autodoc-delay 0.1)
(setq common-lisp-style-default "modern")
(setq slime-repl-auto-right-margin nil)


(defun mm/slime-set-connection-right-margin (conn margin)
  (when (eq (process-status conn) 'open)
    (ignore-errors
      (save-excursion 
        (let ((slime-dispatching-connection conn)) 
          (slime-eval `(cl:setf cl:*print-right-margin* ,margin)))))))

(defun mm/slime-set-right-margin ()
  (dolist (conn slime-net-processes) 
    (let* ((buffer (slime-connection-output-buffer conn))
           (new-width (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (window-width))))
           (last-margin (process-get conn 'last-margin)))
      (unless (eql new-width last-margin)
        (process-put conn 'last-margin new-width)
        (mm/slime-set-connection-right-margin conn new-width)))))

(defun mm/remove-set-right-margin-hook ()
  (remove-hook 'window-configuration-change-hook 'mm/slime-set-right-margin))

(add-hook 'window-configuration-change-hook 'mm/slime-set-right-margin)
(add-hook 'kill-emacs-hook 'mm/remove-set-right-margin-hook)

(require 'info)
(require 'info-look)

;; info had been used before
(if Info-directory-list (add-to-list 'Info-directory-list "/home/max/ansicl/info")
    (add-to-list 'Info-default-directory-list "/home/max/ansicl/info"))

;; configure F1 S to go to CLHS symbol

(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(info-lookup-add-help
 :mode 'slime-repl-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(provide 'my-slime-setup)

;;; 
;;; End of SLIME setup
;;;
