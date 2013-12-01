

(require 'cc-mode)
(require 'c-paredit)

(defun my-set-cc-style (name definition)
  (c-add-style name definition)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when c-buffer-is-cc-mode 
        (c-set-style name)))))

(progn (defconst my-c-style
         '(
           (c-basic-offset . 2)
           (c-offsets-alist
            (inline-open . 0)
            (substatement-open . 0))
           (c-hanging-braces-alist (brace-list-open)
                                   (brace-entry-open)
                                   (statement-cont)
                                   (substatement-open after)
                                   (statement-case-open after)
                                   (block-close . c-snug-do-while)
                                   (extern-lang-open after)
                                   (namespace-open after)
                                   (module-open after)
                                   (composition-open after)
                                   (inexpr-class-open after)
                                   (inexpr-class-close before))
           (c-cleanup-list brace-else-brace
                           brace-elseif-brace
                           brace-catch-brace
                           one-liner-defun
                           defun-close-semi
                           list-close-comma
                           empty-defun-braces
                           scope-operator
                           compact-empty-funcall
                           comment-close-slash)))
       (my-set-cc-style "mgm" my-c-style))

(defun cc/tempo-insert-string-function (string)
  (loop for c across string
        for command = (key-binding (vector c))
        do (cond ((eq command 'self-insert-command)
                  (insert c))
                 ((or (= c ?{) (= c ?}))
                  (let ((last-command-event c)
                        (blink-paren-function))
                    (c-electric-brace nil)))
                 ((or (= c ?() (= c ?)))
                  (let ((last-command-event c)
                        (blink-paren-function))
                    (c-electric-paren nil)))
                 (t (insert c)))))

(defadvice tempo-process-and-insert-string (around my-ccmode-setup activate)
  (if (not c-buffer-is-cc-mode)
      (setq ad-return-value ad-do-it)
    (cc/tempo-insert-string-function string)))

;; (defadvice comment-indent-new-line (around mm/fix-ccmode activate)
;;   (if (not c-buffer-is-cc-mode)
;;       (setq ad-return-value ad-do-it)
;;     (let ((literal (c-in-literal)))
;;       (if (not (eq literal 'c))
;;           (newline-and-indent)
;;         (indent-according-to-mode)
;;         (newline-and-indent)
;;         (insert "* ")
;;         (indent-according-to-mode)))))

(defun my-c-semicolon (arg)
  (interactive "*P")
  (let ((literal (c-in-literal)))
    (when (and (c-paredit-in-string-p literal)
               (looking-at "\""))
      (forward-char 1)
      (setq literal nil))
    (cond
     ((c-paredit-in-comment-p literal)
      (insert ?\;))
     ((c-paredit-in-string-p literal)
      (insert ?\;))
     ((looking-at "[ \t]*\\\\?$")       ; end of line
      (c-electric-semi&comma arg))
     (t
      (let* ((start-pos (point))
             (for-statement-p
              (ignore-errors
                (save-excursion
                  (backward-up-list)
                  (looking-back "for "))))
             (line-end (line-end-position))
             (stmt-end (save-excursion (c-end-of-statement)
                                       (point)))
             (stmt-beg (save-excursion (call-interactively 'c-beginning-of-statement)
                                       (point)))
             (done nil))
        (goto-char stmt-beg)
        (while (and (not done)
                    (c-syntactic-re-search-forward "\\((\\|\\[\\)" nil t nil))
          (let* ((open (char-before))
                 (match (c-syntactic-re-search-forward 
                         (format "%c"
                                 (if (eql open ?\() ?\) ?\]))
                         nil t t)))
            (if (not match)
                (error "Unmatched %c in the current statement" open)
              (when (> (point) start-pos)
                (setq done t)))))
        (goto-char (min stmt-end (max start-pos (point))))
        (cond ((> (point) stmt-end)
               (error "Can't insert semi-colon here"))
              ((and for-statement-p
                    (looking-back ";")  ;
               (tempo-forward-mark)))
              (t
               (when (looking-back ";")
                 (delete-char -1))
               (c-electric-semi&comma arg))))))))

(defun my-c-paredit-electric-brace (arg)
  (interactive "*P")
  (when (memq major-mode '(c-mode c++-mode))
    (while (looking-at "[])]")
      (forward-char 1)))
  (c-paredit-electric-brace arg))

(defun c-tempo-asterisk ()
  "Expand tempo asterisk when not in comment or string"
  (interactive "*")
  (c-electric-star nil)
  (let ((comment (save-excursion
                   (comment-beginning)))
        (in-string-p (in-string-p)))
    (if (or (and (not comment)
                 (not in-string-p))
            (and comment
                 (looking-back "/\\*\\*\\=")))
        (tempo-expand-if-complete))))

(defun enable-tempo-asterisk ()
  (when (member major-mode '(c-mode c++-mode))
    (local-set-key "*" 'c-tempo-asterisk)))

(defun my-c-mode-common-hook ()
  (c-toggle-auto-newline t)
  (c-paredit-mode)
  (enable-tempo-asterisk)
  (abbrev-mode -1))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(defun my-maybe-delete-previous-electric-newline ()
  "If previous command was electric semicolon, that had inserted
a newline, and user next action indicates they probably don't
that newline (by moving point somewhere else or exiting vi insert
mode) then remove that newline."
  (when c-buffer-is-cc-mode
    ;; exiting insert mode after electric semicolon auto inserted
    ;; the newline means we don't want newline.
    (when (and (member last-command '(c-electric-semi&comma
                                      my-c-semicolon
                                      my-c-paredit-electric-brace
                                      ;; viper-open-line
                                      ;; vipen-Open-line
                                      ))
               (save-excursion
                 (beginning-of-line)
                 (looking-at "^[[:space:]]*$")))
      (delete-region (point)
                     (progn
                       (forward-visible-line -1)
                       (end-of-line)
                       (point)))
      ; (goto-char (1- (point)))
      t)))

(defun maybe-do-cleanups ()
  (when
    (and (looking-at ";?[ \t\n]*}")
         ;; {} is not on the same line, and is non-empty
         ;; (let ((opoint (point))
         ;;       (line-start (line-beginning-position)))
         ;;   (save-excursion
         ;;     (goto-char (match-end 0))
         ;;     (c-backward-sexp)
         ;;     (and (< (point) line-start)
         ;;          (not (looking-at "{[ \t\n]*}")))))
         )
    (let ((blink-paren-function nil))
      (c-paredit-close-brace nil))
    t))

;; (defadvice viper-exit-insert-state (after ccmode-cancel-auto-newline activate)
;;   (when c-buffer-is-cc-mode
;;     (let* ((opoint (point))
;;            (did-removed-newline
;;             (my-maybe-delete-previous-electric-newline)))
;;       (when did-removed-newline
;;         ;; maybe-do-cleanups calls c-paredit-close-brace
;;         ;; when it detects possible cleanup, which already does our thing
;;         (unless (or
;;                  ;; Common ideom is pressing o key on
;;                  ;;
;;                  ;; Go to inline hanging_defun() {return blah;}
;;                  ;; 
;;                  ;; which our magic opens into 
;;                  ;;
;;                  ;; inline hanging_defun()
;;                  ;; {
;;                  ;;   return blah;
;;                  ;;   ^
;;                  ;; }
;;                  ;;
;;                  ;; At this point a very common thing is to do Esc O to start adding
;;                  ;; text before return
;;                  ;;
;;                  ;; Therefore don't run the cleanups if previous command was o
;;                  (memq last-command '(viper-open-line viper-Open-line))
;;                  (maybe-do-cleanups))
;;           (let ((next-mark (catch 'found
;;                              (mapc
;;                               (function
;;                                (lambda (mark)
;;                                  (if (< (point) mark)
;;                                      (throw 'found mark))))
;;                               tempo-marks)
;;                              ;; return nil if not found
;;                              nil)))
;;             (when (and next-mark
;;                        (< (- next-mark (point)) 10))
;;               (goto-char next-mark)
;;               (back-to-indentation))))))))

;; (defadvice evil-insert-state 1 (after ccmode-auto-indent activate)
;;   (when c-buffer-is-cc-mode
;;     (c-indent-line)))

(defadvice tempo-forward-mark (before ccmode-cancel-auto-newline activate)
  (when c-buffer-is-cc-mode
    (my-maybe-delete-previous-electric-newline)
    (maybe-do-cleanups)))

(defadvice c-paredit-close-brace (before ccmode-cancel-auto-newline activate)
  (my-maybe-delete-previous-electric-newline))

(defun my-c-style-hook ()
  (c-set-style "mgm"))

(add-hook 'c-mode-hook 'my-c-style-hook) 
(add-hook 'c++-mode-hook 'my-c-style-hook) 

(defun my-c-colon (arg)
  (interactive "*P")
  (cond ((and (looking-at ":")
               (eq 'case-label (caar (c-guess-basic-syntax))))
         (tempo-forward-mark))
        (t (c-electric-colon arg))))

(define-key c-mode-base-map ";" 'my-c-semicolon)
(define-key c-paredit-mode-map "{" 'my-c-paredit-electric-brace)
(define-key c-mode-base-map ":" 'my-c-colon)
(define-key c-mode-base-map "\M-c" 'tempo-template-c-case-with-break)
(define-key c-mode-base-map "\C-\M-c" 'tempo-template-c-case-with-brace)

;; fuck this, just indent line by line
;; (defadvice c-indent-line-or-region (around ident-line-by-line-manually activate)
;;   (if (not region)
;;       (progn (setq c-state-cache nil)
;;              (setq ad-return-value ad-do-it))
;;     (let ((start (region-beginning))
;;           (end (region-end)))
;;       (goto-char start)
;;       (while (< (point) end)
;;         (setq c-state-cache nil)
;;         (c-indent-line)
;;         (forward-line)))))

;; (defadvice indent-region (around ident-line-by-line-manually activate)
;;   (if (not c-buffer-is-cc-mode) (setq ad-return-value ad-do-it)
;;     (goto-char start)
;;     (while (< (point) end)
;;       (setq c-state-cache nil)
;;       (c-indent-line)
;;       (forward-line))))

(provide 'my-ccmode-setup)
