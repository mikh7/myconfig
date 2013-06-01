;;
;; Paredit mode equivalent for cc-mode based modes
;;

(require 'cc-mode)
(require 'paredit) ; to reuse some utilities
(require 'tempo)

(defvar c-paredit-mode nil)

(defvar c-paredit-mode-map (make-sparse-keymap)
  "Keymap for c-paredit-mode")

(define-minor-mode c-paredit-mode
  "Minor mode that simulifies maintaining balanced parentesis and brackets in CC Mode derived modes \\<c-paredit-mode-map>"
  (if (and c-paredit-mode
           (not current-prefix-arg))
      (if (not (fboundp 'check-parens))
          (c-paredit-warn "`check-parens' is not defined; %s"
                        "be careful of malformed S-expressions.")
          (condition-case condition
              (check-parens)
            (error (setq c-paredit-mode nil)
                   (signal (car condition) (cdr condition)))))))

(defun c-paredit-in-comment-p (literal)
  (memq literal '(c c++)))

(defun c-paredit-in-string-p (literal)
  (eq literal 'string))

(defun c-paredit-move-past-close (close)
  "Move past the closing parentesis"
 (let ((literal (c-in-literal)))
   (if (or (c-paredit-in-string-p literal)
           (c-paredit-in-comment-p literal))
       (insert close)
     (let ((result (c-syntactic-re-search-forward
                    (format "%c" close) nil t nil)))
       (when result
         (goto-char (point)))))))

(defun c-paredit-move-past-close-and-newline (close)
  "Move past the closing parentesis"
  (let ((literal (c-in-literal)))
    (if (or (c-paredit-in-string-p literal)
            (c-paredit-in-comment-p literal))
        (insert close)
      (let ((result (c-syntactic-re-search-forward
                     (format "%c" close) nil t nil)))
        (when result
          (goto-char (1+ (point)))
          (newline-and-indent))))))

(eval-and-compile
  
  (defmacro define-c-paredit-pair (open close name)
    `(progn
       (defun ,(paredit-conc-name "c-paredit-open-" name) (&optional n)
         ,(concat "Insert a balanced " name " pair.
With a prefix argument N, put the closing " name " after N
  S-expressions forward.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  " name " pair around the region.
If in a string or a comment, insert a single " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
         (interactive "P")
         (let ((literal (c-in-literal)))
           (cond ((or (c-paredit-in-string-p literal)
                      (c-paredit-in-comment-p literal))
                  (insert ,open))
                 (t
                  (c-paredit-insert-pair n ,open ,close 'goto-char)))))
       (defun ,(paredit-conc-name "c-paredit-close-" name) ()
         ,(concat "Move past one closing delimiter and reindent.
\(Agnostic to the specific closing delimiter.)
If in a string or comment, insert a single closing " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
         (interactive)
         (c-paredit-move-past-close ,close))
       (defun ,(paredit-conc-name "c-paredit-close-" name "-and-newline") ()
         ,(concat "Move past one closing delimiter, add a newline,"
                  " and reindent.
If there was a margin comment after the closing delimiter, preserve it
  on the same line.")
         (interactive)
         (c-paredit-move-past-close-and-newline ,close)))))

(defun c-paredit-space-for-delimiter-p (endp delimiter)
  ;; If at the buffer limit, don't insert a space.  If there is a word,
  ;; symbol, other quote, or non-matching parenthesis delimiter (i.e. a
  ;; close when want an open the string or an open when we want to
  ;; close the string), do insert a space.
  (and nil
       (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp
                              (char-after)
                            (char-before)))
             (list ?w ?_ ?\"
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))))))

(defun c-paredit-insert-pair (n open close forward)
  (let* ((regionp (and (paredit-region-active-p)
                       (paredit-region-safe-for-insert-p)))
         (end (and regionp
                   (not n)
                   (prog1 (region-end)
                     (goto-char (region-beginning))))))
    (let ((spacep (c-paredit-space-for-delimiter-p nil open)))
      (if spacep (insert " "))
      (insert open)
      (save-excursion
        ;; Move past the desired region.
        (cond (n (funcall forward
                          (save-excursion
                            (forward-sexp (prefix-numeric-value n))
                            (point))))
              (regionp (funcall forward (+ end (if spacep 2 1)))))
        (insert close)
        (if (c-paredit-space-for-delimiter-p t close)
            (insert " "))))))

(define-c-paredit-pair ?\( ?\) "parenthesis")
(define-c-paredit-pair ?\[ ?\] "bracket")
(define-c-paredit-pair ?\{ ?\} "brace")
(define-c-paredit-pair ?\< ?\> "brocket")

(defun c-paredit-backward-delete (&optional arg)
  "Delete a character backward or move backward over a delimiter.
If on a closing S-expression delimiter, move backward into the
  S-expression.
If on an opening S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a prefix argument, simply delete a character backward, without
  regard for delimiter balancing."
  (interactive "P")
  (cond ((or arg (bobp))
         (backward-delete-char 1))      ;++ should this untabify?
        ((paredit-in-string-p)
         (paredit-backward-delete-in-string))
        ((paredit-in-comment-p)
         (backward-delete-char 1))
        ;; ((paredit-in-char-p)            ; Escape -- delete both chars.
        ;;  (backward-delete-char 1)
        ;;  (delete-char 1)
        ;; ((paredit-in-char-p (1- (point)))
        ;;  (backward-delete-char 2))      ; ditto
        ((let ((syn (char-syntax (char-before))))
           (or (eq syn ?\))
               (eq syn ?\")))
         (backward-char))
        ((and (eq (char-syntax (char-before)) ?\()
              (eq (char-after) (matching-paren (char-before))))
         (backward-delete-char 1)       ; Empty list -- delete both
         (delete-char 1))               ;   delimiters.
        ;; Delete it, unless it's an opening parenthesis.  The case
        ;; of character literals is already handled by now.
        ((not (eq (char-syntax (char-before)) ?\())
         (backward-delete-char-untabify 1))))

(defun c-paredit-forward-delete (&optional arg)
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
  S-expression.
If on a closing S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a prefix argument, simply delete a character forward, without
  regard for delimiter balancing."
  (interactive "P")
  (let ((literal (c-in-literal)))
    (cond ((or arg (eobp))
           (delete-char 1))
          ((c-paredit-in-string-p literal)
           (paredit-forward-delete-in-string))
          ((c-paredit-in-comment-p literal)
                                        ;++ What to do here?  This could move a partial S-expression
                                        ;++ into a comment and thereby invalidate the file's form,
                                        ;++ or move random text out of a comment.
           (delete-char 1))
          ;; ((paredit-in-char-p)            ; Escape -- delete both chars.
          ;;  (backward-delete-char 1)
          ;;  (delete-char 1))
          ;; ((eq (char-after) ?\\)         ; ditto
          ;;  (delete-char 2))
          ((let ((syn (char-syntax (char-after))))
             (or (eq syn ?\()
                 (eq syn ?\")))
           (forward-char))
          ((and ;; (not (paredit-in-char-p (1- (point))))
            (eq (char-syntax (char-after)) ?\))
            (eq (char-before) (matching-paren (char-after))))
           (backward-delete-char 1)     ; Empty list -- delete both
           (delete-char 1))             ;   delimiters.
          ;; Just delete a single character, if it's not a closing
          ;; parenthesis.  (The character literal case is already
          ;; handled by now.)
          ((not (eq (char-syntax (char-after)) ?\)))
           (delete-char 1)))))

(defun c-paredit-doublequote (&optional n)
  "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  pair of double-quotes around the region, again escaping intermediate
  characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (let ((literal (c-in-literal)))
    (cond ((c-paredit-in-string-p literal)
           (let* ((start+end (paredit-string-start+end-points))
                  (type (char-after (car start+end)))
                  (single-quote-p (eq type ?\')))
             (if (and
                  (not single-quote-p)
                  (eq (cdr start+end)
                      (point)))
                 (forward-char)         ; We're on the closing quote.
               (if single-quote-p
                   (insert ?\")
                 (insert ?\\ ?\")))))
          ((c-paredit-in-comment-p literal)
           (insert ?\"))
          (t
           (paredit-insert-pair n ?\" ?\" 'paredit-forward-for-quote)))))

(defun c-paredit-quote (&optional n)
  "Insert a pair of quotes.
Inside a comment, insert a literal quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (let ((literal (c-in-literal)))
    (cond ((c-paredit-in-string-p literal)
           (let* ((start+end (paredit-string-start+end-points))
                  (type (char-after (car start+end)))
                  (single-quote-p (eq type ?\'))
                  (empty-p (eql (cdr start+end)
                                (1+ (car start+end)))))
             (if (and single-quote-p
                      (eq (cdr start+end)
                          (point))
                      (not empty-p))
                 (forward-char)         ; We're on the closing quote.
               (if single-quote-p
                   (insert ?\\ ?\')
                 (insert ?\')))))
          ((c-paredit-in-comment-p literal)
           (insert ?\'))
          (t
           (insert ?\' ?\')
           (backward-char)))))


(defun c-paredit-electric-brace (arg)
  "Insert pair of braces, first brace is inserted by calling `c-electric-brace'"
  (interactive "*P")
  (let ((literal (c-in-literal)))
    (cond ((or (c-paredit-in-string-p literal)
               (c-paredit-in-comment-p literal))
           (insert ?\{))
          (t
           (let ((stmt-start (save-excursion
                               (when (eq 'same (c-beginning-of-statement-1 nil nil t))
                                 (point))))
                 insert-semicolon)
             (when (and stmt-start
                        (save-excursion
                          (goto-char stmt-start)
                          (looking-at "class\\|struct")))
               (setq insert-semicolon t))
             (c-electric-brace arg)
             (let* ((mark (point-marker)))
               (tempo-insert-mark mark)
               (save-excursion
                 (insert ?\n ?\})
                 (indent-according-to-mode)
                 (if insert-semicolon
                     (insert ?\;))
                 (c-newline-and-indent)
                 (tempo-insert-mark (point-marker)))
               (goto-char mark)))))))


(defvar c-paredit-close-parenthesis-action-alist nil)

(setq c-paredit-close-parenthesis-action-alist
      '((substatement-open tempo-forward-mark)
        (defun-open tempo-forward-mark)
        (class-open tempo-forward-mark)))

(defun c-paredit-close-parenthesis (arg)
  "Close parentesis"
  (interactive "*P")
  (c-paredit-move-past-close ?\))
  (let* ((syntax nil)
         (dummy (save-excursion
                   (c-skip-ws-forward)
                   (c-tentative-buffer-changes
                     (insert ?\n)
                     (setq syntax (c-guess-basic-syntax))
                     nil)))
         (actions (c-intersect-lists (car syntax) c-paredit-close-parenthesis-action-alist)))
    (dolist (action (cdr actions))
      (cond ((symbolp action)
             (funcall action))
            ((listp action)
             (eval action))))))

(defun c-paredit-close-brace (arg)
  "Close brace"
  (interactive "*P")
  (c-paredit-move-past-close ?\})
  (when (looking-back "}")
    (delete-char -1)
    (let ((last-command-event ?\}))
      (c-electric-brace nil))))

(c-intersect-lists '(substatement-open inline-open class-open) '((substatement-open . 1234)))

(define-key c-paredit-mode-map "(" 'c-paredit-open-parenthesis)
(define-key c-paredit-mode-map ")" 'c-paredit-close-parenthesis)
(define-key c-paredit-mode-map "{" 'c-paredit-electric-brace)
(define-key c-paredit-mode-map "}" 'c-paredit-close-brace)
(define-key c-paredit-mode-map "[" 'c-paredit-open-bracket)
(define-key c-paredit-mode-map "]" 'c-paredit-close-bracket)
(define-key c-paredit-mode-map "<" 'c-electric-lt-gt)
(define-key c-paredit-mode-map ">" 'c-electric-lt-gt)

(define-key c-paredit-mode-map (kbd "DEL") 'c-paredit-backward-delete)
(define-key c-paredit-mode-map (kbd "<delete>") 'c-paredit-forward-delete)
(define-key c-paredit-mode-map "\"" 'c-paredit-doublequote)
(define-key c-paredit-mode-map "\'" 'c-paredit-quote)

(defun c-paredit-viper-open-line ()
  (interactive)

  (let* ((opoint (point))
         (line-start (line-beginning-position))
         (literal (progn (goto-char (line-end-position))
                         (c-in-literal)))
         start end)
    (cond ((or (c-paredit-in-string-p literal)
               (c-paredit-in-comment-p literal))
           (goto-char opoint)
           nil)
          ((looking-back "{\\([ \t]*\\)}")
           (let ((start (match-beginning 0))
                 (ws-start (match-beginning 1))
                 (ws-end (match-end 1))
                 (end (match-end 0)))
             ;; (goto-char start)
             ;; (let ((syntax (c-point-syntax)))
             ;;   (when (or (eq (caar (last syntax))
             ;;                 'inline-open)
             ;;             (eq (caar (last syntax))
             ;;                 'defun-open))))

             ;; there are simpler way, just delete entire brace and
             ;; act as we just inserted it again, let brace code do
             ;; all the work
             ;;
             (goto-char start)
             (delete-region start end)
             (viper-change-state-to-insert)
             (let ((last-command-event ?\{))
               (c-paredit-electric-brace arg)))
           t)
          ((and (looking-back "}")
                (progn
                  (setq end (match-beginning 0))
                  (c-backward-sexp)
                  (setq start (1+ (point)))
                  (>= (point) line-start)))
           (let ((tmp (buffer-substring start end)))
             (delete-region (point) (1+ end))
             (viper-change-state-to-insert)
             (let ((last-command-event ?\{))
               (c-paredit-electric-brace arg))
             (insert tmp)
             (indent-according-to-mode)
             (end-of-line)
             (insert "\n")
             (indent-according-to-mode)
             t))
          (t (goto-char opoint)
             nil))))

;; end
(provide 'c-paredit)


