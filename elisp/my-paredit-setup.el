
(ignore-errors (require-if-available 'paredit-magic))
(ignore-errors (require-if-available 'evil))

(setq comment-fill-column 120)

(defvar paredit-magic-word-skips-separator t
  "When T, word movement skip initial symbol constituents.

For example with this variable T, doing 2dw on a one-two-three
will leave -three and without it it will leave two-three.

Default is set to T, because the task of deleting or changing two
or more sub-words in a hyphenated symbol is more common then
wanting to delete symbol and a hyphen")

;; fix slurping not to include top level
(defun my-paredit-forward-slurp-sexp (&optional arg)
  "Like paredit-forward-slurp-sexp but stop and beep if about
to slurp next toplevel defun. Running with C-u argument will
slurp regardless"
  (interactive "p")
  (save-excursion
    (cond ((or (paredit-in-comment-p)
               (paredit-in-char-p))
           (error "Invalid context for slurpage"))
          ((paredit-in-string-p)
           (paredit-forward-slurp-into-string))
          ;; C-u argument? don't check
          ((eql arg 4)
           (my-paredit-forward-slurp-into-list))
          (t
           ;; find the depth of the sexp about to be slurped
           ;; cut-n-paste of code from paredit-forward-slurp-into-list
           ;; except don't actually do the changes
           (let* ((result 
                   (save-excursion 
                     (up-list)
                     (catch 'return     ; Go to the end of the desired
                       (while t         ;   S-expression, going up a
                         (paredit-handle-sexp-errors ;   list if it's not in this,
                             (progn (forward-sexp)
                                    (throw 'return nil))
                           (up-list))))
                     (parse-partial-sexp 
                      (point)
                      (save-excursion 
                        (beginning-of-defun)
                        (point)))))
                  (depth (first result)))
             (if (plusp depth)
                 (my-paredit-forward-slurp-into-list)
               (error "Use C-u argument to slurp top level sexp")))))))

(define-key paredit-mode-map [C-right] 'my-paredit-forward-slurp-sexp)

(dolist (foo '(paredit-forward
               paredit-backward
               up-list
               down-list
               backward-up-list
               backward-down-list
               forward-sexp
               backward-sexp))
  (put foo 'CUA 'move))

(defun my-paredit-forward-slurp-into-list ()
  "Same as paredit version but do not add empty line when its on
a line with closing paren by itself"
  (save-excursion
    (up-list)                          ; Up to the end of the list to
    (let ((close (char-before)))       ;   save and delete the closing
      (backward-delete-char 1)         ;   delimiter.
      (when (save-excursion
              (beginning-of-line)
              (looking-at "^[ \t\n]*\n"))
        (delete-region (match-beginning 0)
                       (match-end 0)))
      (catch 'return                    ; Go to the end of the desired
        (while t                        ;   S-expression, going up a
          (paredit-handle-sexp-errors   ;   list if it's not in this,
              (progn (forward-sexp)
                     (throw 'return nil))
            (up-list))))
      (insert close)
      (condition-case ()
          (progn 
            (backward-up-list) 
            (indent-sexp))
        (error
         (backward-sexp)
         (indent-sexp))))))

(defun my-paredit-forward-slurp-and-add (&optional arg)
  "Slurp next SEXP and advance past it, adding optional space"
  (interactive "p")
  (save-excursion
    (ignore-errors
      (let ((tmp (point)))
        (backward-up-list)
        (paredit-magic-fix-things tmp))))
  (my-paredit-forward-slurp-sexp arg)
  (if (looking-back "[[:space:]]+" nil t)       
      (delete-region (match-beginning 0) (match-end 0)))
  (unless (looking-back "\\s(['`,@]?")
    (insert " "))
  (if (looking-at "[[:space:]]+")       
      (delete-region (match-beginning 0) (match-end 0)))
  (cond ((looking-back "\\_>\\|\\s)")
         (when (looking-at "\\([ \t]+\\)")
           (delete-region (match-beginning 0) (match-end 0))
           (insert " ")))
        ((looking-back "\\s(['`,@]?")
         (when (looking-at "\\([ \t\n]+\\)")
           (delete-region (match-beginning 0) (match-end 0)))))
  (backward-up-list)
  (indent-sexp)
  (paredit-forward)
  (backward-down-list)
  (when (evil-normal-state-p)
    (evil-insert-state))
  (if (looking-back ")[[:space:]]*")
      (progn 
        (paredit-point-at-sexp-end)
        (when (looking-at "[[:space:]]+")
          (delete-region (match-beginning 0) (match-end 0)))
        (backward-down-list)
        (paredit-magic-close-paren))
    (unless (looking-back "[[:space:]('`,@]")
      (insert " "))))

(defun my-paredit-forward-barf-sexp ()
  (interactive)
  (paredit-forward-barf-sexp)
  (let ((indent (save-excursion
                  (back-to-indentation)
                  (point))))
    (when (< (point) indent)
      (goto-char indent))))

(define-key paredit-mode-map "\M-l" 'my-paredit-forward-slurp-and-add)
(define-key paredit-mode-map "\M-h" 'my-paredit-forward-barf-sexp)
(evil-define-key 'normal paredit-mode-map ";s" 'paredit-splice-sexp-killing-backward)

(defvar my-blink-matching-open-overlay nil)

(defvar my-blink-matching-open-overlay nil)

(defun my-blink-matching-open (&optional arg)
  "Temporarely highlight the sexp before point"
  (interactive)
  (when (and (> (point) (point-min))
	     blink-matching-paren
	     ;; Verify an even number of quoting characters precede the close.
	     (= 1 (logand 1 (- (point)
			       (save-excursion
				 (forward-char -1)
				 (skip-syntax-backward "/\\")
				 (point))))))
    (let* ((oldpos (point))
	   blinkpos
           endpos
	   message-log-max  ; Don't log messages about paren matching.
	   matching-paren
	   open-paren-line-string)
      (save-excursion
	(save-restriction
	  (if blink-matching-paren-distance
	      (narrow-to-region (max (point-min)
				     (- (point) blink-matching-paren-distance))
				oldpos))
	  (condition-case ()
	      (let ((parse-sexp-ignore-comments
		     (and parse-sexp-ignore-comments
			  (not blink-matching-paren-dont-ignore-comments))))
                (save-excursion
                  (paredit-point-at-sexp-end)
                  (setq oldpos (point))
                  (setq blinkpos (scan-sexps oldpos -1))
                  (setq endpos (scan-sexps blinkpos 1))))
	    (error nil)))
	(and blinkpos
	     ;; Not syntax '$'.
	     (not (eq (syntax-class (syntax-after blinkpos)) 8))
	     (setq matching-paren
		   (let ((syntax (syntax-after blinkpos)))
		     (and (consp syntax)
			  (eq (syntax-class syntax) 4)
			  (cdr syntax)))))
	(cond
	 ((not (or (eq matching-paren (char-before endpos))
                   ;; The cdr might hold a new paren-class info rather than
                   ;; a matching-char info, in which case the two CDRs
                   ;; should match.
                   (eq matching-paren (cdr (syntax-after (1- oldpos))))))
	  (message "Mismatched parentheses"))
	 ((not blinkpos)
	  (if (not blink-matching-paren-distance)
	      (message "Unmatched parenthesis")))
	 ((pos-visible-in-window-p blinkpos)
	  ;; Matching open within window, temporarily move to blinkpos but only
	  ;; if `blink-matching-paren-on-screen' is non-nil.
	  (when (and blink-matching-paren-on-screen 
                     (not inhibit-redisplay))
            (if my-blink-matching-open-overlay
                (move-overlay my-blink-matching-open-overlay blinkpos endpos (current-buffer))
              (setq my-blink-matching-open-overlay (make-overlay blinkpos endpos)))
            (overlay-put my-blink-matching-open-overlay 'priority show-paren-priority)
            (overlay-put my-blink-matching-open-overlay 'face 'show-paren-match)
            (sit-for blink-matching-delay)
            (delete-overlay my-blink-matching-open-overlay)))
	 (t
	  (save-excursion
	    (goto-char blinkpos)
	    (setq open-paren-line-string
		  ;; Show what precedes the open in its line, if anything.
		  (if (save-excursion
			(skip-chars-backward " \t")
			(not (bolp)))
		      (buffer-substring (line-beginning-position)
					(1+ blinkpos))
		    ;; Show what follows the open in its line, if anything.
		    (if (save-excursion
			  (forward-char 1)
			  (skip-chars-forward " \t")
			  (not (eolp)))
			(buffer-substring blinkpos
					  (line-end-position))
		      ;; Otherwise show the previous nonblank line,
		      ;; if there is one.
		      (if (save-excursion
			    (skip-chars-backward "\n \t")
			    (not (bobp)))
			  (concat
			   (buffer-substring (progn
					       (skip-chars-backward "\n \t")
					       (line-beginning-position))
					     (progn (end-of-line)
						    (skip-chars-backward " \t")
						    (point)))
			   ;; Replace the newline and other whitespace with `...'.
			   "..."
			   (buffer-substring blinkpos (1+ blinkpos)))
			;; There is nothing to show except the char itself.
			(buffer-substring blinkpos (1+ blinkpos)))))))
	  (message "Matches %s"
		   (substring-no-properties open-paren-line-string))))))))


(defun blink-matching-open (&optional arg)
  (interactive)
  (my-blink-matching-open arg))

(defun paredit-blink-paren-match (arg)
  (my-blink-matching-open))

(defvar my-paredit-kill-line-kill nil)
(defvar my-paredit-kill-line-kill-after nil)

(define-key  paredit-magic-mode-map (kbd "M-RET") 'comment-indent-new-line)

(defun paredit-magic-yank-characters (beg end &optional register yank-handler)
  "Saves the characters defined by the region BEG and END in the kill-ring."
  (let ((text (filter-buffer-substring beg end)))
    (when yank-handler
      (setq text (propertize text 'yank-handler (list yank-handler))))
    (when register
      (evil-set-register register text))
    (unless (eq register ?_)
      (kill-new text))))

;; TODO Split into smaller functions
;;
;; For example, concatenating the text to a kill, should not create
;; the ")(" or ")la" situations, and then should indent the kill in
;; the temporary buffer
(defun new-my-paredit-kill-line-1 (vbeg vend &optional yank-only type register yank-handler)
  "Kill text from VBEG to VEND, while preserving SEXP structure balance using
a lot of heuristics"
  ;; basically algorithm is
  ;; Go beg,
  ;; find next SEXP begin and END
  ;; is END ends before end?
  ;; if so delete it, r
  ;; otherwise, descend one level down, repeat
  (let ((done nil)
        this-sexp-start
        this-sexp-end
        this-sexp-end-greedy
        next-sexp-start
        skip-start-point
        end-on-current-line-p
        (kill "")
        (kill-after "")
        spliced-p)

    (goto-char vend)
    (setq vend (point-marker))
    (goto-char vbeg)
    (setq vbeg (point-marker))

    ;; (message "Deleting vbeg=%s vend=%s" vbeg vend)  
    
    (if (eq last-command 'my-dd-command)
        (setq kill my-paredit-kill-line-kill 
              kill-after my-paredit-kill-line-kill-after))
    
    (while (not done)
      (setq this-sexp-start nil
            this-sexp-end nil
            this-sexp-end-greedy nil
            next-sexp-start nil)
      (save-excursion
        (ignore-errors
          (forward-sexp)
          (setq this-sexp-end (point-marker))
          (save-excursion
            (backward-sexp)
            (setq this-sexp-start (point-marker)))
          (setq next-sexp-start (ignore-errors
                                  (save-excursion
                                    (paredit-point-at-sexp-start)
                                    (if (> (point) this-sexp-end)
                                        (point-marker)
                                      (this-sexp-end)))))
          (setq this-sexp-end-greedy 
                (or next-sexp-start
                    (ignore-errors
                      (save-excursion
                        (up-list)
                        (backward-down-list)
                        (point-marker)))
                    (save-excursion
                      (end-of-line)
                      (point-marker))))))
      (setq end-on-current-line-p (>= (line-end-position)
                                      vend))
      (cond 
       ;; Delete comments or whitespace before the SEXP
       ((and this-sexp-start (>= this-sexp-start vend)
             (< (point) vend))
        (setq kill (concat kill (filter-buffer-substring (point) vend)))
        (if yank-only (goto-char vend)
          (delete-region (point) vend) 
          (ignore-errors
            (indent-sexp))))
       ;; all done
       ((or (>= (point) vend)
            (and this-sexp-start (>= this-sexp-start vend)))
        (setq done t))

       ;; entire SEXP at point fits within the delete region,
       ;; and starts ((. In this case delete only the inner one))
       ((and this-sexp-end (< this-sexp-end vend)
             ;; this regexp  matches prefixes, and double ((
             (looking-at "\\(['`#.+-]?['`#.+-]?(\\)("))
        (goto-char (match-end 1))
        (let ((opoint (point)))
          (forward-sexp)
          (setq kill (concat kill (filter-buffer-substring opoint (point))))
          (unless yank-only 
            (delete-region opoint (point)))))
       ;; entire SEXP at point fits within the delete region
       ;; and this sexp is the last thing on current line
       ((and this-sexp-end (<= this-sexp-end vend)
             (save-excursion
               (goto-char this-sexp-end)
               ;; this regexp finds if we are looking an zero or more
               ;; closing parenthesis until end of line (with optional
               ;; closing comment) or end of buffer
               (looking-at "\\([ \t]*)?\\)*\\(;[^\n]*\\)?\\(?:\n\\|\\'\\)")))
        ;; we could have skipped a few sexps (because we were not sure
        ;; if we'll have to descend into sub-list or not, so we could
        ;; not delete immediately). Now that we found we did not have
        ;; to descend into sublist, delete from the point we started
        ;; skipping from.
        ;;
        ;; ie (a ^b c d ^^(blah crap)...) point is ^^ and ^ is where we
        ;; may have started from, delete from there..
        (let ((start (or skip-start-point
                         (cond ((and (eq type 'line)
                                     (looking-back "^[ \t]*"))
                                (line-beginning-position))
                               (t (point)))))
              (end this-sexp-end))
          ;; TODO include comment here too, if vend is after end of line
          (when (and (eq type 'line)
                     (>= vend (line-end-position))
                     (>= this-sexp-end-greedy (line-end-position)))
            (setq end (line-beginning-position 2)))
          (setq kill (concat kill (filter-buffer-substring start end)))
          (unless (or skip-start-point
                      (and (plusp (length kill))
                           (= (aref kill (1- (length kill))) ?\n))) 
            (setq kill (concat kill "\n")))
          (if yank-only (goto-char this-sexp-end)
            (delete-region start end) 
            (goto-char start) 
            ;; delete any extra whitespace too
            (when (looking-at "\\([ \t]*\\)")
              ;; (setq kill (concat kill (filter-buffer-substring (match-beginning 1) 
              ;;                                                      (match-end 1))))
              (let ((beg (match-beginning 0))
                    (end (if (eq type 'line) (match-end 0)
                           (min (match-end 0) vend)))) 
                (when (> end beg) (delete-region beg end))) 
              (indent-according-to-mode)) 
            (ignore-errors
              (indent-sexp)))))
       ;; this sexp fits withing the region but was not the last thing on current
       ;; line, in this case skip forward, remembering where we started..
       ;; if we finally found sexp that is the last thing on its line, without
       ;; having to descend into lists, we'll delete from the first point that
       ;; we skipped forward from
       ;;
       ;; If we had to descend into lists, the point of descend into the list will
       ;; be the new start point
       ;;
       ;; So pressing dd on (a b c d (e) f (blah ) will eventually delete from
       ;; b to (blah). But pressing dd on
       ;;
       ;; (a b c d (e) f (blah foo
       ;;                  bar))
       ;;                  
       ;; will descend into (blah) and not delete  anything before (blah
       ;; 
       ((and this-sexp-end (< this-sexp-end vend))
        (unless skip-start-point
          (setq skip-start-point (cond ((and (eq type 'line)
                                             (looking-back "^[ \t]*"))
                                        (line-beginning-position))
                                       (t this-sexp-start))))
        (forward-sexp)
        (paredit-point-at-sexp-start))
       ;; If current sexp ends after end, but the SEXP is a
       ;; a symbol, we can delete it without destroying balance
       ((and this-sexp-end (>= this-sexp-end vend)
             (looking-at "\\s'*\\s\\?\\_<"))
        (unless skip-start-point
          (setq skip-start-point (max
                                  (cond ((and (eq type 'line)
                                              (looking-back "^[ \t]*"))
                                         (line-beginning-position))
                                        (t this-sexp-start))
                                  vbeg)))
        (goto-char vend))
       ;; Delete partial when on the same line
       ((and skip-start-point
             end-on-current-line-p
             (> (point) skip-start-point))
        (setq kill (concat kill (filter-buffer-substring skip-start-point (point))))
        (unless yank-only 
          (delete-region skip-start-point (point))))
       ;; None of these situations, we have to descend into sublist
       (t (let ((ok nil))
            ;; if we have to descend into the list, don't delete
            ;; anything before that
            (setq skip-start-point nil)
            (ignore-errors
              (down-list)
              (setq ok t)
              ;; if after descending into a list, we are not looking
              ;; at another sublist
              (when (not (looking-at "[`',]?("))
                ;; Below determines if in situation of (^blah (more crap )...
                ;; we delete "blah" too, or start from (more crap). We only
                ;; start from more crap, if our end, is on different line
                (when (not end-on-current-line-p) 
                  (forward-sexp) 
                  (if (not (looking-at "[ \t]*\\(([ \t]*)\\)?[ \t]*\n"))
                      (paredit-point-at-sexp-start)
                    ;; Copied from paredit paredit-splice-sexp-killing-backwards
                    ;; Move backward until we hit the open paren ; then
                    ;; kill that selected region.
                    ;; (paredit-splice-sexp-killing-backward)
                    (save-excursion 
                      (let* ((end (paredit-point-at-sexp-start))
                             (text (progn 
                                     (paredit-ignore-sexp-errors
                                       (while (not (bobp))
                                         (backward-sexp)))
                                     (filter-buffer-substring (point) end))))
                        (setq kill (concat kill text))
                        (setq spliced-p t)
                        (unless yank-only 
                          (delete-region (point) end)
                          (backward-up-list)        ; Go up to the beginning...
                          (save-excursion
                            (forward-sexp)            ; Go forward an expression, to
                            (backward-delete-char 1)) ; delete the end delimiter.
                          (delete-char 1)             ; ...to delete the open char.
                          (paredit-ignore-sexp-errors
                            (backward-up-list)      ; Reindent, now that the
                            (indent-sexp))))        ; structure has changed. 
                      )))))
            (if (not ok) (setq done t))))))
    ;; If end of region is on current line, and we descended into list, start deleting
    (when (and skip-start-point
               end-on-current-line-p
               (> (point) skip-start-point))
      (setq kill (concat kill (filter-buffer-substring skip-start-point (point))))
      (unless yank-only 
        (delete-region skip-start-point (point))))
    (when (> (point) vend)
      (goto-char vend))
    (when spliced-p
      (setq kill (concat "(" kill ")")))
    (when yank-handler
      (setq kill (propertize kill 'yank-handler (list yank-handler))))
    (when register
      (evil-set-register register kill))
    (unless (eq register ?_)
      (kill-new kill))
    (setq my-paredit-kill-line-kill kill)
    (indent-according-to-mode)
    (setq this-command 'my-dd-command)))

(defun new-my-paredit-kill-line (vbeg vend &optional yank-only type register yank-handler)
  (if yank-only
      (save-excursion
        (new-my-paredit-kill-line-1 vbeg vend yank-only type register yank-handler))
    (new-my-paredit-kill-line-1 vbeg vend yank-only type register yank-handler)))

;; Magic motions
(evil-define-union-move paredit-magic-evil-move-word (count)
  "Move by words."
  (evil-move-chars
   (if paredit-magic-word-skips-separator
       "^ \t\r\n[:word:]-"
     "^ \t\r\n[:word:]")
   count)
  (let ((word-separating-categories evil-cjk-word-separating-categories)
        (word-combining-categories evil-cjk-word-combining-categories))
    (evil-forward-word count))
  (evil-move-empty-lines count))

(evil-define-motion paredit-magic-evil-forward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((move (if bigword #'evil-move-WORD #'paredit-magic-evil-move-word))
        (orig (point)))
    (prog1 (if (and evil-want-change-word-to-end
                    (not (looking-at "[[:space:]]"))
                    (eq evil-this-operator #'paredit-magic-evil-change))
               (evil-move-end count move)
             (evil-move-beginning count move))
      ;; if we reached the beginning of a word on a new line in
      ;; Operator-Pending state, go back to the end of the previous
      ;; line
      (when (and (evil-operator-state-p)
                 (> (line-beginning-position) orig)
                 (looking-back "^[[:space:]]*" (line-beginning-position)))
        ;; move cursor back as long as the line contains only
        ;; whitespaces and is non-empty
        (evil-move-end-of-line 0)
        ;; skip non-empty lines containing only spaces
        (while (and (looking-back "^[[:space:]]+$" (line-beginning-position))
                    (not (<= (line-beginning-position) orig)))
          (evil-move-end-of-line 0))
        ;; but if the previous line is empty, delete this line
        (when (bolp) (forward-char))))))

(evil-define-motion paredit-magic-evil-forward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'paredit-magic-evil-move-word)))
    ;; if changing a one-letter word, don't move point to the
    ;; next word (which would change two words)
    (if (and (evil-operator-state-p)
             (looking-at "[[:word:]]"))
        (prog1 (evil-move-end count move)
          (unless (bobp) (backward-char)))
      (evil-move-end count move nil t))))

(evil-define-motion paredit-magic-evil-backward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (let ((move (if bigword #'evil-move-WORD #'paredit-magic-evil-move-word)))
    (evil-move-beginning (- (or count 1)) move)))

(evil-define-motion paredit-magic-evil-backward-word-end (count &optional bigword)
  "Move the cursor to the end of the COUNT-th previous word.
If BIGWORD is non-nil, move by WORDS."
  :type inclusive
  (let ((move (if bigword #'evil-move-WORD #'paredit-magic-evil-move-word)))
    (evil-move-end (- (or count 1)) move nil t)))

(evil-define-motion paredit-magic-evil-forward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th next WORD."
  :type exclusive
  (paredit-magic-evil-forward-word-begin count t))

(evil-define-motion paredit-magic-evil-forward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th next WORD."
  :type inclusive
  (paredit-magic-evil-forward-word-end count t))

(evil-define-motion paredit-magic-evil-backward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th previous WORD."
  :type exclusive
  (paredit-magic-evil-backward-word-begin count t))

(evil-define-motion paredit-magic-evil-backward-WORD-end (count)
  "Move the cursor to the end of the COUNT-th previous WORD."
  :type inclusive
  (paredit-magic-evil-backward-word-end count t))

;;; Operator commands

(defun paredit-magic-figure-yank-handler (type yank-handler)
  (or yank-handler
      (cond ((eq type 'line) #'paredit-magic-paste-lines-handler)
            ((eq type 'block) (error "This should not happen"))
            (t #'paredit-magic-paste-handler))))

(defun paredit-magic-paste-handler (text)
  (let ((text (apply #'concat (make-list (or evil-paste-count 1) text)))
        (opoint (point)))
    (remove-list-of-text-properties
     0 (length text) yank-excluded-properties text)
    (insert text)))

(defun paredit-magic-paste-lines-handler (text)
  "Inserts the current text linewise."
  (let ((text (apply #'concat (make-list (or evil-paste-count 1) text)))
        (opoint (point))
        (paste-method (cond ((or (eq this-command #'evil-paste-before)
                                 (eq this-command #'paredit-magic-evil-paste-before))
                             'before)
                            ((or (eq this-command #'evil-paste-after)
                                 (eq this-command #'paredit-magic-evil-paste-after))
                             'after))))
    (remove-list-of-text-properties
     0 (length text) yank-excluded-properties text)
    ;; remove leading \n
    (when (string-match "\\`\\([ \t]*\n\\)+\\([^\n \t].*\\)" text)
      (setq text (match-string 1 text)))
    ;; remove too many trailing \n
    (when (string-match "\\`\\(.*[^\n \t]\\(?:[ \t]*\n\\)\\)\\(?:[\t ]*\n\\)+[ \t]*\\'" text)
      (setq text (match-string 1 text)))
    ;; ensure we end with newline
    (when (and (plusp (length text))
               (not (= ?\n (aref text (1- (length text))))))
      (setq text (concat text "\n")))
    (log-expr paste-method)
    (cond
     ((eq paste-method 'before)
      (evil-move-beginning-of-line)
      (evil-move-mark (point))
      (insert text)
      (setq evil-last-paste
            (list #'evil-paste-before
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (evil-set-marker ?\[ (mark))
      (evil-set-marker ?\] (1- (point)))
      (evil-exchange-point-and-mark)
      (save-excursion 
        (let (done)
          (ignore-errors
            (backward-up-list)
            (setq done t)
            (indent-sexp))
          (unless done
            (indent-region (point) (mark)))))
      (back-to-indentation))
     ((eq paste-method 'after)
      (paredit-magic-newline-below)
      (evil-move-mark (line-end-position 0))
      (insert text)
      (evil-set-marker ?\[ (1+ (mark)))
      (evil-set-marker ?\] (1- (point)))
      (delete-char -1)                  ; delete the last newline
      (setq evil-last-paste
            (list #'evil-paste-after
                  evil-paste-count
                  opoint
                  (mark t)
                  (point)))
      (ignore-errors 
        (indent-region (mark t) (point)))
      (evil-move-mark (1+ (mark t)))
      (evil-exchange-point-and-mark)
      ;; (save-excursion 
      ;;   (let (done)
      ;;     (ignore-errors
      ;;       (backward-up-list)
      ;;       (setq done t)
      ;;       (indent-sexp))
      ;;     (unless done
      ;;       (indent-region (point) (mark)))))
      (back-to-indentation))
     (t
      (insert text)))))

(evil-yank-handler 'evil-forward-word-begin)

(evil-define-operator paredit-magic-evil-yank (beg end type register yank-handler)
  "Saves the characters in motion into the kill-ring."
  :move-point nil
  :repeat nil
  (interactive "<R><x><y>")
  (if (or (and (fboundp 'cua--global-mark-active)
               (fboundp 'cua-copy-region-to-global-mark)
               (cua--global-mark-active))
          (eq type 'block))
      (evil-yank beg end type register yank-handler)
    ;; Structural yank
    (new-my-paredit-kill-line beg end t type register 
                              (paredit-magic-figure-yank-handler type yank-handler))))

(evil-define-operator paredit-magic-evil-yank-line (beg end type register)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R><x>")
  (paredit-magic-evil-yank beg end type register))

(evil-define-operator paredit-magic-evil-delete (beg end type register yank-handler)
  "Delete text from BEG to END with TYPE.
Save in REGISTER or in the kill-ring with YANK-HANDLER."
  (interactive "<R><x><y>")
  (if (eq type 'block)
      (evil-delete beg end type register yank-handler delete-func)
    (new-my-paredit-kill-line beg end nil type register 
                              (paredit-magic-figure-yank-handler type yank-handler)) 
    (unless register
      (let ((text my-paredit-kill-line-kill))
        (unless (string-match-p "\n" text)
          ;; set the small delete register
          (evil-set-register ?- text)))))
  ;; place cursor on beginning of line
  ;; (when (and (evil-called-interactively-p)
  ;;            (eq type 'line))
  ;;   (evil-first-non-blank))
  )

(evil-define-operator paredit-magic-evil-delete-line (beg end type register yank-handler)
  "Delete to end of line."
  :motion nil
  :keep-visual t
  (interactive "<R><x>")
  (if (eq type 'block)
      (evil-delete-line beg end type register yank-handler) 
    ;; act linewise in Visual state
    (let* ((beg (or beg (point)))
           (end (or end beg)))
      (when (evil-visual-state-p)
        (unless (memq type '(line))
          (let ((range (evil-expand beg end 'line)))
            (setq beg (evil-range-beginning range)
                  end (evil-range-end range)
                  type (evil-type range))))
        (evil-exit-visual-state))
      (cond
       ((eq type 'line)
        (paredit-magic-evil-delete beg end type register yank-handler))
       (t
        (paredit-magic-evil-delete beg (line-end-position) type register yank-handler))))))

(evil-define-operator paredit-magic-evil-delete-whole-line
  (beg end type register yank-handler)
  "Delete whole line."
  :motion evil-line
  (interactive "<R><x>")
  (paredit-magic-evil-delete beg end type register yank-handler))

(evil-define-operator paredit-magic-evil-change
  (beg end type register yank-handler delete-func)
  "Change text from BEG to END with TYPE.
Save in REGISTER or the kill-ring with YANK-HANDLER.
DELETE-FUNC is a function for deleting text, default `evil-delete'.
If TYPE is `line', insertion starts on an empty line.
If TYPE is `block', the inserted text in inserted at each line
of the block."
  (interactive "<R><x><y>")
  (if (eq type 'block)
      (evil-change beg end type register yank-handler delete-func)
    (let ((opoint (save-excursion
                    (goto-char beg)
                    (line-beginning-position))))
      (new-my-paredit-kill-line beg end nil type register
                                (paredit-magic-figure-yank-handler type yank-handler))
      (cond
       ((eq type 'line)
        (if (= opoint (point))
            (evil-open-above 1)
          (evil-open-below 1)))
       (t
        (evil-insert 1))))))

(evil-define-operator paredit-magic-evil-change-line (beg end type register yank-handler)
  "Change to end of line."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (paredit-magic-evil-change beg end type register yank-handler))

(evil-define-operator paredit-magic-evil-change-whole-line
  (beg end type register yank-handler)
  "Change whole line."
  :motion evil-line
  (interactive "<R><x>")
  (paredit-magic-evil-change beg end type register yank-handler))

;; motions
(evil-define-key 'motion paredit-magic-mode-map "w" 'paredit-magic-evil-forward-word-begin)
(evil-define-key 'motion paredit-magic-mode-map "W" 'paredit-magic-evil-forward-WORD-begin)
(evil-define-key 'motion paredit-magic-mode-map "b" 'paredit-magic-evil-backward-word-begin)
(evil-define-key 'motion paredit-magic-mode-map "B" 'paredit-magic-evil-backward-WORD-begin)
(evil-define-key 'motion paredit-magic-mode-map "e" 'paredit-magic-evil-forward-word-end)
(evil-define-key 'motion paredit-magic-mode-map "E" 'paredit-magic-evil-forward-WORD-end)

(evil-define-key 'normal paredit-magic-mode-map "y" 'paredit-magic-evil-yank)
(evil-define-key 'normal paredit-magic-mode-map "Y" 'paredit-magic-evil-yank-line)
(evil-define-key 'normal paredit-magic-mode-map "d" 'paredit-magic-evil-delete)
(evil-define-key 'normal paredit-magic-mode-map "D" 'paredit-magic-evil-delete-line)
;; (evil-define-key evil-ex-completion-map "\C-u" 'paredit-magic-evil-delete-whole-line)
(evil-define-key 'normal paredit-magic-mode-map "c" 'paredit-magic-evil-change)
(evil-define-key 'normal paredit-magic-mode-map "C" 'paredit-magic-evil-change-line)
(evil-define-key 'normal paredit-magic-mode-map "S" 'paredit-magic-evil-change-whole-line)



;; (evil-define-key 'normal paredit-magic-mode-map "C" 'paredit-evil-change-line)


(defun paredit-magic-newline-below ()
  "Create a newline below, that is opened in a DWIM manner, for example if
point is on a line that ends multiline S-EXP (ie some kind of body form),
add a new line inside of that body form"
  (interactive)
  (end-of-line) 
  (while (and
          ;; at closig paren
          (looking-back ")")
          ;; begins before current line
          (< (scan-sexps (point) -1) (point-at-bol)))
    (goto-char (1- (point))))
  (newline-and-indent))

(defun paredit-magic-evil-open-below (count)
  "Insert a new line in the S-EXP that starts on the current line"
  (interactive "p")
  (paredit-magic-newline-below)
  (setq evil-insert-count count
        evil-insert-lines t
        evil-insert-vcount nil)
  (evil-insert-state 1))

(evil-define-key 'normal paredit-magic-mode-map "o" 'paredit-magic-evil-open-below)

(defadvice backward-down-list (around keep-line-if-just-entered activate)
  (if (and paredit-mode 
           paredit-magic-mode
           (member last-command '(paredit-magic-evil-open-below
                                  paredit-magic-evil-open-above
                                  evil-open-below evil-open-above))
           (looking-back "\n[[:space:]]*"))
      (progn
        (delete-region (match-beginning 0) (match-end 0))
        (setq ad-return-value ad-do-it)
        (newline-and-indent)
        (paredit-magic-fix-things))
    (setq ad-return-value ad-do-it)))


(defvar my-magic-enter-auto-insert 
  '(paredit-forward paredit-backward up-list down-list 
                    backward-up-list backward-down-list))

;; not sure wtf I had this for
;; (defadvice evil-ret (around my-magic-enter activate)
;;   (if (or (not paredit-magic-mode)
;;           (not (member last-command my-magic-enter-auto-insert)))
;;       (setq ad-return-value ad-do-it)
;;     (evil-insert-state)
;;     (indent-according-to-mode)))

(defun my-paredit-duplicate-sexp (&optional arg)
  "Duplicate the SEXP that starts on the current line"
  (interactive)
  (let ((pt (point)) 
        linestart start end)
    (setq pt (point))
    (beginning-of-line)
    (setq linestart (point))
    (back-to-indentation)
    (setq start (point))
    (forward-sexp)
    (setq end (point))
    (insert "\n")
    (insert-buffer-substring (current-buffer) linestart start)
    (insert-buffer-substring (current-buffer) start end)
    (back-to-indentation)
    (goto-char (+ (point) (- pt start)))))

(evil-define-key 'motion paredit-mode-map "\C-d" nil)
(evil-define-key 'normal paredit-mode-map "\C-d" 'my-paredit-duplicate-sexp)
(evil-define-key 'insert paredit-mode-map "\C-d" 'my-paredit-duplicate-sexp)

(defadvice create-scratch-buffer (after enable-paredit activate)
  (paredit-mode t)
  (paredit-magic-mode t))

(require 'eldoc)

;; Fix eldoc space
(eldoc-add-command 'tempo-space 'paredit-forward 'paredit-backward
                   'up-list 'backward-up-list 'down-list
                   'my-paredit-forward-slurp-and-add
                   'paredit-magic-backspace
                   'paredit-magic-close-paren)

(provide 'my-paredit-setup)
