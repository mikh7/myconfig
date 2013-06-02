
(ignore-errors (require-if-available 'paredit-magic))
(ignore-errors (require-if-available 'evil))

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

(evil-define-union-move paredit-magic-move-word (val)
  (let ((limit (point))
        (opoint (point)))
    (ignore-errors (setq limit (scan-sexps (point) val)))
    (evil-move-word val)
    (cond
     ((and (> val 0)
           (> (point) limit))
      (goto-char limit))
     ((and (< val 0)
           (< (point) limit))
      (goto-char limit)))
    (if (= (point) opoint) 0 1)))

(evil-define-motion paredit-magic-forward-word-begin (count &optional bigword)
  "Move the cursor to the beginning of the COUNT-th next word.
If BIGWORD is non-nil, move by WORDS."
  :type exclusive
  (or count (setq count 1))
  (let* ((beg (or (ignore-errors (scan-sexps (point) count)) (point)))
         (end (or (ignore-errors (scan-sexps (point) (- count))) (point)))
         (move
          ;; (if bigword #'paredit-magic-move-WORD #'paredit-magic-move-word)
          (if bigword #'evil-move-WORD #'evil-move-word))
         (orig (point)))
    (save-restriction 
      (narrow-to-region beg end)
      (prog1 (if (and evil-want-change-word-to-end
                      (not (looking-at "[[:space:]]"))
                      (eq evil-this-operator #'evil-change))
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
          (when (bolp) (forward-char)))))))

(evil-define-motion paredit-magic-forward-WORD-begin (count)
  "Move the cursor to the beginning of the COUNT-th next WORD."
  :type exclusive
  (paredit-magic-forward-word-begin count t))

(evil-define-key 'motion paredit-magic-mode-map "w" 'paredit-magic-forward-word-begin)
(evil-define-key 'motion paredit-magic-mode-map "W" 'paredit-magic-forward-WORD-begin)


;; (defun paredit-magic-end-of-Word-kernel (val)
;;   (while (> val 0)
;;     (while (looking-at "[()]")
;;       (forward-char)
;;       (viper-move-marker-locally 'viper-com-point (point)))
;;     (when (viper-looking-at-separator)
;;       (viper-skip-all-separators-forward))
;;     (cond
;;      ((viper-looking-at-alpha)
;;       (viper-skip-alpha-forward "_"))
;;      ((not (viper-looking-at-alphasep))
;;       (viper-skip-nonalphasep-forward)))
;;     (let ((viper-SEP-char-class " -()"))
;;       (viper-skip-nonseparators 'forward))
;;     (setq val (1- val))))

;; (defun viper-forward-word (arg)
;;   "Forward word."
;;   (interactive "P")
;;   (viper-leave-region-active)
;;   (let* ((do-paredit-magic
;;           (and paredit-mode paredit-magic-mode))
;;          (val (viper-p-val arg))
;;          (com (viper-getcom arg)))
;;     (if com (viper-move-marker-locally 'viper-com-point (point)))
;;     (if do-paredit-magic (paredit-magic-forward-word-kernel val)
;;       (viper-forward-word-kernel val))
;;     (if com
;; 	(progn
;; 	  (cond ((viper-char-equal com ?c)
;; 		 (viper-separator-skipback-special 'twice viper-com-point))
;; 		;; Yank words including the whitespace, but not newline
;; 		((viper-char-equal com ?y)
;; 		 (viper-separator-skipback-special nil viper-com-point))
;; 		((viper-dotable-command-p com)
;; 		 (viper-separator-skipback-special nil viper-com-point)))
;; 	  (viper-execute-com 'viper-forward-word val com)))))

;; (defun viper-forward-Word (arg)
;;   "Forward word delimited by white characters."
;;   (interactive "P")
;;   (viper-leave-region-active)
;;   (let* ((do-paredit-magic
;;           (and paredit-mode paredit-magic-mode))
;;          (val (viper-p-val arg))
;;          (com (viper-getcom arg)))
;;     (if com (viper-move-marker-locally 'viper-com-point (point)))
;;     (if do-paredit-magic
;;         (paredit-magic-forward-Word-kernel val)
;;       (viper-loop val
;;         (viper-skip-nonseparators 'forward)
;;         (viper-skip-separators t)))
;;     (if com (progn
;; 	      (cond ((viper-char-equal com ?c)
;;                      (viper-separator-skipback-special 'twice viper-com-point))
;; 		    ;; Yank words including the whitespace, but not newline
;; 		    ((viper-char-equal com ?y)
;; 		     (viper-separator-skipback-special nil viper-com-point))
;; 		    ((viper-dotable-command-p com)
;; 		     (viper-separator-skipback-special nil viper-com-point)))
;; 	      (viper-execute-com 'viper-forward-Word val com)))))

;; (defadvice vimpulse-end-of-word (around paredit-magic activate)
;;   "Descend into SEXPS first"
;;   (if (or (not paredit-mode)
;;           (not paredit-magic-mode))
;;       (setq ad-return-value ad-do-it)
;;     ;; copy of code from vimpulse-viper-function-redefinitions.el
;;     (viper-leave-region-active)
;;     (let ((val (viper-p-val arg))
;;           (com (viper-getcom arg)))
;;       (paredit-magic-end-of-word-kernel val)
;;       (if com
;;           (viper-execute-com 'viper-end-of-word val com)
;;         (viper-backward-char-carefully)))))

;; (defadvice vimpulse-end-of-Word (around paredit-magic activate)
;;   "Descend into SEXPS first"
;;   (if (or (not paredit-mode)
;;           (not paredit-magic-mode))
;;       (setq ad-return-value ad-do-it)
;;     ;; copy of code from vimpulse-viper-function-redefinitions.el
;;     (viper-leave-region-active)
;;     (let ((val (viper-p-val arg))
;;           (com (viper-getcom arg)))
;;       (paredit-magic-end-of-Word-kernel val)
;;       (if com
;;           (viper-execute-com 'viper-end-of-word val com)
;;         (viper-backward-char-carefully)))))

;; (defadvice vimpulse-delete (around my-paredit-dd activate)
;;   ;; not in paredit or paredit-magic mode
;;   (if (or (not paredit-magic-mode)
;;           (not paredit-mode))
;;       (setq ad-return-value ad-do-it) 
;;     (let ((state (paredit-current-parse-state)))
;;       (cond
;;        ;; inside string or comment
;;        ((or (paredit-in-string-p state)
;;             (paredit-in-comment-p state)
;;             (save-excursion
;;               (back-to-indentation)
;;               (looking-at ";")))
;;         (setq ad-return-value ad-do-it))
;;        ;; normal dd
;;        ((not (memq vimpulse-this-motion-type '(line)))
;;         (setq ad-return-value ad-do-it))
;;        ;; handle deleting beginning of multi-line string
;;        ((save-excursion
;;           (back-to-indentation)
;;           (when (looking-at "\"")
;;             (let ((end-of-string (save-excursion
;;                                    (forward-sexp)
;;                                    (point))))
;;               ;; if string is withing the delete region,
;;               ;; let structural delete handle it as SEXP
;;               (and (> end-of-string (line-end-position))
;;                    (>= end-of-string end)))))
;;         (back-to-indentation)
;;         (forward-char)
;;         (setq beg (point))
;;         (setq ad-return-value ad-do-it))
;;        ;; structural delete
;;        (t (new-my-paredit-kill-line beg end))))))

(defvar my-paredit-kill-line-kill nil)
(defvar my-paredit-kill-line-kill-after nil)

(defun my-paredit-buffer-substring (beg end)
  "Return buffer substring, but skip whitespace in the beginning"
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end)
                (memq (char-after) '(32 9)))
      (forward-char))
    (buffer-substring (point) end)))

(defun new-my-paredit-kill-line (vbeg vend &optional arg)
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
        (kill "")
        (kill-after ""))

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
      (cond 
       ;; Delete comments or whitespace before the SEXP
       ((and this-sexp-start (>= this-sexp-start vend)
             (< (point) vend))
        (setq kill (concat kill (my-paredit-buffer-substring (point) vend)))
        (delete-region (point) vend)
        (ignore-errors
          (indent-sexp)))
       ;; all done
       ((or (>= (point) vend)
            (and this-sexp-start (>= this-sexp-start vend)))
        (setq done t))

       ;; entire SEXP at point fits within the delete region,
       ;; and starts ((. In this case delete only the inner one))
       ((and this-sexp-end (< this-sexp-end vend)
             (looking-at "\\(['`#.+-]?['`#.+-]?(\\)("))
        (goto-char (match-end 1))
        (let ((opoint (point)))
          (forward-sexp)
          (setq kill (concat kill (my-paredit-buffer-substring opoint (point))))
          (delete-region opoint (point))))
       ;; entire SEXP at point fits within the delete region
       ;; and this sexp is the last thing on current line
       ((and this-sexp-end (< this-sexp-end vend)
             (save-excursion
               (goto-char this-sexp-end)
               (looking-at "\\([ \t]*)?\\)*\\(;[^\n]*\\)?\n")))
        ;; we could have skipped a few sexps (because we were not sure
        ;; if we'll have to descend into sub-list or not, so we could
        ;; not delete immediately). Now that we found we did not have
        ;; to descend into sublist, delete from the point we started
        ;; skipping from.
        ;;
        ;; ie (a ^b c d ^^(blah crap)...) point is ^^ and ^ is where we
        ;; may have started from, delete from there..
        (let ((start (or skip-start-point (point))))
          (setq kill (concat kill (my-paredit-buffer-substring start this-sexp-end)))
          (unless skip-start-point)
            (setq kill (concat kill "\n"))
          (delete-region start this-sexp-end)
          (goto-char start)
          (when (looking-at "\\([ \t]*\n\\)[ \t]*")
            ;; (setq kill (concat kill (my-paredit-buffer-substring (match-beginning 1) 
            ;;                                                      (match-end 1))))
            (delete-region (match-beginning 0) (match-end 0))
            (indent-according-to-mode))
          (ignore-errors
            (indent-sexp))))
       ;; this sexp fits withing the region but was not the last thing on current
       ;; line, in this case skip forward, remembering where we started..
       ;; if we finally found sexp that is the last thing on its line, without
       ;; having to descend into lists, we'll delete from the first point that
       ;; we skipped forward from
       ;;
       ;; If we had to descend into lists, the point of descend into the list will
       ;; be the new start point
       ;;
       ;; So pressing dd on (a b c d (e) f (blah ) will delete eventually delete from
       ;; b to (blah). But pressing dd on
       ;;
       ;; (a b c d (e) f (blah foo
       ;;                  bar))
       ;;                  
       ;; will descend into (blah) and not delete  anything before (blah
       ;; 
       ((and this-sexp-end (< this-sexp-end vend))
        (unless skip-start-point
          (setq skip-start-point this-sexp-start))
        (forward-sexp)
        (paredit-point-at-sexp-start))
       (t (let ((ok nil))
            ;; if we have to descend into the list, don't delete
            ;; anything before that
            (setq skip-start-point nil)
            (ignore-errors
              (down-list)
              (setq ok t)
              (when (not (looking-at "[`',]?("))
                (forward-sexp)
                (if (not (looking-at "[ \t]*\\(([ \t]*)\\)?[ \t]*\n"))
                    (paredit-point-at-sexp-start)
                  (paredit-splice-sexp-killing-backward))))
            (if (not ok) (setq done t))))))
    (if (eq last-command 'my-dd-command)
        (kill-new kill t)
      (kill-new kill))
    (setq my-paredit-kill-line-kill kill)
    (indent-according-to-mode)
    (setq this-command 'my-dd-command)))

;; (defun my-paredit-viper-open-line ()
;;   (interactive)

;;   (end-of-line)
;;   (while (and
;;           ;; at closig paren
;;           (looking-back ")")
;;           ;; begins before current line
;;           (< (scan-sexps (point) -1) (point-at-bol)))
;;     (goto-char (1- (point))))
;;   (evil-insert-state 1)
;;   (viper-autoindent))

;; (defadvice viper-open-line (around my-paredit-magic-open-line activate)
;;   (cond ((and paredit-mode
;;               paredit-magic-mode)
;;          (my-paredit-viper-open-line))
;;         ((and c-buffer-is-cc-mode
;;               (fboundp 'c-paredit-viper-open-line))
;;          (setq ad-return-value (or (c-paredit-viper-open-line) ad-do-it)))
;;         (t (setq ad-return-value ad-do-it))))

(defadvice backward-down-list (around keep-line-if-just-entered activate)
  (if (and paredit-mode 
           paredit-magic-mode
           (member last-command '(viper-open-line viper-Open-line))
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

;; (defadvice viper-next-line-at-bol (around my-magic-enter
;;                                           activate)
;;   (if (or (not paredit-magic-mode)
;;           (not (member last-command my-magic-enter-auto-insert)))
;;       (setq ad-return-value ad-do-it)
;;     (evil-insert-state 1)
;;     (viper-autoindent)))

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

(evil-define-key 'motion paredit-mode-map "\C-d" 'my-paredit-duplicate-sexp)
(evil-define-key 'insert paredit-mode-map "\C-d" 'my-paredit-duplicate-sexp)

(defadvice create-scratch-buffer (after enable-paredit activate)
  (paredit-mode t)
  (paredit-magic-mode t))

(add-hook 'emacs-startup-hook (lambda ()
                                (let ((buffer (get-buffer "*scratch*")))
                                  (when buffer
                                    (with-current-buffer buffer
                                      (paredit-mode t)
                                      (paredit-magic-mode t))))))


(require 'eldoc)

;; Fix eldoc space
(eldoc-add-command 'tempo-space 'paredit-forward 'paredit-backward
                   'up-list 'backward-up-list 'down-list
                   'my-paredit-forward-slurp-and-add
                   'paredit-magic-backspace
                   'paredit-magic-close-paren)

(provide 'my-paredit-setup)
