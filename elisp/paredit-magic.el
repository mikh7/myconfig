;;;
;;; More electric commands to speed up Lisp typing
;;;


(defvar paredit-magic-mode nil)
(defvar paredit-magic-pre-marker nil)
(defvar paredit-magic-space-point nil)
(defvar paredit-magic-show-doc nil
  "Show the name of the situation that paredit megic just applied")

(defvar paredit-magic-force-newline nil
  "Where usually paredit-magic smart parentesis closer does not do newlines
such as an consequent of if, forces us to do one. This is bound if situation
is called when user indicated they want newline by their choice of keystroke
ie Enter")

(make-variable-buffer-local 'paredit-magic-space-point)
(make-variable-buffer-local 'paredit-magic-pre-marker)
(make-variable-buffer-local 'paredit-magic-mode)

(defvar paredit-magic-mode-map (make-sparse-keymap))
(add-to-list 'minor-mode-map-alist (cons 'paredit-magic-mode paredit-magic-mode-map))

(define-key paredit-magic-mode-map ")" 'paredit-magic-close-paren)
(define-key paredit-magic-mode-map (kbd "DEL")  'paredit-magic-backspace)
(define-key paredit-magic-mode-map (kbd "<deletechar>") 'paredit-forward-delete)
(define-key paredit-magic-mode-map (kbd "<C-backspace>") 'paredit-backward-kill-word)
(define-key paredit-magic-mode-map (kbd "M-DEL") 'paredit-forward-kill-word)
(define-key paredit-magic-mode-map (kbd "<C-delete>") 'paredit-forward-kill-word)
(define-key paredit-magic-mode-map "*" 'paredit-magic-asterisk)

(defun paredit-magic-asterisk (arg)
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (save-excursion
    (unless (or (paredit-in-string-p)
                (paredit-in-comment-p))
      (when (ignore-errors (paredit-point-at-sexp-start))
        (cond ((looking-at "['`,@]*(")
               (indent-sexp))
              ((not (looking-at ")"))
               (backward-up-list)
               (indent-sexp)))))))

(defvar paredit-magic-in-backspace nil
  "Bound to t when we are inside of paredit-bagic-backspace, so
  that paredit-magic-close-paren handlers know not to close more
  then 1 level of parentesis")

(defvar paredit-magic-close-paren nil
  "A list situations for `paredit-magic-find-situation'")

(defvar paredit-magic-backspace-newline nil
  "A list of situations for `paredit-magic-find-situation'")

(defvar paredit-magic-space-auto-newline nil
  "A list of situations for `paredit-magic-find-situation'")

(defun paredit-magic-backspace (&optional arg)
  (interactive "P")
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p))
      (paredit-backward-delete arg)
    (cond ((looking-back "))[[:space:]\n]+")
           (let ((inhibit-redisplay nil)
                 (paredit-magic-in-backspace t))
             (backward-down-list)
             (paredit-magic-fix-things)
             (backward-down-list)
             (paredit-magic-close-paren)))
          (t (cond
              ((looking-back ")[[:space:]\n]+")
               (delete-region (1+ (match-beginning 0))
                              (point))
               (backward-char 1)
               (unless (looking-back "[[:space:]\n]")
                 (insert " "))
               (let ((situation (paredit-magic-find-situation paredit-magic-backspace-newline)))
                 (when situation
                   (let ((doc (plist-get situation :documentation)))
                     (when (and paredit-magic-show-doc doc)
                       (message "Close paren: %s" doc))
                     (eval (plist-get situation :action)))))
               (paredit-magic-slime-echo-args))
              (t
               (paredit-backward-delete arg)
               (paredit-magic-slime-echo-args)))))))

(setq paredit-magic-close-paren nil)

;; (defadvice evil-paste-after (after paredit-magic-auto-indent activate)
;;   (when (and paredit-magic-mode
;;              (not (paredit-in-string-p))
;;              (not (paredit-in-comment-p)))
;;     (ignore-errors
;;       (save-excursion
;;         (backward-up-list)
;;         (ignore-errors (backward-up-list))
;;         (indent-sexp)))))

;; (defadvice evil-put-back (around paredit-magic-structural-paste activate)
;;   (if (not paredit-magic-mode) (setq ad-return-value ad-do-it)
;;     (let ((val (viper-p-val arg))
;;           (text (if viper-use-register
;;                     (cond ((viper-valid-register viper-use-register '(digit))
;;                            (current-kill
;;                             (- viper-use-register ?1) 'do-not-rotate))
;;                           ((viper-valid-register viper-use-register)
;;                            (get-register (downcase viper-use-register)))
;;                           (t (error viper-InvalidRegister viper-use-register)))
;;                   (current-kill 0)))
;;           sv-point chars-inserted lines-inserted)
;;       (if (null text)
;;           (if viper-use-register
;;               (let ((reg viper-use-register))
;;                 (setq viper-use-register nil)
;;                 (error viper-EmptyRegister reg))
;;             (error "Viper bell")))
;;       (setq viper-use-register nil)
;;       (if (viper-end-with-a-newline-p text)
;;           (progn
;;             (end-of-line)
;;             (while (and
;;                     ;; at closing parenthesis
;;                     (looking-back ")")
;;                     ;; begins before current line
;;                     (< (scan-sexps (point) -1) (point-at-bol)))
;;               (goto-char (1- (point))))
;;             (if (eobp)
;;                 (insert "\n")
;;               (if (and (looking-at ")"))
;;                   (unless (looking-back "\n[ \t]*")
;;                     (insert "\n")
;;                     (indent-according-to-mode))
;;                 (forward-line 1))))
;;         (if (not (eolp)) (viper-forward-char-carefully)))
;;       (paredit-magic-before-yank text)
;;       (set-marker (viper-mark-marker) (point) (current-buffer))
;;       (viper-set-destructive-command
;;        (list 'viper-put-back val nil viper-use-register nil nil))
;;       (setq sv-point (point))
;;       (viper-loop val (viper-yank text))
;;       (setq chars-inserted (abs (- (point) sv-point))
;;             lines-inserted (abs (count-lines (point) sv-point)))
;;       (if (or (> chars-inserted viper-change-notification-threshold)
;;               (> lines-inserted viper-change-notification-threshold))
;;           (unless (viper-is-in-minibuffer)
;;             (message "Inserted %d character(s), %d line(s)"
;;                      chars-inserted lines-inserted))))
;;     ;; Vi puts cursor on the last char when the yanked text doesn't contain a
;;     ;; newline; it leaves the cursor at the beginning when the text contains
;;     ;; a newline
;;     (if (viper-same-line (point) (mark))
;;         (or (= (point) (mark)) (viper-backward-char-carefully))
;;       (exchange-point-and-mark)
;;       (if (bolp)
;;           (back-to-indentation)))
;;     (viper-deactivate-mark)))

(defadvice newline-and-indent (after paredit-magic-auto-indent activate)
  (when (and paredit-magic-mode)
    (cond ((paredit-in-string-p)
           (indent-relative-maybe))
          ((not (paredit-in-comment-p))
           (ignore-errors
             (save-excursion
               (backward-up-list)
               (indent-sexp)))))))

;; (defadvice viper-Put-back (after paredit-magic-auto-indent activate)
;;   (when (and paredit-magic-mode
;;              (not (paredit-in-string-p))
;;              (not (paredit-in-comment-p)))
;;     (ignore-errors
;;       (save-excursion
;;         (backward-up-list)
;;         (ignore-errors (backward-up-list))
;;         (indent-sexp)))
;;     (let ((indent (save-excursion
;;                     (back-to-indentation)
;;                     (point))))
;;       (when (< (point) indent)
;;         (goto-char indent)))))

(defun paredit-magic-forward-delete (&optional arg)
  "Same as `paredit-forward-delete' but passes t to `delete-char'
to save result in the kill ring"
  (interactive "P")
  (cond ((or arg (eobp))
         (delete-char 1 t))
        ((paredit-in-string-p)
         (paredit-forward-delete-in-string))
        ((paredit-in-comment-p)
         ;++ What to do here?  This could move a partial S-expression
         ;++ into a comment and thereby invalidate the file's form,
         ;++ or move random text out of a comment.
         (delete-char 1 t))
        ((paredit-in-char-p)            ; Escape -- delete both chars.
         (backward-delete-char 1 t)
         (delete-char 1 t))
        ((eq (char-after) ?\\)         ; ditto
         (delete-char 2 t))
        ((let ((syn (char-syntax (char-after))))
           (or (eq syn ?\()
               (eq syn ?\")))
         (forward-char))
        ((and (not (paredit-in-char-p (1- (point))))
              (eq (char-syntax (char-after)) ?\))
              (eq (char-before) (matching-paren (char-after))))
         (backward-delete-char 1)       ; Empty list -- delete both
         (delete-char 1))               ;   delimiters.
        ;; Just delete a single character, if it's not a closing
        ;; parenthesis.  (The character literal case is already
        ;; handled by now.)
        ((not (eq (char-syntax (char-after)) ?\)))
         (delete-char 1 t))))

;; (defadvice evil-delete-char ()
;;   (if (not paredit-magic-mode) (setq ad-return-value ad-do-it)
;;     ;; basically replacement of viper-delete-char that
;;     ;; preserves parentesis
;;     (let ((val (viper-p-val arg))
;;           end-del-pos)
;;       (viper-set-destructive-command
;;        (list 'viper-delete-char val nil nil nil nil))
;;       (if (and viper-ex-style-editing
;;                (> val (viper-chars-in-region (point) (viper-line-pos 'end))))
;;           (setq val (viper-chars-in-region (point) (viper-line-pos 'end))))
;;       (if (and viper-ex-style-motion (eolp))
;;           (if (bolp) (error "Viper bell") (setq val 0))) ; not bol---simply back 1 ch
;;       (save-excursion
;;         (viper-forward-char-carefully val)
;;         (setq end-del-pos (point)))
;;       (if viper-use-register
;;           (progn
;;             (cond ((viper-valid-register viper-use-register '((Letter)))
;;                    (viper-append-to-register
;;                     (downcase viper-use-register) (point) end-del-pos))
;;                   ((viper-valid-register viper-use-register)
;;                    (copy-to-register
;;                     viper-use-register (point) end-del-pos nil))
;;                   (t (error viper-InvalidRegister viper-use-register)))
;;             (setq viper-use-register nil)))

;;       (viper-loop val
;;                   (paredit-magic-forward-delete))
;;       (if viper-ex-style-motion
;;           (if (and (eolp) (not (bolp))) (backward-char 1))))))

(defvar mytext nil)

(defun paredit-magic-before-yank (text)
  (when paredit-magic-mode
    (let ((state (paredit-current-parse-state)))
      (unless (or (paredit-in-string-p state)
                  (paredit-in-char-p)
                  (paredit-in-comment-p state))
        (setq mytext text)
        (let ((text-has-newline-p (string-match "\n" text))
              (text-ends-with-paren-p (string-match ")[[:space:]]*$" text))
              (text-ends-with-newline-p (string-match "\n$" text))
              (text-starts-with-newline-p (string-match "^[[:space:]]*\n" text))
              (text-starts-with-paren-p (string-match "^[[:space:]\n]*(" text))
              (point-at-sexp-start-p (looking-at "[[:space:]]*'?("))
              (point-at-sexp-end-p (looking-back ")[[:space:]]*")))
          (cond
           ;; If we pasting before ( and text does not end with
           ;; newline, insert one
           ((and text-has-newline-p     
                 text-ends-with-paren-p 
                 (not text-ends-with-newline-p)
                 point-at-sexp-start-p)
            (save-excursion
              (dotimes (i (if (eql (first state) 0) 2 1))
                (insert ?\n)
                (indent-according-to-mode))))
           ;; Same as above, and text has newline, but we are pasting
           ;; at top level, add extra newline, so pasting
           ((and text-has-newline-p
                 text-ends-with-paren-p
                 point-at-sexp-start-p
                 (eql (first state) 0))
            (save-excursion
              (insert ?\n)
              (indent-according-to-mode)))
           ((and point-at-sexp-end-p
                 (not text-starts-with-newline-p)
                 text-starts-with-paren-p)
            (insert ?\n)
            (indent-according-to-mode))))))))

;; this requires changing viper-yank to defun from defsubst
;; (defadvice viper-yank (around paredit-magic-fix-sexps activate)
;;   (paredit-magic-before-yank text)
;;   (setq ad-return-value ad-do-it))

;; (defadvice insert-for-yank (around paredit-magic-fix-sexps activate)
;;   (paredit-magic-before-yank string)
;;   (setq ad-return-value ad-do-it))

(defadvice cua-paste-pop (after paredit-magic-auto-indent activate)
  (when (and paredit-magic-mode
             (not (paredit-in-string-p))
             (not (paredit-in-comment-p)))
    (ignore-errors
      (save-excursion
        (backward-up-list)
        (indent-sexp)))))

(defadvice evil-join (after paredit-magic-auto-indent activate)
  (when (and paredit-magic-mode
             (not (paredit-in-string-p))
             (not (paredit-in-comment-p)))
    (ignore-errors
      (save-excursion
        (backward-up-list)
        (indent-sexp)))))

(defvar paredit-magic-get-path-maxdepth 5)

(defun paredit-magic-get-path ()
  "Return PATH around the point, each path element is (FUNCTION
POINT-FORM-NUMBER LAST-P SUBFUNCTION), where FUNCTION is the
first element of the list (or NIL) point number is element of the
list that contains point, last-p is if point is in, on, or after
the last element of the list and SUBFUNCTION is the 2nd ATOM of
the list, even if its in the sublist "
  (save-excursion
    (let ((initial-point (or (save-excursion
                               (ignore-errors
                                 (backward-sexp)
                                 (forward-sexp)
                                 (point)))
                             (point)))
          containing-form
          (depth 1)
          function
          (path '())
          done)
      (while (and (not done) (<= depth paredit-magic-get-path-maxdepth)
                  (setq containing-form
                        (ignore-errors
                          (save-excursion (backward-up-list depth)
                                          (point)))))
        (save-excursion 
          (condition-case nil
              (progn
                (parse-partial-sexp containing-form initial-point 1 t)
                (forward-char 1)
                (let (tem function method tentative-defun 2nd)
                  (if (not (looking-at "\\sw\\|\\s_"))
                      ;; This form doesn't seem to start with a symbol
                      (setq function nil method nil)
                    (setq tem (point))
                    (forward-sexp 1)
                    (setq function (downcase (buffer-substring-no-properties
                                              tem (point))))
                    ;; extract 2nd symbol, possible inside a list
                    ;; this is needed for proper identification of 
                    ;; (def something) or (def (something flags)) forms
                    (setq 2nd (save-excursion 
                                (ignore-errors 
                                  (paredit-point-at-sexp-start)
                                  (when (looking-at "(")
                                    (down-list)
                                    (paredit-point-at-sexp-start))
                                  (when (looking-at "\\sw\\|\\s_")
                                    (let ((start (point)))
                                      (forward-sexp)
                                      (intern (downcase (buffer-substring-no-properties
                                                         start (point)))))))))
                    (goto-char tem)
                    (setq tem (intern function)))
                  (let ((n 0) (is-last t))
                    ;; How far into the containing form is the current form?
                    (if (< (point) initial-point)
                        (while (condition-case ()
                                   (progn
                                     (forward-sexp 1)
                                     (if (>= (point) initial-point)
                                         (progn 
                                           (ignore-errors
                                             (if (> (point) initial-point)
                                                 (setq is-last nil))
                                             (forward-sexp 1)
                                             (setq is-last nil))
                                           nil)
                                       (parse-partial-sexp (point)
                                                           initial-point 1 t)
                                       (setq n (1+ n))
                                       t))
                                 (error nil))))
                    (setq path (cons (list tem n is-last 2nd) path)))))
            (error (setq done t))))
        (incf depth))
      path)))

(defun paredit-magic-find-situation (situation-list)
  "Return the situation that matches the LISP forms around the
  point in the current buffer.

  SITUATION-LIST is a list of situations and each situation is a
  property list (plist). `paredit-magic-find-situation' uses
  the :PATH property to match the current context around point

  :PATH property must contain a list of path elements. Each path
  element matches an LISP form The first path element matches the
  outermoust LISP form, and the last path element matches the
  innermost.

  Path elements in themself are property lists, that can contain
  the following properties:

  :FUNCTION - if present, then value must be either a symbol, a
  list in the form of (quote symbol) a list or a lambda form. Path
  element matches if the first element of the lisp form matches the
  symbol or any of the symbols in the list, or if lambda form when
  called with the first element of the lisp form returns
  true. If :FUNCTION property is omitted then path element matches
  any lisp form regardless of the first symbol.

  Please note that the lisp form where the first element is a list
  are considered to have function of NIL, not the value of list: So
  to match the form ((...)) one must use :FUNCTION (QUOTE NIL)

  The (QUOTE SYMBOL) syntax is supported in order to distinguish
  between no property being present (which mean any) or matching a
  function NIL (which means none)

  :SUBFUNCTION - same meaning as the :FUNCTION but identifies
  2nd symbol in the list, even if its in its own sublist...
  For example: (FOO BAR) or (FOO (BAR BAZ)) SUBFUNCTION 
  will be BAR.

  :POSITION - if present, then value is used to match the position
  of the inner form that contains the point in the current form. If
  position is not specified, then path element matches regardless
  of position

  The value can be one of:

  Number - matches exactly this position. For example:
  (:function foo :position 1) matches (foo (first param) (...))
  only if cursor is inside of the (first param) sexp


  (> number) - matches position greater then number

  (< number) - matches position smaller then number
   
  :last      - matches the last position

  Example: if the cursor (represented by |) is in the following
  following form:

  (let ((one)
        (two (three four)
             (five (six seven eight (nine|))))))

  Then the path matching exactly where the cursor is will be:

  '((:function let :position 1) 
    (:function nil :position 2) 
    (:function two :position 2)
    (:function five :position 1) 
    (:function six :position 3) 
    (:function nine :position 1))
  "
  (let* ((paredit-magic-get-path-maxdepth 
          (let ((n 0))
           (dolist (elem situation-list n)
             (setq n (max n (length (plist-get elem :path)))))))
         (path (nreverse (paredit-magic-get-path)))
         (done nil))
    (while (and (not done) situation-list)
      (let* ((elem (pop situation-list))
             (path-elements (reverse (plist-get elem :path))))
        (let ((ok t)
              (path path))
          (while (and ok path-elements path)
            (let ((pat (pop path-elements))
                  (elem (pop path)))
              (setq ok (paredit-magic-path-elem-matches pat elem))))
          (when (and ok (null path-elements))
            (setq done elem)))))
    done))

(defun paredit-magic-path-elem-matches (pattern element)
  "Return t if path element description matches actual path
  element."
  (let ((function (plist-get pattern :function))
        (function-value (car element))
        (subfunction (plist-get pattern :subfunction))
        (subfunction-value (nth 3 element))
        (position (plist-get pattern :position))
        (lastp (plist-get pattern :lastp)))
    (and
     (cond 
      ((null function) t)              ; not present meants don't care
      ((symbolp function)
       (eq function function-value))
      ((and (consp function) (eq (car function) 'quote))
       (eq (cadr function) function-value))
      ((and (consp function) (eq (car function) 'lambda)) ; lambda
       (let ((result (funcall function function-value)))
         ;; (log-sexp result)
         result))
      ((consp function)
       (member function-value function)))
     (cond 
      ((null subfunction) t)              ; not present meants don't care
      ((symbolp subfunction)
       (eq subfunction subfunction-value))
      ((and (consp subfunction) (eq (car subfunction) 'quote))
       (eq (cadr subfunction) subfunction-value))
      ((and (consp subfunction) (eq (car subfunction) 'lambda)) ; lambda
       (funcall subfunction subfunction-value))
      ((consp subfunction)
       (member subfunction-value subfunction)))
     (cond 
      ((null position) t)               ; not present means dont care
      ((numberp position)               ; just a number
       (= position (cadr element)))
      ((and (consp position)            ; (> number)
            (eq (car position) '>)
            (numberp (cadr position)))
       (> (cadr element) (cadr position)))
      ((and (consp position)            ; (< number)
            (eq (car position) '<)
            (numberp (cadr position)))
       (< (cadr element) (cadr position)))
      ((and (consp position) (eq (car position) 'lambda)) ; lambda
       (funcall position (cadr element)))
      ((eq position :last)              ; last one
       (caddr element)))
     (or (not lastp)
         (caddr element)))))

(defun def-situation (situation-list-var &rest situation)
  "Adds situation to the list of sitations stored in situation-list-var variable.

  SITUATION-LIST-VAR - A symbol in which value list of situations
  is stored.  
  SITUATION - A property list, must contain :path property"
  (let ((path (plist-get situation :path)))
    (or path (error ":path property not found in the situation plist"))
    (let ((old nil)
          (ptr (symbol-value situation-list-var)))
      (while (and ptr (not (equal (plist-get (car ptr) :path)
                                  path)))
        (setq ptr (cdr ptr)))
      (if ptr
        (setcar ptr situation)
        (set situation-list-var (cons situation (symbol-value situation-list-var)))))))

(setq paredit-magic-close-paren nil)

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (let let* bind bind*) :position 1)
    (:function 'nil)
    ()
    ())
  :documentation "Close parentesis in (let ((var (foo param|))))
  automatically advance to next variable"
  :action '(progn
             (paredit-move-past-close-and-reindent)
             (unless paredit-magic-in-backspace
               (paredit-magic-close-paren-and-newline))))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (bind bind*) :position 1)
    (:function 'nil)
    ()
    (:function :values))
  :documentation "Close parentesis in (bind (((:values one
two|))) need to be present so that more generic situation does
not close parentethis twice"
  :action '(progn
             (cond ((> (current-column) 50)
                    (let ((blink-matching-paren nil))
                      (paredit-magic-close-paren-and-newline))
                    (unless paredit-magic-in-backspace
                      (paredit-open-parenthesis)))
                   (t (paredit-move-past-close-and-reindent)
                      (unless paredit-magic-in-backspace
                        (paredit-open-parenthesis nil))))))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (let let* bind bind*) :position 1)
    ())
  :documentation "Close parentesis in (let (var1 var2 |))
  automatically advance to the implied progn"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (let let* bind bind*) :position 1)
    (:function 'nil)
    ())
  :documentation "Close parentesis in (let ((var ...|)))
  automatically advance to next variable"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (let let* bind bind*) :position 1)
    (:function 'nil))
  :documentation "Close parentesis in (let ((var ...)|))
  automatically do newline and advance to the start of implied progn"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))


(defun paredit-magic-show-path ()
  (interactive)
  (let ((path (paredit-magic-get-path)))
    (message "Path: %s" path)))

(defun paredit-magic-show-situation ()
  (interactive)
  (let ((situation (paredit-magic-find-situation paredit-magic-close-paren)))
    (message "Found %s" situation)))

(setq paredit-magic-backspace-newline nil)

(def-situation 'paredit-magic-backspace-newline
  :path '((:function (if aif) :position (> 1)))
  :action '(newline-and-indent))

(def-situation 'paredit-magic-backspace-newline
  :path '((:function
           (lambda (function)
             (memq function paredit-magic-body0-forms))))
  :action '(newline-and-indent))

(def-situation 'paredit-magic-backspace-newline
  :path '((:function
           (lambda (function)
             (memq function paredit-magic-body1-forms))
           :position (> 1)))
  :action '(newline-and-indent))

(def-situation 'paredit-magic-backspace-newline
  :path '((:function
           (lambda (function)
             (memq function paredit-magic-body2-forms))
           :position (> 2)))
  :action '(newline-and-indent))

(def-situation 'paredit-magic-backspace-newline
  :path 
  '((:function (cond acond))
    nil)
  :documentation "When backspacing into  (cond (EXP )|) position
point on the next line after EXP"
  :action '(newline-and-indent))

(def-situation 'paredit-magic-close-paren
  :path '((:function if :position 2) nil)
  :documentation "Close parentesis in (if (...) (...|))
  autamatically do newline for the else clause"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path '((:function if :position 1))
  :documentation "Close parentesis in (if (...|) (...))
  autamatically do newline, when `paredit-magic-force-newline' is set"
  :action '(if paredit-magic-force-newline
               (paredit-magic-close-paren-and-newline)
             (paredit-close-parenthesis)))

(def-situation 'paredit-magic-close-paren
  :path '((:function (lambda (symbol)
                       (and (eq major-mode 'emacs-lisp-mode)
                            (member symbol '(if aif)))) :position (> 2)) nil)
  :documentation "Close parentesis in (if (COND) (THEN) (ELSE1) (ELSE2 ..|))
  in emacs lisp style if statement (with multiple ELSE clauses allowed)
  do newline for the next ELSE clause"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path '((:function (lambda (symbol)
                       (and (not (eq major-mode 'emacs-lisp-mode))
                            (eq symbol 'if))) :position (> 2)) nil)
  :documentation "Close parentesis in (if (COND) (THEN) (ELSE|)))
  in non-emacs lisp style if statement (with single ELSE clause allowed)
  automatically close two parestesis and advance to after the if statement"
  :action '(progn
             (paredit-move-past-close-and-reindent)
             (paredit-magic-close-paren-and-newline)))

(defconst paredit-magic-body0-forms
  '(progn loop iterate iter ignore-errors tagbody save-buffer save-current-buffer save-excursion
          save-frame-config save-match-data
          log-indented restart-case
          save-restriction save-restriction-if-possible 
          save-selected-frame save-selected-window save-window-excursion)
  "List of PROGN-like forms. Used to auto-newline after closing sub-forms")

(defconst paredit-magic-body1-forms
  '(let let* bind labels flet when awhen when-let unless while defun defadvice 
        defmacro defmethod defgeneric
        deftest defpackage defsystem
        prog1 log-indented-by
        print-unreadable-object dolist dotimes
        with-current-buffer
        with-typed-slots
        eval-when
        do do*)
  "List of LET like forms, where body starts at the third sub-form")

(defconst paredit-magic-body2-forms
  '(with-slots with-accessors multiple-value-bind destructuring-bind)
  "List of MULTIPLE-VALUE-BIND like forms, where body starts with the 3rd sub-form")

(def-situation 'paredit-magic-close-paren
  :path `((:function (lambda (function)
                       (or
                        (memq function paredit-magic-body0-forms)
                        (and (string-match "^with-" (symbol-name function))
                             (not (memq function paredit-magic-body1-forms))
                             (not (memq function paredit-magic-body2-forms))))))
          ())
  :documentation "Close parentesis in (progn-equivalent (foo |))
  automatically do new line"
  :action `(progn (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path `((:function (lambda (function)
                       (or
                        (memq function paredit-magic-body1-forms)
                        (and (string-match "^with-" (symbol-name function))
                             (not (memq function paredit-magic-body2-forms)))))
                     :position (> 0))
          ())
  :documentation "Close parentesis in (prog1-equivalent x (foo |))
automatically do new line"
  :action `(progn (paredit-magic-close-paren-and-newline)))

;; better description
;; :path (@paredit-magic-body2-forms x @[*])
(def-situation 'paredit-magic-close-paren
  :path `((:function (lambda (function)
                       (memq function paredit-magic-body2-forms))
                     :position (> 1))
          ())
  :documentation "Close parentesis in (multiple-value-bind (...|) obj (...|))
automatically do new line"
  :action `(progn (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path `((:function def :position (> 1))
          ())
  :documentation "Close parentesis in (def ... (...|))
automatically do new line"
  :action `(progn (paredit-magic-close-paren-and-newline)))

;; better description
;; :path (labels|flet ((x *)))

(def-situation 'paredit-magic-close-paren
  :path '((:function (labels flet)
                     :position 1)
          (:function (lambda (sym)
                       (eq sym nil)))
          (:position (> 0))
          ())
  :documentation "Close parentisis and newline (labels|flet ((foo (|) .. .. (bar ..|))))"
  :action `(progn (paredit-magic-close-paren-and-newline)))

;; better description
;; :path (labels|flet (*))
(def-situation 'paredit-magic-close-paren
  :path '((:function (labels flet)
                     :position 1)
          (:function (lambda (sym)
                       (eq sym nil)))
          nil)
  :documentation "Close parentisis and newline (labels|flet ((foo () ...)|)))"
  :action `(progn (paredit-magic-close-paren-and-newline)))

(defun paredit-magic-move-past-close-and-newline (close)
  (cond ((or (paredit-in-string-p)
             (paredit-in-comment-p))
         (insert close))
        (t (if (paredit-in-char-p) (forward-char))
           (paredit-move-past-close-and-reindent)
           (let ((comment.point (paredit-find-comment-on-line)))
             (if (looking-at "[ \t]*\n[ \t]*\n")
                 (progn (forward-line)
                        (lisp-indent-line))
               (newline))
             (if comment.point
                 (save-excursion
                   (forward-line -1)
                   (end-of-line)
                   (indent-to (cdr comment.point))
                   (insert (car comment.point)))))
           (lisp-indent-line)
           (paredit-ignore-sexp-errors (indent-sexp))
           (paredit-blink-paren-match t))))

(defun paredit-magic-close-paren-and-newline ()
  "Close parentesis and newline. Do not add newline if there is already empty line there"
  (interactive)
  (paredit-magic-move-past-close-and-newline ?\)))


(defun paredit-magic-slime-echo-args ()
  "Echo slime arglist if slime is active and we are in lisp
buffer. Called after moving past close parenthesis and other
structure move functions"
  (when (or (eq major-mode 'slime-repl-mode)
            (and (eq major-mode 'lisp-mode)
                 (boundp 'slime-mode)
                 slime-mode))
    (when (slime-background-activities-enabled-p)
      (slime-echo-arglist))))

(defun paredit-magic-close-paren (&optional arg)
  (interactive)
  (let ((close ?\))
        (situation))
    (cond ((or (paredit-in-string-p)
               (paredit-in-comment-p))
           (insert close))
          ((setq situation (paredit-magic-find-situation paredit-magic-close-paren))
           (let ((doc (plist-get situation :documentation)))
             (when (and paredit-magic-show-doc doc)
               (message "Close paren: %s" doc))
             (eval (plist-get situation :action))
             (paredit-magic-slime-echo-args)))
          ((not (paredit-in-char-p))
           ;; if we are at the line containing only closing newlines
           (if (save-excursion
                 (beginning-of-line)
                 (looking-at "^[[:space:]]*)"))
               (paredit-move-past-close-and-newline close)
             (paredit-move-past-close-and-reindent)
             (setq paredit-magic-space-point (point-marker))
             (insert " ")
             (paredit-magic-slime-echo-args)
             (paredit-blink-paren-match t))))))

(defun paredit-magic-post-command-hook ()
  (when paredit-magic-mode
    (condition-case nil
	(paredit-magic-post-command-hook-1)
      (error nil))))

(defun paredit-magic-pre-command-hook ()
  (when paredit-magic-mode
    (condition-case nil
	(paredit-magic-pre-command-hook-1)
      (error nil))))

(defun paredit-magic-after-change-hook (start end old-len)
  (when paredit-magic-mode
    (condition-case nil
	(paredit-magic-after-change-hook-1 start end old-len)
      (error nil))))

(defvar paredit-magic-fix-depth 0)
(make-variable-buffer-local 'paredit-magic-fix-depth)

(defun paredit-magic-for-each-sexp (function)
  (funcall function)
  (let ((end-of-sexp (ignore-errors 
                       (save-excursion 
                         (forward-sexp)
                         (point))))
        (down-list (ignore-errors (down-list)
                                  (point)))
        (paredit-magic-fix-depth (if paredit-magic-fix-depth (1+ paredit-magic-fix-depth)
                                   0)))
    (when ;; its a nested list
        (and down-list (< down-list end-of-sexp))
      (let ((done)
            (prev-point))
        (while (not done)
          (save-excursion
            (paredit-magic-for-each-sexp function))
          (setq done (or (eobp) (null (ignore-errors (forward-sexp)
 (point)))
                         (equal (point) prev-point)))
          (setq prev-point (point)))))))

(defmacro when-point-not-between (point start end &rest body)
  `(let ((point ,point)
         (start ,start)
         (end ,end))
     (when (or (< point start)
               (> point end))
       ;; (message "Here start=%s point=%s end=%s expr = %s" 
       ;;          start point end (or (< start point)
       ;;                              (> end point)))
       ,@body)))


(defun paredit-magic-looking-at-body ()
  (looking-at "(\\(defun\\|defadvice\\|defmethod\\|def[a-z-]+\\|progn\\|save-excursion\\|let\\|when\\|if\\|ignore-errors\\|while\\|loop\\|save-restriction\\|save-match-data\\|with-[a-z-]+\\|dolist\\|dotimes\\|do\\|unless\\)"))

(defun paredit-magic-fix-one-line ()
  ;; (message "at %s" (buffer-substring-no-properties 
  ;;                   (point) (+ 5 (point))))
  (cond 
   ;; ( sexp) removal of space
   ((looking-at "(\\([ \t]+\\)\\(\\sw\\|\\s_\\|(\\|\"\\)")
    (when-point-not-between 
     pt (match-beginning 1) (match-end 1)
     (delete-region (match-beginning 1) (match-end 1))))
   ;; ((...) ) 
   ;; ((...)
   ;;  )
   ;; removal of space/newlines between closing parentesis
   ((looking-at "\\([ \t\n]+\\))")
    (when-point-not-between 
     pt (match-beginning 1) (match-end 1) 
     (delete-region (match-beginning 1) (match-end 1))))))


(defun paredit-magic-fix-things (&optional where)
  (when (buffer-modified-p)
    (or where (setq where (point)))
    (let ((pt (point-marker))
          start end state)
      (save-excursion
        (beginning-of-defun)
        ;; start of fix region is 2 lines before the point
        ;; unless defun starts earlier
        (setq start (max (point) (save-excursion
                                   (goto-char pt)
                                   (forward-line -2)
                                   (beginning-of-line)
                                   (paredit-skip-whitespace t)
                                   (point))))
        (setq end   
              (save-excursion
                (goto-char pt)
                (forward-line 2)
                (end-of-line)
                (point)))
        (goto-char start)
        (back-to-indentation)
        (paredit-magic-fix-one-line)
        (while (and (<= (point) end) 
                    (not (eobp)))
          (let ((state (paredit-current-parse-state)))
            (cond ((or (paredit-in-comment-p state) 
                       (looking-at ";"))
                   (beginning-of-line 2)
                   (paredit-skip-whitespace t))
                  ((paredit-in-char-p)
                   (forward-char)
                   (paredit-skip-whitespace t))
                  ((paredit-in-string-p state)
                   (let ((start+end (paredit-string-start+end-points state)))
                     (cond ((cdr start+end)
                            (goto-char (cdr start+end))
                            (forward-char))
                           (t
                            (paredit-forward)
                            (paredit-skip-whitespace t)))))
                  ((looking-at "['`@,]*(")
                   (paredit-magic-fix-one-line)
                   (down-list)
                   (paredit-magic-fix-one-line)
                   (paredit-skip-whitespace t))
                  (t
                   (paredit-forward)
                   (paredit-magic-fix-one-line)
                   (paredit-skip-whitespace t)))))))))


(defvar paredit-magic-last-command-point nil)
(make-variable-buffer-local 'paredit-magic-last-command-point)

(defun paredit-magic-post-command-hook-1 ()
  (paredit-magic-fix-things paredit-magic-last-command-point))

(defun paredit-magic-pre-command-hook-1 ()
  (setq paredit-magic-last-command-point (point)))

(defun paredit-magic-after-change-hook-1 (start end old-len))

;; (add-hook 'after-change-functions 'paredit-magic-after-change-hook)

(defun paredit-magic-mode (&optional arg)
  (interactive)
  (setq paredit-magic-mode (or arg (not paredit-magic-mode)))
  (message "Paredit magic mode %s" (if paredit-magic-mode "enabled" "disabled"))
  (add-hook 'post-command-hook 'paredit-magic-post-command-hook)
  (add-hook 'pre-command-hook 'paredit-magic-pre-command-hook))


(defun my-paredit-forward-slurp-and-add (&optional arg)
  (interactive)
  (my-paredit-forward-slurp-sexp arg)
  (backward-up-list)
  (paredit-forward)
  (backward-down-list)
  (when (and (boundp viper-current-state)
             (eq viper-current-statie 'insert-state))
    (evil-insert-state 1))
  (unless (looking-back "[[:space:]]")
    (insert " ")))


(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (case ecase switch typecase etypecase acase aecase aetypecase) :position (> 0))
    nil)
  :documentation "Close parentesis in (case|ecase (value (...)|))
automatically do newline and advance to the start of implied progn"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (case ecase switch typecase etypecase acase aecase aetypecase) :position (> 1))
    nil
    nil)
  :documentation "Close parentesis in (case|ecase (value (...|)))
automatically do newline and advance to the start of implied progn"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))


(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (case ecase switch typecase etypecase acase aecase aetypecase)
               :position 1
               :lastp t))
  :documentation "Auto-newline instead of closing parentesis when pressing ) in the (case obj|) situation"
  :action '(unless (looking-back "^[ \t]*")
             (newline-and-indent)))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (lambda (x)
                 (member x paredit-magic-body2-forms))
               :position 2
               :lastp t))
  :documentation "Auto-newline instead of closing parentesis when pressing ) in the body2 forms after the object"
  :action '(unless (looking-back "^[ \t]*")
             (newline-and-indent)))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (cond acond))
    nil)
  :documentation "Close parentesis in (cond (exp (...)|))
automatically do newline and advance to the start of implied progn"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (cond acond))
    nil
    nil)
  :documentation "Close parentesis in (cond (exp (...|)))
automatically do newline and advance to the start of implied progn"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis in defclass slot does auto-newline"
  :path '((:function defclass :position 3)
          nil
          nil)
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis in def class slot does auto-newline"
  :path '((:function def :subfunction class :position 4)
          nil
          nil)
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis in defclass base classes list"
  :path '((:function defclass :position 2)
          nil)
  :action '(progn
             (paredit-magic-close-paren-and-newline)))


(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis in defstruct slot does auto-newline"
  :path '((:function defstruct :position (> 1))
          nil)
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis in def struct slot does auto-newline"
  :path '((:function def :subfunction struct :position (> 2))
          nil)
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis in def struct slot does auto-newline"
  :path '((:function handler-case :position (> 0))
          nil)
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis (handler-case FORM (handler (|))"
  :path '((:function handler-case :position (> 1))
          nil
          nil)
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :documentation "Close parentesis in the inline (:method) inside of defgeneric form"
  :path '((:function :method :position (> 0))
          nil)
  :action '(paredit-magic-close-paren-and-newline))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (do do*) :position 1)
    (:function 'nil)
    ())
  :documentation "Close parentesis in (do ((var init (next () last|))))
 automatically advance to next variable"
  :action '(progn
             (paredit-move-past-close-and-reindent)
             (unless paredit-magic-in-backspace
               (paredit-magic-close-paren-and-newline))))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (do do*) :position 1)
    (:function 'nil)
    ())
  :documentation "Close parentesis in (do ((var init next|))))
  automatically advance to next variable"
  :action '(progn
             (paredit-magic-close-paren-and-newline)))

(def-situation 'paredit-magic-close-paren
  :path 
  '((:function (do do*) :position 2)
    ()
    ())
  :documentation "Close parentesis in (do (vars...) (cond (result (foo bar|)))
 automatically advance to next variable"
  :action '(progn
             (paredit-move-past-close-and-reindent)
             (unless paredit-magic-in-backspace
               (paredit-magic-close-paren-and-newline))))

(provide 'paredit-magic)


