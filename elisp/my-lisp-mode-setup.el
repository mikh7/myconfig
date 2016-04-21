
;;;
;;; Lisp indentation setup
;;;
(load "cl-indent")

;; Inside of defclass :documentation resets indent on next line to beginning of line
;; (put 'defclass 'common-lisp-indent-function 'my-documentation-indent-hack)

(defun my-documentation-indent-hack (path state indent-point
                                          sexp-column normal-indent)
  (cond ((save-excursion
           (goto-char indent-point)
           (back-to-indentation)
           (or
            (and (looking-at "\"")
                 (save-excursion
                   (backward-sexp)
                   (looking-at ":documentation")))
            (looking-at ":documentation")))
         (1+ sexp-column))
        (t (lisp-indent-259
            '(6 4 (&whole 2 &rest 1) (&whole 2 &rest 1)) path state indent-point
            sexp-column normal-indent))))

(put 'defstruct-early 'common-lisp-indent-function 
     (get 'defstruct 'common-lisp-indent-function))

(put 'defclass-early 'common-lisp-indent-function 
     (get 'defclass 'common-lisp-indent-function))

(put 'bind 'common-lisp-indent-function 
     '((&whole 4 &rest (&whole 1 1 2)) &body))

(put 'iter 'common-lisp-indent-function '(2 &rest 2))

(put 'catch-case 'common-lisp-indent-function 
     (get 'handler-bind 'common-lisp-indent-function))

(put 'make-instance 'common-lisp-indent-function
     '(1 &rest 1))

(put 'with-slots 'common-lisp-indent-function
     '((&whole 6 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
               &rest 1) 4 &body))

(put :module 'common-lisp-indent-function
     '(4 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 &rest 1))

(font-lock-add-keywords 'lisp-mode lisp-font-lock-keywords-2 'set)


;; Highlighting of flet/labels/macrolet local functions/macros with
;; font-lock-function-name-face

(defun mm/highlight-binding-like-form (bound form-regexp 
                                             &optional
                                             include-paren
                                             object-in-second-form-p
                                             object-include-paren)
  "Matches the LET like list in the form of (FORM-REGEXP (BINDINGS) [<OBJECT>] ...).
Each BINDING is either a symbol which is taken as binding name,
or a list in a form of (BINDING-NAME &rest ignored)

Will setup match data as follows:
  0. Opening parentesis and the form name
  1. Form name
  2. Optional 2nd SEXP in the form, if OBJECT-IN-SECOND-FORM-P was non-NIL
  3. Rest  of the match elements will be the binding names
"
  (when (re-search-forward form-regexp bound t)
    (let ((names '())
          (all-start (match-beginning 0))
          (all-end (match-end 0))
          (kw-start (match-beginning 1))
          (kw-end (match-end 1))
          2nd-form-start 2nd-form-end
          (parse-sexp-ignore-comments t))
      (catch 'done
        (condition-case e
            (save-excursion 
              ;; go inside the bindings list
              (goto-char (scan-lists all-end 1 -1))
              (while t
                (let* ((next (scan-sexps (point) 1))
                       (prev (goto-char (scan-sexps next -1)))
                       name-start
                       name-end)
                  (if (not (looking-at "[`',@]*("))
                      (setq name-start (point)
                            name-end next)
                    ;; down into the definition
                    (goto-char (scan-lists (point) 1 -1))
                    (setq name-end (scan-sexps (point) 1)
                          name-start (scan-sexps name-end -1))
                    (when (and (not include-paren)
                               (= ?\((char-after name-start)))
                      (decf name-end)
                      (incf name-start)))
                  (push name-start names)
                  (push name-end names)
                  ;; advance to the next local function
                  (goto-char next))))
          (error
           ;; (message "got error %s" e)
           (throw 'done nil))))
      (when object-in-second-form-p
        (ignore-errors
          (setq 2nd-form-end (scan-sexps (point) 2))
          (setq 2nd-form-start (scan-sexps 2nd-form-end -1))
          (goto-char 2nd-form-start)
          (when (and (looking-at "\\s(")
                     (not object-include-paren))
            (incf 2nd-form-start))
          (goto-char 2nd-form-end)
          (when (and (looking-back "\\s)")
                     (not object-include-paren))
            (decf 2nd-form-end))))
      (set-match-data (append
                       (list all-start all-end
                             kw-start kw-end)
                       (and object-in-second-form-p
                            (list 2nd-form-start 2nd-form-end))
                       (nreverse names)
                       (list (current-buffer))))
      (goto-char all-end)
      t)))

(defun mm/highlight-case-like-form (bound form-regexp
                                          have-obj
                                          include-paren-in-obj
                                          include-paren-in-cond
                                          include-paren-in-consequent)
  "Matches CASE like form and sets match data as follows:

  0: opening parentesis and form name
  1: form name
  2: (optional when have-obj is non-NIL) First s-exp after form name
  3/4: first sexp and rest of ELSE branch, which is found by having CAR of the subform being T,
       will be NIL if no ELSE branch was found
  5/6: first element and rest of the first case form
  6/7..8/9... rest of the case forms
  "
  (when (re-search-forward form-regexp bound t)
    (let* ((all-start (match-beginning 0))
           (all-end (match-end 0))
           (kw-start (match-beginning 1))
           (kw-end (match-end 1))
           obj-form-start obj-form-end
           else-branch
           (subforms '())
           (parse-sexp-ignore-comments t))
      (catch 'done
        (condition-case e
            (save-excursion 
              (when have-obj 
                (setq obj-form-end (scan-sexps (point) 1)) 
                (setq obj-form-start (scan-sexps obj-form-end -1)) 
                (goto-char obj-form-start)
                (when (and (looking-at "\\s(")
                           (not include-paren-in-obj))
                  (incf obj-form-start))
                (goto-char obj-form-end) 
                (when (and (looking-back "\\s)")
                           (not include-paren-in-obj))
                  (decf obj-form-end))) 
              (while t
                (let ((next (scan-sexps (point) 1)))
                  ;; down into the definition
                  (goto-char (scan-lists (point) 1 -1)) 
                  (let* ((name-end (scan-sexps (point) 1))
                         (name-start (scan-sexps name-end -1))
                         (tmp name-end)
                         cons-start
                         cons-end
                         else-branch-p)
                    (goto-char name-start)
                    (setq else-branch-p (looking-at "[tT]\\>"))
                    (when (and (looking-at "[`',@]*(")
                               (not include-paren-in-cond))
                      (goto-char (scan-lists (point) 1 -1))
                      (setq name-start (point))
                      (goto-char name-end)
                      (when (looking-at "\\s)")
                        (decf name-end)))
                    (cond ((not else-branch-p) 
                           (push name-start subforms) 
                           (push name-end subforms))
                          (t (when else-branch
                               ;; if we have duplicate else branch
                               ;; highlight previous one as subform
                               (setq subforms (nconc else-branch subforms)
                                     else-branch nil))
                             (push name-start else-branch)
                             (push name-end else-branch)))
                    ;; do the 2nd expr
                    (goto-char tmp)
                    (ignore-errors 
                      (if (looking-at "[ \t]\n")
                          (progn (forward-line) 
                                 (beginning-of-line)
                                 (setq cons-start (point)))
                        (forward-sexp)
                        (backward-sexp)))
                    (setq cons-start (point))
                    (goto-char next)
                    (setq cons-end (scan-lists (point) -1 -1))
                    (setq cons-start (min cons-start cons-end))
                    (when (= cons-start cons-end)
                      (setq cons-start nil cons-end nil))
                    (cond ((not else-branch-p) 
                           (push cons-start subforms) 
                           (push cons-end subforms))
                          (t (push cons-start else-branch) 
                             (push cons-end else-branch)))))))
          (error
           ;; (message "got error %s" e)
           (throw 'done nil))))
      (set-match-data (append
                       (list all-start all-end
                             kw-start kw-end)
                       (and have-obj (list obj-form-start obj-form-end))
                       (if else-branch (nreverse else-branch)
                         `(nil nil nil nil))
                       (nreverse subforms)
                       (list (current-buffer))))
      (goto-char all-end)
      t)))

(defun mm/highlight-obj+rest (bound form-regexp
                                    have-obj1
                                    include-paren-in-obj1
                                    have-obj2
                                    include-paren-in-obj2)
  "Matches a form with optional 2nd s-exp, and rest of subforms.

  0: opening parentesis and form name
  1: form name
  2: (optional when have-obj1 is non-NIL) First s-exp after form name
  2: (optional when have-obj2 is non-NIL) Second s-exp after form name
  3: rest of the subforms
  "
  (when (re-search-forward form-regexp bound t)
    (let* ((all-start (match-beginning 0))
           (all-end (match-end 0))
           (kw-start (match-beginning 1))
           (kw-end (match-end 1))
           obj1-form-start obj1-form-end
           obj2-form-start obj2-form-end
           rest-start rest-end
           (parse-sexp-ignore-comments t))
      (catch 'done
        (condition-case e
            (save-excursion 
              (when have-obj1 
                (setq obj1-form-end (scan-sexps (point) 1)) 
                (setq obj1-form-start (scan-sexps obj1-form-end -1)) 
                (goto-char obj1-form-start)
                (when (and (looking-at "\\s(")
                           (not include-paren-in-obj1))
                  (incf obj1-form-start))
                (goto-char obj1-form-end) 
                (when (and (looking-back "\\s)")
                           (not include-paren-in-obj1))
                  (decf obj1-form-end)))
              (when have-obj2 
                (setq obj2-form-end (scan-sexps (point) 1)) 
                (setq obj2-form-start (scan-sexps obj2-form-end -1)) 
                (goto-char obj2-form-start)
                (when (and (looking-at "\\s(")
                           (not include-paren-in-obj2))
                  (incf obj2-form-start))
                (goto-char obj2-form-end) 
                (when (and (looking-back "\\s)")
                           (not include-paren-in-obj2))
                  (decf obj2-form-end))) 
              (let ((tmp (scan-sexps (point) 1))) 
                (goto-char tmp)
                (setq rest-start (scan-sexps (point) -1))
                (setq rest-end (1- (scan-lists (point) 1 1)))
                (goto-char all-end)
                (setq rest)))
          (error
           ;; (message "got error %s" e)
           (throw 'done nil))))
      (set-match-data (append
                       (list all-start all-end
                             kw-start kw-end)
                       (and have-obj1 (list obj1-form-start obj1-form-end))
                       (and have-obj2 (list obj2-form-start obj2-form-end))
                       (list rest-start rest-end)
                       (list (current-buffer))))
      (goto-char all-end)
      t)))


(defun mm/match-labels (bound)
  (mm/highlight-binding-like-form bound "(\\<\\(labels\\|flet\\|macrolet\\)\\>" nil))

(defun mm/match-bind (bound)
  (mm/highlight-binding-like-form bound "(\\_<\\(let\\*?\\|bind\\|symbol-macrolet\\)\\_>" nil))

(defun mm/match-bind-with-object (bound)
  (mm/highlight-binding-like-form bound "(\\_<\\(with-accessors\\|with-slots\\|multiple-value-bind\\)\\_>" nil t t))

(defun mm/match-conds (bound)
  (mm/highlight-case-like-form bound "(\\<\\(cond\\|acond\\)\\>" nil nil t t))

(defun mm/match-cases (bound)
  (mm/highlight-case-like-form bound "(\\<\\(case\\|typecase\\|ecase\\|etypecase\\|switch\\)\\>" t t t t))

(defun mm/match-if (bound)
  (mm/highlight-obj+rest bound "(\\_<\\(if\\)\\_>" t t t t))

(defun mm/match-when (bound)
  (mm/highlight-obj+rest bound "(\\_<\\(when\\|unless\\)\\_>" t t nil nil))

(defface mm/bold '((t (:weight extra-bold))) "Face to display shit")
(defface mm/condition '((t (:background "LightCyan"))) "Face to display shit")
(defface mm/consequent '((t (:background "HoneyDew2"))) "Face to display shit")
(defface mm/otherwise '((t (:background "#E544E5E5D570"))) "Face to display shit")

(defun mm/font-lock-update-keyword (mode keyword &optional how)
  "Like `font-lock-add-keywords' but uses CAR of each keyword to compare"
  (or mode (error "Mode is required"))
  (let ((spec (cons (list keyword) how)) cell)
    (if (setq cell (assq mode font-lock-keywords-alist))
        (if (eq how 'set)
            (setcdr cell (list spec))
          (let ((removed (remove* (first keyword) (cdr cell) :key 'caaar :test-not 'equal))) 
            (dolist (elem removed)
              (font-lock-remove-keywords mode (car elem)))
            (setcdr cell (append
                          (remove* (first keyword) (cdr cell) :key 'caaar)
                          (list spec))))) 
      (push (list mode spec) font-lock-keywords-alist))
    ;; Make sure that `font-lock-removed-keywords-alist' does not
    ;; contain the new keywords.
    (font-lock-update-removed-keyword-alist mode (list keyword) how)))

(defun mm/apply-highlight (match face fancy merge)
  ;; (log-sexp match (match-beginning 4) (match-end 4))
  (if (not fancy) (font-lock-apply-highlight (list match face merge t)) 
    (let ((start (match-beginning match))
          (end (match-end match))
          (outer-form-start (match-beginning 0))
          (object nil)
          (prop 'face)
          next prev
          tmp
          (val (list face))
          (col (when (eq fancy 'rect)
                 (save-excursion
                   (goto-char start)
                   (current-column))))
          (do-col (member fancy '(rect first-indent-rect))))
      (while (/= start end)
        (setq next (next-single-property-change start prop object
                                                (min end
                                                     (save-excursion
                                                       (goto-char start)
                                                       (line-end-position))))
              prev (get-text-property start prop object))
        ;; Canonicalize old forms of face property.
        (and (memq prop '(face font-lock-face))
             (listp prev)
             (or (keywordp (car prev))
                 (memq (car prev) '(foreground-color background-color)))
             (setq prev (list prev)))
        ;; (setq tmp (min next
        ;;                (save-excursion
        ;;                  (goto-char start)
        ;;                  (line-end-position))))
        (let* ((their-outer-start (get-text-property start :outer-form-start))
               (merge (if (not (eq merge 'prepend-if-nested)) merge
                        (if (and their-outer-start
                                 (< outer-form-start their-outer-start))
                            'append 'prepend)))) 
          ;; (log-sexp start next val merge outer-form-start)
          (cond ((eq merge 'prepend)
                 ;; (log-sexp start next val merge)
                 (add-text-properties start next 
                                      (list prop 
                                            (append val (if (listp prev) prev (list prev))) 
                                            :outer-form-start outer-form-start)
                                      object))
                ;; Anything else means append
                (t (add-text-properties start next
                                        (if (not their-outer-start) 
                                            (list prop 
                                                  (append (if (listp prev) prev (list prev)) val)
                                                  :outer-form-start outer-form-start)
                                            (list prop (append (if (listp prev) prev (list prev)) val)))
                                        object))))
        (setq start (min end
                         (save-excursion
                           (goto-char next)
                           ;; (log-sexp (point) col)
                           (if (not (eolp)) (point)
                             (forward-char) 
                             (cond ((not do-col)
                                    (skip-syntax-forward " ")
                                    (point))
                                   (col (min (+ (point) col) 
                                             (progn 
                                               (skip-syntax-forward " ")
                                               (point))))
                                   (t 
                                    (unless (looking-at "^$") 
                                      (skip-syntax-forward " ") 
                                      (setq col (current-column))) 
                                    (point))))))))))
  nil)


(mm/font-lock-update-keyword
 'lisp-mode
 `(mm/match-conds
   (1 font-lock-keyword-face)
   (2 (mm/apply-highlight 2 'mm/condition 'rect 'prepend-if-nested) nil t)
   (3 (mm/apply-highlight 3 'mm/otherwise 'rect 'prepend-if-nested) nil t)
   (4 (mm/apply-highlight 4 'mm/condition 'rect 'prepend-if-nested) nil t)
   (5 (mm/apply-highlight 5 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (6 (mm/apply-highlight 6 'mm/condition 'rect 'prepend-if-nested) nil t)
   (7 (mm/apply-highlight 7 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (8 (mm/apply-highlight 8 'mm/condition 'rect 'prepend-if-nested) nil t)
   (9 (mm/apply-highlight 9 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (10 (mm/apply-highlight 10 'mm/condition 'rect 'prepend-if-nested) nil t)
   (11 (mm/apply-highlight 11 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (12 (mm/apply-highlight 12 'mm/condition 'rect 'prepend-if-nested) nil t)
   (13 (mm/apply-highlight 13 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (14 (mm/apply-highlight 14 'mm/condition 'rect 'prepend-if-nested) nil t)
   (15 (mm/apply-highlight 15 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (16 (mm/apply-highlight 16 'mm/condition 'rect 'prepend-if-nested) nil t)
   (17 (mm/apply-highlight 17 'mm/consequent 'rect 'prepend-if-nested) nil t))
 'add)

(mm/font-lock-update-keyword
 'lisp-mode
 `(mm/match-cases
   (1 font-lock-keyword-face)
   (2 'mm/bold)
   (3 (mm/apply-highlight 3 'mm/condition 'rect 'prepend-if-nested) nil t)
   (4 (mm/apply-highlight 4 'mm/otherwise 'rect 'prepend-if-nested) nil t)
   (5 (mm/apply-highlight 5 'mm/condition 'rect 'prepend-if-nested) nil t)
   (6 (mm/apply-highlight 6 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (7 (mm/apply-highlight 7 'mm/condition 'rect 'prepend-if-nested) nil t)
   (8 (mm/apply-highlight 8 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (9 (mm/apply-highlight 9 'mm/condition 'rect 'prepend-if-nested) nil t)
   (10 (mm/apply-highlight 10 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (11 (mm/apply-highlight 11 'mm/condition 'rect 'prepend-if-nested) nil t)
   (12 (mm/apply-highlight 12 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (13 (mm/apply-highlight 13 'mm/condition 'rect 'prepend-if-nested) nil t)
   (14 (mm/apply-highlight 14 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (15 (mm/apply-highlight 15 'mm/condition 'rect 'prepend-if-nested) nil t)
   (16 (mm/apply-highlight 16 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (17 (mm/apply-highlight 17 'mm/condition 'rect 'prepend-if-nested) nil t)
   (18 (mm/apply-highlight 18 'mm/consequent 'rect 'prepend-if-nested) nil t))
 'add)

(mm/font-lock-update-keyword
 'lisp-mode
 `(mm/match-if
   (1 font-lock-keyword-face)
   (2 (mm/apply-highlight 2 'mm/condition 'rect 'prepend-if-nested) nil t)
   (3 (mm/apply-highlight 3 'mm/consequent 'rect 'prepend-if-nested) nil t)
   (4 (mm/apply-highlight 4 'mm/otherwise 'rect 'prepend-if-nested) nil t))
 'add)

(mm/font-lock-update-keyword
 'lisp-mode
 `(mm/match-when
   (1 font-lock-keyword-face)
   (2 (mm/apply-highlight 2 'mm/condition 'rect 'prepend-if-nested) nil t)
   (3 (mm/apply-highlight 3 'mm/consequent 'rect 'prepend-if-nested) nil t))
 'add)

(mm/font-lock-update-keyword
 'lisp-mode
 `(mm/match-labels
   (1 font-lock-keyword-face)
   (2 font-lock-function-name-face append t)
   (3 font-lock-function-name-face append t)
   (4 font-lock-function-name-face append t)
   (5 font-lock-function-name-face append t)
   (5 font-lock-function-name-face append t)
   (6 font-lock-function-name-face append t)
   (7 font-lock-function-name-face append t)
   (8 font-lock-function-name-face append t)
   (9 font-lock-function-name-face append t))
 'add)

(mm/font-lock-update-keyword
 'lisp-mode
 `(mm/match-bind-with-object
   (1 font-lock-keyword-face)
   (2 'mm/bold append t)
   (3 font-lock-variable-name-face append t)
   (4 font-lock-variable-name-face append t)
   (5 font-lock-variable-name-face append t)
   (5 font-lock-variable-name-face append t)
   (6 font-lock-variable-name-face append t)
   (7 font-lock-variable-name-face append t)
   (8 font-lock-variable-name-face append t)
   (9 font-lock-variable-name-face append t)
   (10 font-lock-variable-name-face append t))
 'add)

(mm/font-lock-update-keyword
 'lisp-mode
 `(mm/match-bind
   (1 font-lock-keyword-face)
   (2 font-lock-variable-name-face prepend t)
   (3 font-lock-variable-name-face prepend t)
   (4 font-lock-variable-name-face prepend t)
   (5 font-lock-variable-name-face prepend t)
   (5 font-lock-variable-name-face prepend t)
   (6 font-lock-variable-name-face prepend t)
   (7 font-lock-variable-name-face prepend t)
   (8 font-lock-variable-name-face prepend t)
   (9 font-lock-variable-name-face prepend t))
 'add)


;; type of definers
;; type definers: class struct series etc... Has name of the type
;; variable/object definers
;; function/macro definers


(defconst mm/extra-keywords
  '("with-class-slots" "bind" "catch-case" "awhen" "acond" 
    "when-let" "if-let" "aif" "iterate" "iter" "and" 
    "or" "for" "until" "while" "with" "generate" 
    "reducing" "maximizing" "minimizing" "maximize" "minimize" 
    "in" "in-vector" "in-sequence" "collect" "repeat" "collecting" 
    "in-string" "finally" "in-hash" "values"
    "save-market-excursion"
    "save-workspace-excursion"
    "save-window-excursion"
    "with-series"
    "with-workspace"
    "with-window"
    "with-typed-slots"
    "return-values"
    "return-values-from"))

(font-lock-add-keywords 
 'lisp-mode 
 `((,(concat
      "(\\<" (regexp-opt mm/extra-keywords t)
      "\\>") 
    1 font-lock-keyword-face nil)))

;; (font-lock-add-keywords 
;;  'lisp-mode
;;  `((,(concat "(\\(def\\sw+\\)" 
;;              "[ \t'\(]*"
;;              "\\(setf[ \t]+\\sw+)\\|\\sw+\\)?")
;;     (1 font-lock-keyword-face nil)
;;     (2 font-lock-function-name-face))))

(font-lock-add-keywords 
 'lisp-mode
 `((,(concat "(\\(def\\("
             ;; Function declarations.
             "\\(test\\|series\\)\\|"
             ;; Variable declarations.
             "\\(varlike\\|situation\\)\\|"
             ;; Structure declarations.
             "\\(structlike\\)\\|"
             ;; any other symbol
             "\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
             "\\)\\)"
             "[ \t'\(]*"
             "\\(setf[ \t]+\\sw+)\\|\\sw+\\)?")
    (1 font-lock-keyword-face)
    (6 (cond ((match-beginning 3)
              font-lock-function-name-face)
             ((match-beginning 4)
              font-lock-variable-name-face)
             (t font-lock-type-face)) nil t))))

;; highlight cl-def/demacs (def type identifier) and (def (keyword ..options) identifier)
;; with parentesis and options ie (def (funciton io) blah ())
(font-lock-add-keywords 
 'lisp-mode
 `((,(concat "(\\(defc?\\)[ \t]+(\\("
             ;; Function declarations.
             "\\(advice\\|generic\\|macro\\|method\\|function\\|print-object\\)\\|"
             ;; Variable declarations.
             "\\(constant\\|parameter\\|special-variable\\)\\|"
             ;; Structure declarations.
             "\\(class\\|struct\\|type\\)\\|"
             ;; any other symbol
             "\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
             "\\)"
             "[^)]*)"
             "[ \t'\(]*"
             "\\(setf[ \t]+\\sw+)\\|\\sw+\\)?")
    (1 font-lock-keyword-face)
    (2 font-lock-keyword-face nil)
    (6 (cond ((match-beginning 3)
              font-lock-function-name-face)
             ((match-beginning 4)
              font-lock-variable-name-face)
             (t font-lock-type-face)) nil))))

;; same as above but without parentesis
(font-lock-add-keywords 
 'lisp-mode
 `((,(concat "(\\(defc?\\)[ \t]+\\("
             ;; Function declarations.
             "\\(advice\\|generic\\|macro\\|method\\|function\\|print-object\\)\\|"
             ;; Variable declarations.
             "\\(constant\\|parameter\\|special-variable\\)\\|"
             ;; Structure declarations.
             "\\(class\\|struct\\|type\\)\\|"
             ;; any other symbol
             "\\_<\\(?:\\sw\\|\\s_\\)+\\_>"
             "\\)"
             "[ \t'\(]*"
             "\\(setf[ \t]+\\sw+)\\|\\sw+\\)?")
    (1 font-lock-keyword-face)
    (2 font-lock-keyword-face nil)
    (6 (cond ((match-beginning 3)
              font-lock-function-name-face)
             ((match-beginning 4)
              font-lock-variable-name-face)
             (t font-lock-type-face)) nil))))

;; highlight ?keywords used in AI programming as function designators
(font-lock-add-keywords 'lisp-mode 
                        '(("\\_<\\?\\(?:\\sw\\|\\s_\\)+\\_>"
                           0 font-lock-function-name-face)))

;; DEmacs indentation
(require-if-available 'indent-def)

;; DEmacs highlight of docstrings
(put 'def lisp-doc-string-elt-property
     (lambda ()
       ;; The function is called with point right after "def".
       (forward-comment (point-max))
       (cond ((looking-at lisp-indent-def-function-regexp)
              (if (and (looking-at "method\\|(method")
                       (save-excursion 
                         (forward-sexp 3)
                         (backward-sexp 1)
                         (looking-at ":\\|\\sw+")))
                  5 4))
             (t -1))))

;;; Lisp mode customizations
(defun mgm-after-lisp-mode ()
  "Automatically turn on paredit and paredit-magic"
  (when (fboundp 'paredit-mode)
    (paredit-mode))
  (when (fboundp 'paredit-magic-mode)
    (paredit-magic-mode))
  ;; (setq viper-syntax-preference 'emacs)
  (set (make-local-variable 'add-log-current-defun-function) 'mm/lisp-current-defun))

(defun redshank--end-of-sexp-column ()
  "Move point to end of current form, neglecting trailing whitespace."
  (forward-sexp)
  (while (forward-comment +1))
  (skip-chars-backward "[:space:]"))

(defun redshank--sexp-column-widths ()
  "Return list of column widths for s-expression at point."
  (down-list)
  (loop do (while (forward-comment 1))
        until (or (looking-at ")") (eobp))
        collect (- (- (point)
                      (progn
                        (redshank--end-of-sexp-column)
                        (point))))
        finally (up-list)))

(defun redshank--max* (&rest args)
  (reduce #'max args :key (lambda (arg) (or arg 0))))

(defun redshank-align-sexp-columns (column-widths)
  "Align expressions in S-expression at point.
COLUMN-WIDTHS is expected to be a list."
  (down-list)
  (loop initially (while (forward-comment +1))
        for width in column-widths
        until (looking-at ")")
        do (let ((beg (point)))
             (redshank--end-of-sexp-column)
             (let ((used (- (point) beg)))
               (just-one-space (if (looking-at "[[:space:]]*)") 0
                                 (1+ (- width used))))))
        finally (up-list)))

(defun redshank--defclass-slot-form-at-point-p ()
  (ignore-errors
    (save-excursion
      (backward-up-list +3)
      (looking-at "(defclass\\S_"))))

(defun redshank-align-forms-as-columns (beg end)
  "Align S-expressions in region as columns.
Example:
  \(define-symbol-macro MEM (mem-of *cpu*))
  \(define-symbol-macro IP (ip-of *cpu*))
  \(define-symbol-macro STACK (stack-of *cpu*))

is formatted as:

  \(define-symbol-macro MEM   (mem-of *cpu*))
  \(define-symbol-macro IP    (ip-of *cpu*))
  \(define-symbol-macro STACK (stack-of *cpu*))"
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (goto-char beg)
    (let* ((columns
            (loop do (while (forward-comment +1))
                  until (or (looking-at ")") (eobp))
                  collect (redshank--sexp-column-widths)))
           (max-column-widths
            (loop for cols = columns then (mapcar #'cdr cols)
                  while (some #'consp cols)
                  collect (apply #'redshank--max* (mapcar #'car cols)))))
      (goto-char beg)
      (loop do (while (forward-comment +1))
            until (or (looking-at ")") (eobp))
            do (redshank-align-sexp-columns max-column-widths)))))

(defun redshank-align-defclass-slots ()
  "Align slots of the Common Lisp DEFCLASS form at point.
Example (| denotes cursor position):
|(defclass identifier ()
   ((name :reader get-name :initarg :name)
    (location :reader get-location :initarg :location)
    (scope :accessor get-scope :initarg :scope)
    (definition :accessor get-definition :initform nil))
   (:default-initargs :scope *current-scope*))

is formatted to:

|(defclass identifier ()
   ((name       :reader   get-name       :initarg  :name)
    (location   :reader   get-location   :initarg  :location)
    (scope      :accessor get-scope      :initarg  :scope)
    (definition :accessor get-definition :initform nil))
   (:default-initargs :scope *current-scope*))"
  (interactive "*")
  (save-excursion
    (when (redshank--looking-at-or-inside "defclass")
      (down-list)
      (forward-sexp +3)                 ; move to slots definitions
      (let ((slots.end (save-excursion (forward-sexp) (point))))
        (redshank-align-forms-as-columns (progn (down-list) (point))
                                     slots.end)))))

(defadvice comment-indent-new-line (around fix-error-with-nil-comment-column activate)
  (if (null comment-column)
      (let ((comment-column 0))
        (setq ad-return-value ad-do-it))
    (setq ad-return-value ad-do-it)))

(defun mm/lisp-current-defun ()
  ;; If we are now precisely at the beginning of a defun,
  ;; make sure beginning-of-defun finds that one
  ;; rather than the previous one.
  (save-excursion
    (let ((location (point))
          (def-p nil)
          word
          word2)
      (or (eobp) (forward-char 1))
      (beginning-of-defun)
      ;; Make sure we are really inside the defun found,
      ;; not after it.
      (when (and (looking-at "\\s(")
                 (progn (end-of-defun)
                        (< location (point)))
                 (progn (forward-sexp -1)
                        (>= location (point))))
        (if (looking-at "\\s(")
            (forward-char 1))
        ;; Skip the defining construct name, typically "defun"
        ;; or "defvar".
        (setq def-p (looking-at "def\\>"))
        (forward-sexp 1)
        ;; The second element is usually a symbol being defined.
        ;; If it is not, use the first symbol in it.
        (save-excursion
          (skip-chars-forward " \t\n'(")
          (setq word (buffer-substring-no-properties
                      (point)
                      (save-excursion
                        (forward-sexp 1)
                        (point)))))
        (if (not def-p) word
          (forward-sexp 2)
          (forward-sexp -1)
          (setq word2 (buffer-substring-no-properties
                       (point)
                       (save-excursion
                         (forward-sexp 1)
                         (point))))
          (cond ((equal word "function") word2)
                ((not (equal word "method"))
                 (format "%s %s" word word2))
                (t
                 (let (done
                       specializers)
                   ;; skip name
                   (forward-sexp 1)
                   ;; down into parameter list
                   (goto-char (scan-lists (point) 1 -1))
                   (ignore-errors
                     (while t
                       ;; beginning of arg
                       (forward-sexp 1)
                       (forward-sexp -1)
                       (when (looking-at "\\s(")
                         (save-excursion
                           ;; down into specializer
                           (goto-char (scan-lists (point) 1 -1))
                           (forward-sexp 2)
                           (forward-sexp -1)
                           (when (looking-at "\\s(")
                             ;; its an EQL specializer, down into it and skip 2 sexps and back one
                             (goto-char (scan-lists (point) 1 -1))
                             (forward-sexp 2)
                             (forward-sexp -1))
                           (if (not (looking-at "\\s("))
                               (push (buffer-substring-no-properties
                                      (point)
                                      (save-excursion
                                        (forward-sexp 1)
                                        (point)))
                                     specializers))))
                       (forward-sexp 1)))
                   (mapconcat #'identity (cons word2 (nreverse specializers)) " ")))))))))


;; Emacs lisp mode
(defun mgm-after-emacs-lisp-mode ()
  (when (fboundp 'paredit-mode)
    (paredit-mode))
  (when (fboundp 'eldoc-mode)
    (eldoc-mode))
  (when (fboundp 'paredit-magic-mode)
    (paredit-magic-mode)))


(define-key emacs-lisp-mode-map "\C-c\C-c" 'eval-defun)
(define-key lisp-interaction-mode-map "\C-c\C-c" 'eval-defun)

(evil-define-key 'normal emacs-lisp-mode-map ",b" 'edebug-set-breakpoint)
(evil-define-key 'normal emacs-lisp-mode-map ",e" 'eval-expression)
(evil-define-key 'normal emacs-lisp-mode-map "ze" 'eval-last-sexp)


(provide 'my-lisp-mode-setup)
