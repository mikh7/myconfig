
(require 'tempo)
(require 'cc-mode)

(defvar tempo-tags-list nil
  "Used to automatically set tempo tags used after change in the
major mode. Each of the list is in the form:

  (triger &REST tags)

TRIGGER can be one of:

  (:mode mode-name)    - matches if major mode is MODE-NAME
                         mode-name can be a list
  (:name REGEX)        - true if buffer file name matches REGEX

TAGS is either the list of tags to be used, or a lambda form
that will be called and should return list of tags to be used

Example usage:

  (defvar c-tempo-tags nil)
  (defvar c++-tempo-tags nil)
  (defvar proj-tempo-tags nil)

  (tempo-define-template ... 'c-tempo-tags)
  (tempo-define-template .... 'c++-tempo-tags)
  (tempo-define-template .... 'proj-tempo-tags)

  (add-tempo-tags '((:mode c-mode) c-tempo-tags))
  (add-tempo-tags '((:mode c++-mode) c-tempo-tags c++-tempo-tags))
  (add-tempo-tags '((:name \"/project/\" my-proj-tags)))
  (add-tempo-tags '(some-mode
                     (lambda ()
                      (if whatever (tempo-use-tag-list 'tags1)
                                   (tempo-use-tag-list 'tags2)))))
")

(defadvice tempo-insert-template (after tempo-go-insert-mode activate)
  (when (evil-normal-state-p)
    (evil-insert-state 1)))

(defadvice tempo-forward-mark (after tempo-go-insert-mode activate)
  (when (evil-normal-state-p)
    (evil-insert-state 1)))

(defadvice tempo-backward-mark (after tempo-go-insert-mode activate)
  (when (evil-normal-state-p)
    (evil-insert-state 1)))

(define-key evil-motion-state-map " " 'tempo-forward-mark)


;; (global-set-key "\M-n" 'tempo-forward-mark)
;; (global-set-key "\M-p" 'tempo-backward-mark)
(global-set-key [C-return] 'tempo-forward-mark)

(defvar c-tempo-tags nil
  "Tempo tags for C mode")

(defvar c++-tempo-tags nil
  "Tempo tags for C++ mode")

(defvar zsh-tempo-tags nil
  "Tempo tags for zsh-script mode")

(defvar sh-tempo-tags nil
  "Tempo tags for sh-script mode")

(defvar emacs-lisp-tempo-tags nil
  "Tempo tags for emacs lisp mode")

(defvar lisp-tempo-tags nil
  "Tempo tags for lisp mode")

(defvar cl-tempo-tags nil
  "Tempo tags for common lisp mode")

(defun my-tempo-use-tags (list)
  (or (listp list) (setq list (list list)))
  (dolist (elem list)
    (if (functionp elem)
        (funcall elem)
      (funcall 'tempo-use-tag-list elem))))

(defun my-tempo-set-tags-for-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (setq tempo-local-tags nil)
    (tempo-invalidate-collection)
    (let ((name (buffer-file-name (current-buffer))))
      (when name
        (setq name (file-truename name))
        (dolist (elem tempo-tags-list)
          (let ((trigger (car elem))
                (tags (cdr elem)))
            (when
                (or (and (symbolp trigger)
                         (eq major-mode trigger))
                    (and (consp trigger)
                         (eq (car trigger) :mode)
                         (member major-mode (cdr trigger)))
                    (and (consp trigger)
                         (eq (car trigger) :name)
                         (string-match (cadr trigger) name))
                    (and (stringp trigger)
                         (string-match trigger name)))
              (my-tempo-use-tags tags))))))))

(defun tempo-reset-tags-all-buffers (&optional arg)
  "Reset tempo tags in all buffers according to `tempo-tag-list'
                                    in all buffers"
  (interactive)
  (dolist (buffer (buffer-list))
    (my-tempo-set-tags-for-buffer buffer)))

(defun add-tempo-tags (&rest spec)
  (dolist (elem spec)
    (add-to-list 'tempo-tags-list elem))
  (tempo-reset-tags-all-buffers))

(add-hook 'after-change-major-mode-hook 'my-tempo-set-tags-for-buffer)

(setq c-tempo-tags   nil
      c++-tempo-tags nil
      sh-tempo-tags  nil
      zsh-tempo-tags nil
      tempo-tags-list nil
      lisp-tempo-tags nil
      emacs-lisp-tempo-tags nil
      cl-tempo-tags nil)


(defun my-c-match-finder ()
  "Match C symbol or #proprocessor directive"
  (save-excursion
    (cond ((or (re-search-backward "\\(#[[:word:]]+\\)\\=" nil t)
               (re-search-backward "\\b\\([[:word:]]+\\)\\=" nil t))
           (cons (buffer-substring (match-beginning 1) (match-end 1))
                 (match-beginning 1)))
          ((re-search-backward "\\([ \n\t]\\|^\\)\\([^ \t]+\\)\\=" nil t)
           (cons (buffer-substring (match-beginning 2) (match-end 2))
                 (match-beginning 2))))))

(defun tempo-space ()
  "Expand tempo space when not in comment or string"
  (interactive "*")
  (let* ((comment
          (save-excursion (comment-beginning)))
         (in-string-p (unless (or comment (eq major-mode 'sh-mode))
                        (my-in-string-p)))
         (expand-abbrev nil))
    (if (or (and (not comment)
                 (not in-string-p))
            (and comment
                 (looking-back "/\\*\\*?\\=")))
        (or (tempo-expand-if-complete)
            (progn (insert " ")
                   (setq expand-abbrev t)))
      (insert " ")
      (setq expand-abbrev t))
    (when (and expand-abbrev abbrev-mode)
      (when (expand-abbrev)
        (insert " ")))))

(defun enable-tempo-space ()
  (local-set-key " " 'tempo-space)
  (when (member major-mode '(c-mode c++-mode))
    (setq tempo-match-finder 'my-c-match-finder)))

(defun enable-tempo-forward/back-mark ()
  (local-set-key "\M-n" 'tempo-forward-mark)
  (local-set-key "\M-p" 'tempo-backward-mark))

(dolist (hook
         '(c-mode-common-hook
           sh-mode-hook
           emacs-lisp-mode-hook))
  (add-hook hook 'enable-tempo-space))

(dolist (hook
         '(c-mode-common-hook
           sh-mode-hook))
  (add-hook hook 'enable-tempo-forward/back-mark))

(add-tempo-tags '(c-mode c-tempo-tags)
                '(c++-mode c-tempo-tags c++-tempo-tags))

(add-tempo-tags '(sh-mode
                  (lambda ()
                    (cond ((and (symbolp sh-shell)
                                (eq sh-shell 'zsh))
                           (tempo-use-tag-list 'zsh-tempo-tags))
                          (t (tempo-use-tag-list 'sh-tempo-tags))))))

(add-tempo-tags '(emacs-lisp-mode lisp-tempo-tags emacs-lisp-tempo-tags)
                '(lisp-mode lisp-tempo-tags cl-lisp-tempo-tags))

(defadvice tempo-define-template (after invalidate-collection activate)
  (tempo-reset-tags-all-buffers))

(tempo-define-template "c-main"
		       '(> "int main (int argc, char *argv[])" > n> 
                           "{" > n>
                           > r n>
                           "return EXIT_SUCCESS;" n> ;
                           "}" > n>)
		       "main"
		       "Insert a C main function"
		       'c-tempo-tags)

(tempo-define-template "c-if"
		       '(> "if (" r ") {" 
                           > r > n >
                           "}" >)
		       "if"
		       "Insert a C if statement"
		       'c-tempo-tags)

(tempo-define-template "c-while"
		       '(> "while (" r ") {" 
                           > r > n >
                           "}" >)
		       "while"
		       "Insert a C while statement"
		       'c-tempo-tags)

(tempo-define-template "c-do"
		       '(> "do {"
                           > r > n >
                           "} while (" > r ");" n> r)
		       "do"
		       "Insert a C while statement"
		       'c-tempo-tags)

(tempo-define-template "c-switch"
		       '(> "switch (" r ") {" 
                           > r > n >
                           "}" >)
		       "switch"
		       "Insert a C switch statement"
		       'c-tempo-tags)

(tempo-define-template "c-case-with-break"
		       '("case " > r ":" > n
                           > r > n > "break;" n>)
		       nil
		       "Insert a C switch statement"
		       'c-tempo-tags)

(tempo-define-template "c-case-with-brace"
		       '("case " > r ": {"
                         > r > n
                         > "break;" n > 
                         "}" >)
		       nil
		       "Insert a C switch statement"
		       'c-tempo-tags)

(tempo-define-template "c-for"
		       '(> "for (" r "; " r "; " r ") " 
                           "{" > > r > n >
                           "}" >)
		       "for"
		       "Insert a C for statement"
		       'c-tempo-tags)


(tempo-define-template "c-else"
		       '("else " "{"
                         r > n > "}" >)
		       "else"
		       "Insert a C else statement"
		       'c-tempo-tags)

(tempo-define-template "c-elif"
		       '(> "else if (" r ") " "{"
                           r > n > "} " >)
		       "elif"
		       "Insert a C else if statement"
		       'c-tempo-tags)

(tempo-define-template "c-line-comment"
                       '("/* " > r " */" r)
                       "/*"
                       "Insert C style line comment"
                       'c-tempo-tags)

(tempo-define-template "c-comment"
                       '("/**" > n> "* " > r n> "*/")
                       "/**"
                       "Insert C style block comment"
                       'c-tempo-tags)

(tempo-define-template "c-include1"
		       '("#include \"" r "\"" >)
		       "#i"
		       "Insert a #include \"\" statement"
		       'c-tempo-tags)

(tempo-define-template "c-include2"
		       '("#include <" r ">" >)
		       "#<"
		       "Insert a #include <> statement"
		       'c-tempo-tags)

(tempo-define-template "c-ifdef1"
		       '("#ifdef " (P "ifdef-clause: " clause) > n> p n
                         "#endif /* " (s clause)" */" n>)
		       "#id"
		       "Insert a #ifdef #endif statement"
                       'c-tempo-tags)

(tempo-define-template "c-ifdef2"
		       '("#ifdef " (P "ifdef-clause: " clause) > n> p n
			 "#else /* !(" (s clause) ") */" n> p n
			 "#endif /* " (s clause)" */" n>)
		       "#ie"
		       "Insert a #ifdef #else #endif statement"
		       'c-tempo-tags)

(tempo-define-template "c++-class"
                       '("class " r "{" 
                         > r n
                         (insert "};") > n>)
                       "sc"
                       "Insert C++ class statemnt"
                       'c++-tempo-tags)

(tempo-define-template "c++-class-with-parent"
                       '("class " (P "Class name: " class) " : public "
                         (P "Parent Class: " parent) n>
                         > "{" > n> >
                         "public:" > n>
                         > (s class) "(" r ")" n>
                         > ":" > " " (s parent) "(" r ")" n>
                         "{" > n> r n> "}" > n>
                         > "~" (s class) "()" n>
                         "{" > n> r n> "}" > n>
                         "private:" > n> > r n>
                         > "};" > n>)
                       "ssc"
                       "Insert inline C++ class with parent"
                       'c++-tempo-tags)

(tempo-define-template "zsh-ifc"
                       '(> "if [[ " r " ]] ; then" n>
                           > r n>
                           > "fi" >)
                       "ifc"
                       "Insert zsh conditional if statement"
                       'zsh-tempo-tags)

(tempo-define-template "zsh-elifc"
                       '(> "elifc [[ " > r " ]] ; then" n>
                           > r n>)
                       "elifc"
                       "Insert zsh elif conidional statement"
                       'zsh-tempo-tags)

(tempo-define-template "zsh-if"
                       '(> "if " r " ; then" n>
                           > r n>
                           > "fi" >)
                       "if"
                       "Insert zsh if statement"
                       'zsh-tempo-tags)

(tempo-define-template "zsh-elif"
                       '(> "elifc " > r " ; then" n>
                           > r n>)
                       "elif"
                       "Insert zsh elif statement"
                       'zsh-tempo-tags)

(tempo-define-template "case"
                       '(> "case " r " ; in" n>
                           > "(" r ")" n>
                           "    "r " ;;" r n> 
                           "esac" > n>)
                       "case"
                       "Insert zsh if statement"
                       'zsh-tempo-tags)

(tempo-define-template "for"
                       '(> "for " r " in " r " ; do" n>
                           "    " r > n> 
                           "done" > n>)
                       "for"
                       "Insert zsh for statement"
                       'zsh-tempo-tags)

(defvar wt-tempo-tags nil
  "Tempo tags  for  Wt library programming")

(setq wt-tempo-tags nil)

(add-to-list 'tempo-tags-list
             '("projects/sunflare/.*\\.\\(cpp\\|h\\|C\\)"
               wt-tempo-tags))

(tempo-define-template "const-wt-string"
                       '("const Wt::WString& ")
                       "cws"
                       "Insert const Wt::WString&"
                       'wt-tempo-tags)

(tempo-define-template "emacs-lisp-interactive-defun"
                       '("defun " r " (&optional arg)" n>
                         "\"Function documentation\"" n>
                         "(interactive \"P\")" n>
                         r)
                       "idefun"
                       "Insert interactive defun"
                       'emacs-lisp-tempo-tags)

  

