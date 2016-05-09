


(require 'org-compat)
(require 'org)
(require 'org-agenda)
(require-if-available 'org-depend)
(require-if-available 'org-clock)
(require-if-available 'org-id)
(require 'reveal)
(require-if-available 'org-habit)
(require-if-available 'org-mobile)
(require-if-available 'org-src)
(require-if-available 'org-element)
(require-if-available 'org-wl)


(unless (fboundp 'org-mode-p)
  (defun org-mode-p ()
    (eq major-mode 'org-mode)))

(unless (fboundp 'org-get-at-bol)
  (defun org-get-at-bol (property)
    "Get text property PROPERTY at beginning of line."
    (get-text-property (point-at-bol)  property)))

(unless (fboundp 'org-agenda-next-line)
  (defun org-agenda-next-line ()
    (interactive)
    (call-interactively 'next-line))
  (defun org-agenda-previous-line ()
    (interactive)
    (call-interactively 'previous-line)))

(dolist (mode '(org-mode org-agenda-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (add-to-list 'evil-normal-state-modes mode))

(add-to-list 'auto-mode-alist (cons "\\.org$" 'org-mode))
(add-to-list 'auto-mode-alist (cons "\\.org_archive$" 'org-mode))

(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cj" (make-sparse-keymap))
(define-key global-map "\C-cg" (lambda (&rest args)
                                  (interactive "P")
                                  (interactive)
                                  (org-refile '(4))))
;; make return key autoindent

;; make C-RET enter vi insert state
(add-hook 'org-insert-heading-hook
          (lambda ()
            (if (evil-normal-state-p)
                (evil-insert-state))))

;; set my variables
(setq 
 org-todo-keywords 
 '((type "TODO(d!)" "NEXT(n)" "WAIT(w@/!)" "MAYBE(m)" "NEW" "|" 
         "DONE(t@!)" "CANCELLED(c@!)" "DELEGATED(l@!)")
   (sequence "BAD(b!)" "GOOD(g!)" "CBT" "CBTOK" "CBTFAIL"))
 org-agenda-files                 '("~/my-org/")
 org-refile-targets               '((org-agenda-files . (:level . 1))
                                    ("~/my-org/Trading_system_devel.org" .
                                     (:maxlevel . 4)))
 org-refile-use-outline-path       t
 org-log-done                      'note
 org-tags-match-list-sublevels     t
 org-hide-leading-stars            t
 ;; priorities
 org-lowest-priority               ?F
 org-highest-priority              ?A
 org-default-priority              ?D
 org-priority-start-cycle-with-default  nil
 ;; clocking
 org-agenda-start-with-clockreport-mode nil
 org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5 :fileskip0 t :compact t)
 ;; sorting
 org-agenda-sorting-strategy      '((agenda
                                     time-up priority-down)
                                    (todo 
                                     priority-down)
                                    (tags 
                                     priority-down))
 ;;
 ;; Agenda
 ;;
 org-agenda-skip-additional-timestamps-same-entry t
 ;; even if days  are empty show them
 org-agenda-show-all-dates t
 org-deadline-warning-days 7
 ;; show repeating entries in the future
 org-agenda-repeating-timestamp-show-all t
 ;; start today
 org-agenda-start-on-weekday 0
 org-agenda-skip-deadline-if-done t
 org-agenda-skip-scheduled-if-done t
 org-agenda-show-inherited-tags nil
 org-use-fast-todo-selection t
 org-depend-highest-priority t
 org-depend-auto-trigger-todos "NEXT"
 
 org-agenda-use-grid t
 org-agenda-time-grid '((daily today require-timed)
                        "----------------"
                        (800 1000 1200 1400 1600 1800 2000))
 org-log-state-notes-into-drawer t
 ;; default " %i %-12:c%?-12t%-7e % s"
 org-agenda-prefix-format '((agenda . "%-10:c%?-12t % s")
                            (timeline . "  % s")
                            (todo . "%10:c ")
                            (tags . "%-10:c")
                            (search . "%-10:c"))
 org-columns-default-format "%4TODO %70ITEM(Task) %6Effort(E){:} %CLOCKSUM(Act)"
 org-global-properties '(("Effort_ALL" . "0:05 0:10 0:20 0:30 0:40 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00 10:00 12:00 16:00 20:00 30:00 40:00 50:00 70:00 100:00 150:00 200:00 300:00 400:00        "))
 org-stuck-projects '("proj" ("NEXT" "WAIT") ("hold" "inf" "info" "done") "")
 org-tags-column -79
 org-M-RET-may-split-line '((default . nil))
 org-icalendar-include-todo t
 org-icalendar-categories '(all-tags category)
 org-blank-before-new-entry '((heading . nil)
                              (plain-list-item . nil))
 org-use-effective-time t
 org-habit-following-days 7
 org-habit-show-habits-only-for-today t
 org-habit-show-all-today t
 org-habit-graph-column 45
 org-habit-show-done-always-green t
 org-tag-alist-for-agenda nil
 ;; TAGS 
 org-tag-alist '(("emacs" . ?e)
                 ("org" . ?o)
                 ;; ("sunflare" . ?s)
                 ;; ("comp" . ?s)
                 ("slime" . ?s)
                 ("stump" . ?S)
                 ("bind" . ?b)
                 ("paredit" . ?p)
                 ("log4cl" . ?l)
                 ("lisp" . ?L)
                 ;; ("gdb" . ?g)
                 ("web" . ?w)
                 ("wl" . ?u)
                 ("ats" . ?a)
                 ;; good/bad
                 ("good" . ?G)
                 ("bad" . ?B))
 org-fast-tag-selection-single-key 'expert
 org-agenda-log-mode-items '(closed)
 org-agenda-dim-blocked-tasks 'invisible
 org-agenda-diary-file "~/my-org/Diary.org")


;; Custom agenda command definitions

(defun mm/skip-projects ()
  "Skip trees that are projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (cond
     ((mm/is-project-p)
      next-headline)
     (t
      nil))))

(defvar mm/skip-evening-time 1600)

(defun mm/skip-evening ()
  "Skip entries tagged evng unless current time > 4pm"
  (when (and (save-excursion
               (outline-back-to-heading)
               (re-search-forward ".*evng" (line-end-position) t))
             (< (string-to-number (time-stamp-hhmm))
                mm/skip-evening-time))
    (org-end-of-subtree t)))

(defun mm/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (cond
     ((mm/is-project-p)
      next-headline)
     ((org-is-habit-p)
      next-headline)
     (t
      nil))))

(setq org-src-fontify-natively t)

(defun mm/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ NEXT " subtree-end t)))))
    (if (and (mm/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      subtree-end)))

(defun mm/skip-non-inactive-projects ()
  "Skip trees that are not stuck projects"
  (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
         (has-todo (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward org-not-done-heading-regexp
                                             subtree-end t)))))
    (if (and (mm/is-project-p) (not has-todo))
        nil ; a stuck project, has subtasks but no next task
      subtree-end)))

(defun mm/skip-non-project-trees ()
  "Skip trees that are not projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (mm/is-project-p)
        nil
      subtree-end)))

(defun mm/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (cond
     ((mm/is-project-p)
      subtree-end)
     ((org-is-habit-p)
      subtree-end)
     (t
      nil))))

(defun mm/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (let ((next-headline (save-excursion (outline-next-heading))))
    ;; Consider only tasks with done todo headings as archivable candidates
    (if (member (org-get-todo-state) org-done-keywords)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (daynr (string-to-int (format-time-string "%d" (current-time))))
               (a-month-ago (* 60 60 24 (+ daynr 1)))
               (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
               (this-month (format-time-string "%Y-%m-" (current-time)))
               (subtree-is-current (save-excursion
                                     (forward-line 1)
                                     (and (< (point) subtree-end)
                                          (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
          (if subtree-is-current
              subtree-end ; Has a date in this month or last month, skip it
            nil))  ; available to archive
      (or next-headline (point-max)))))  

(setq org-agenda-custom-commands
      '(("h" "Habits (Undone)" agenda "STYLE=\"habit\""
          ((org-agenda-overriding-header "Habits")
           (org-agenda-skip-scheduled-if-done nil)
           (org-agenda-files '("~/my-org/Habits.org" "~/my-org/Habits.orgg"))
           (org-agenda-sorting-strategy '(alpha-up))
           (org-habit-show-habits t)
           (org-habit-show-habits-only-for-today t)
           (org-habit-show-all-today nil)
           (org-agenda-span 'day)))
        ("H" "Habits (ALL)" agenda "STYLE=\"habit\""
         ((org-agenda-overriding-header "Habits")
          (org-agenda-skip-scheduled-if-done nil)
          (org-agenda-files '("~/my-org/Habits.org" "~/my-org/Habits.orgg"))
          (org-agenda-sorting-strategy '(alpha-up))
          (org-habit-show-habits t)
          (org-habit-show-habits-only-for-today t)
          (org-habit-show-all-today t)
          (org-agenda-span 'day)))
        ("A" "Archive" tags "-refile-keep"
         ((org-agenda-overriding-header "Tasks to Archive")
          (org-agenda-skip-function 'mm/skip-non-archivable-tasks)))
        ("d" "Done projects (no TODO entries)"
         tags "-keep-refile/"
         ((org-agenda-overriding-header "Projects with no TODO")
          (org-agenda-skip-function 'mm/skip-non-inactive-projects)))
        ("/" "Occur with archives"
         search ""
         ((org-agenda-text-search-extra-files '(agenda-archives))))
        ("t" "TODO without maybe"
         todo "TODO|NEXT|WAIT|NEW")
        ("a" "Agenda + NEXT (priority)"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-habit-show-habits t)
                   (org-habit-show-habits-only-for-today t)
                   (org-habit-show-all-today nil)
                   ;; (org-agenda-skip-function 'mm/skip-evening)
                   ))
          (tags-todo "-hold/!NEXT"
                     ((org-agenda-overriding-header "Next Tasks")
                      (org-agenda-tags-todo-honor-ignore-options t)
                      (org-agenda-todo-ignore-scheduled t)
                      (org-agenda-todo-ignore-deadlines t)
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy
                       '(priority-down category-keep)))))
         ((org-agenda-buffer-name "*Agenda*")))
        ("N" "Mobile Agenda"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-habit-show-habits nil))))
         ((org-agenda-buffer-name "*Agenda*")))))


(when (not (fboundp 'invisible-p))
  (defun invisible-p (pos)
    (line-move-invisible-p pos)))

;; (defadvice viper-next-line-carefully (around org-include-folded-lines activate)
;;   (if (not (org-mode-p))
;;       (setq ad-return-value ad-do-it)
;;     ;; for org-mode, withing a single heading jump
;;     ;; over invisible lines
;;     (let ((limit (point-max)))
;;       (save-restriction
;;         (save-excursion
;;           (let ((done nil))
;;             (org-back-to-heading t)
;;             (while (not done)
;;               (or (org-goto-sibling)
;;                   (ignore-errors (org-up-heading-all 1)
;;                                  (org-goto-sibling))
;;                   (goto-char (point-max)))
;;               (setq done (or (eobp) (invisible-p (point))))))
;;           (setq limit (point)))
;;         (condition-case nil
;;             ;; do not use forward-line! need to keep column
;;             (progn
;;               (with-no-warnings 
;;                 (call-interactively 'next-line arg)
;;                 (if (> (point) limit)
;;                     (progn
;;                       (goto-char limit)))
;;                 (forward-line -1)))
;;           (error nil))))))

;; (defadvice vimpulse-line-range (around org-include-folded-lines activate)
;;   (if (not (org-mode-p))
;;       (setq ad-return-value ad-do-it)
;;     ;; for org-mode, withing a single heading jump
;;     ;; over invisible lines
;;     (setq ad-return-value
;;           (let* ((beg (min from to))
;;                  (end (max from to)))
;;             (vimpulse-make-motion-range
;;              (save-excursion
;;                (goto-char beg)
;;                (line-beginning-position))
;;              (save-excursion
;;                (goto-char end)
;;                (line-beginning-position)
;;                (line-move 1 t)
;;                (line-beginning-position))
;;              'line)))))

(defun org-next-visible-line-same-heading ()
 (when (org-mode-p)
    (save-restriction
      (push-mark)
      (or (outline-next-heading)
          (goto-char (point-max)))
      (exchange-point-and-mark)
      (narrow-to-region (point) (mark))
      (pop-mark)
      (next-line) ;; ignores invisible
      (when (eobp)
        (goto-char (point-max)))
      (forward-line -1))))

;; Fix open line to go to the next visible line, skipping invisible ones
;; the limit is the current heading
;; (defadvice viper-open-line (before my-org-mode-open-line-visible activate)
;;   (when (org-mode-p)
;;     (let ((org-show-hierarchy-above t)
;;           (org-show-following-heading t))
;;       (org-show-context))
;;     (org-next-visible-line-same-heading)))

(defadvice org-insert-todo-heading (after go-insert-mode activate)
  (evil-insert-state))

;; get the o after a headline automatically do TAB for indentation
;; (defadvice viper-open-line (after my-org-mode-vi-open-line activate)
;;   (when (org-mode-p)
;;     (org-cycle)
;;     ;; TODO same thing but for pressing on headline, should skip all the 
;;     ;; state entries

;;     ;; TODO below needs fixing so that it detects any amount of whitespace before
;;     ;; not just prev line
;;     (when (save-excursion
;;             (forward-line -1)
;;             (looking-at "^\\([ \t]+\\)\\(- State \"\\|:PROPERTIES:\\|:END:\\|:LOGBOOK\\|SCHEDULED:\\|DEADLINE:\\|CLOSED:\\)"))
;;       (let ((len (length (match-string 1))))
;;         ;; TODO only need if wasnt empty line there before
;;         (push-mark)
;;         (forward-line 0)
;;         (delete-region (point) (mark))
;;         (pop-mark)
;;         (insert (format "%s" (make-string len ?\ )))))))

;; Fix paste in the same way
;; (defadvice viper-put-back (before my-org-mode-put-back-visible  activate)
;;   (when (org-mode-p)
;;     (let ((org-show-hierarchy-above t)
;;           (org-show-entry-below t))
;;       (org-show-context))
;;     (org-next-visible-line-same-heading)))


;; close the current most outer level
(defun my-org-fold-toplevel (&optional arg)
  "Fold the top level tree"
  (interactive)
  (ignore-errors (outline-up-heading 20))
  (hide-subtree))

(defun my-org-fold-all (&optional arg)
  "Fold everything"
  (interactive)
  (ignore-errors (outline-up-heading 20))
  (hide-subtree)
  (save-excursion
    (org-cycle t)))

(defun my-org-skip-forward (arg)
  "Move forward to the next visible 2nd or greater level heading,
skipping headings of the same level as the starting position"
  (interactive "p")
  (let ((initial-level (org-outline-level))
        (done nil))
    (while (not done)
      (outline-next-visible-heading arg)
      (let ((level (org-outline-level)))
        (when (and (> initial-level 1) (= level 1))
           (setq initial-level -1))
        (setq done (or
                    (and (< arg 0) (bobp)) 
                    (and (> arg 0) (eobp))
                    (and (not (= level initial-level)))))))))

(defun my-org-skip-backward (arg)
  (interactive "p")
  (my-org-skip-forward (- arg)))

(defun my-log-new-todo ()
  (let (heading-start 
        todo-keyword
        indent)
    (save-excursion
      (org-back-to-heading t)
      (setq heading-start (point))
      (looking-at org-todo-line-regexp)
      (setq todo-keyword (match-string 2))
      (setq indent (- (match-beginning 2) heading-start))
      (end-of-line)
      (insert (format "\n%s- State %-12s %s" 
                      (make-string indent ?\ )
                      (format "\"%s\"" (match-string 2))
                      (format-time-string
                       (org-time-stamp-format 'long 'inactive)
                       (current-time)))))))

(defun my-org-end-of-parent ()
  "Go to the end of the parent of the current headline, return parent headline level"
  (org-back-to-heading t)
  (org-up-heading-safe)
  (let ((level (org-outline-level)))
    (org-end-of-subtree t t)
    (or (bolp) (insert "\n"))
    (org-back-over-empty-lines)
    (org-reveal nil)
    level))

(defun my-org-insert-todo-heading-end (arg)
  "Insert TODO heading at the end of the current project"
  (interactive "p")
  (let ((parent-level (my-org-end-of-parent)))
    (when parent-level
      (end-of-line 0)
      (org-reveal nil)
      (org-insert-todo-heading-respect-content)
      (evil-insert-state))))

(defun my-org-insert-todo-heading-start (arg)
  "Insert TODO heading at the end of the current project"
  (interactive "p")
  (org-back-to-heading t)
  (org-up-heading-safe)
  (outline-next-heading)
  (beginning-of-line)
  (org-reveal)
  (org-insert-todo-heading-respect-content)
  (evil-insert-state))

(defun my-org-sort (&optional arg)
  "Sort current item if it has any TODO children, otherwise sort
children of parent. With a prefix arg sort current item if it has
any children"
  (interactive "P")
  (let ((goal-column 0) eoh eol eos has-children has-todo-children struct)
    ;; First, determine end of headline (EOH), end of subtree or item
    ;; (EOS), and if item or heading has children (HAS-CHILDREN).
    (save-excursion
      (if (org-at-item-p)
	  (progn
	    (beginning-of-line)
	    (setq struct (org-list-struct))
	    (setq eoh (point-at-eol))
	    (setq eos (org-list-get-item-end-before-blank (point) struct))
	    (setq has-children (org-list-has-child-p (point) struct)))
	(org-back-to-heading)
	(setq eoh (save-excursion (outline-end-of-heading) (point)))
	(setq eos (save-excursion
		    (org-end-of-subtree t)
		    (unless (eobp)
		      (skip-chars-forward " \t\n"))
		    (if (eobp) (point) (1- (point)))))
        ;; loop through sub-headings
        (save-excursion
          (outline-next-heading)
          (while (< (point) eos)
            (when (and (org-at-heading-p t)
                       (looking-at org-todo-line-regexp)
                       (match-end 2))
              (setq has-todo-children (match-string 2)))
            (outline-next-heading)))
	(setq has-children
	      (or (save-excursion
		    (let ((level (funcall outline-level)))
		      (outline-next-heading)
		      (and (org-at-heading-p t)
			   (> (funcall outline-level) level))))
		  (save-excursion
		    (org-list-search-forward (org-item-beginning-re) eos t))))))
    (if (or has-todo-children
            (and arg has-children))
        (org-sort arg)
      (save-excursion
        (if (org-up-heading-safe)
            (my-org-sort arg)
          (error "Nothing to sort"))))))


(define-key org-agenda-mode-map ";" (make-sparse-keymap))
(define-key org-agenda-mode-map "z" (make-sparse-keymap))
(define-key org-agenda-mode-map "Y" (make-sparse-keymap))
(define-key org-agenda-mode-map "y" (make-sparse-keymap))
(define-key org-agenda-mode-map "s" nil)
(define-key org-agenda-mode-map "S" nil)
(define-key org-agenda-mode-map "/" nil)
(define-key org-agenda-mode-map "T" nil)
;; I prefer w to be vi's key, and switch to weekly by using vw
(define-key org-agenda-mode-map "w" nil)
(define-key org-agenda-mode-map "\C-m" 'my-org-agenda-switch-to)

;; (viper-reset-overrides 'org-agenda-mode 'vi-state)
(evil-give-back-keys-in-mode '(org-agenda-mode) nil)

(defun mm/org-agenda-refile (&optional arg)
  "Like `org-agenda-refile' but does not refresh agenda"
  (interactive "P")
  (org-agenda-refile arg nil t))

;; give back z as zz
(evil-define-key 'normal org-agenda-mode-map "zz" 'org-agenda-add-note)
(evil-define-key 'normal org-agenda-mode-map ";r" 'mm/org-agenda-refile)
(evil-define-key 'normal org-agenda-mode-map "Yt" 'org-agenda-todo-yesterday)
(evil-define-key 'normal org-agenda-mode-map "yt" 'org-agenda-todo-yesterday)
(evil-define-key 'normal org-agenda-mode-map ";;" 'org-agenda-filter-by-tag)
(evil-define-key 'normal org-agenda-mode-map ";/" 'org-agenda-filter-by-tag)

(evil-define-key 'normal org-agenda-mode-map "st" nil)
(evil-define-key 'normal org-agenda-mode-map "sT" 'org-agenda-show-tags)
(evil-define-key 'normal org-agenda-mode-map "k" 'org-agenda-previous-line)
(evil-define-key 'normal org-agenda-mode-map "j" 'org-agenda-next-line)


(define-key org-mode-map [C-next] 'my-org-skip-forward)
(define-key org-mode-map [C-prior] 'my-org-skip-backward)

(define-key org-mode-map "\M-h" 'org-metaleft)
(define-key org-mode-map "\M-l" 'org-metaright)
(define-key org-mode-map "\M-k" 'org-metaup)
(define-key org-mode-map "\M-j" 'org-metadown)
(define-key org-mode-map "\M-c" 'org-metaup)
(define-key org-mode-map "\M-t" 'org-metadown)
;; (evil-define-key 'normal org-mode-map ";c" 'my-org-fold-toplevel)
(evil-define-key 'normal org-mode-map ";;" 'my-org-fold-all)
(evil-define-key 'normal org-mode-map ";r" 'org-refile)
(evil-define-key 'normal org-mode-map ";n" 'my-org-insert-todo-heading-end)
(evil-define-key 'normal org-mode-map ";N" 'my-org-insert-todo-heading-start)
(evil-define-key 'normal org-mode-map ";w" 'org-cut-special)
(evil-define-key 'normal org-mode-map ";y" 'org-paste-special)
(evil-define-key 'normal org-mode-map ";t" 'mm/org-set-tags)
(evil-define-key 'normal org-mode-map ";l" 'org-shiftright)
(evil-define-key 'normal org-mode-map ";h" 'org-shiftleft)
(evil-define-key 'normal org-mode-map "zt" 'org-todo)
(evil-define-key 'normal org-mode-map ";s" 'my-org-sort)
(evil-define-key 'normal org-mode-map "za" 'org-archive-subtree-default)
(evil-define-key 'normal org-mode-map ";e" 'org-gfm-export-to-markdown)
(evil-define-key 'normal org-mode-map "; " 'mm/org-cc-space)
(evil-define-key 'normal org-mode-map ";c" 'mm/org-cc-space)
(evil-define-key 'normal org-mode-map ";=" 'mm/org-cc-=)
(evil-define-key 'normal org-mode-map "zl" 'org-open-at-point)
(evil-define-key 'normal org-mode-map "\C-p" 'outline-previous-visible-heading)
(evil-define-key 'normal org-mode-map "\C-n" 'outline-next-visible-heading)

(define-key org-mode-map (kbd "RET") 'org-return-indent)

(defun mm/org-set-tags (&optional arg just-align)
  "Go back to heading the do org-set-tags"
  (interactive "P")
  (save-excursion 
    (org-back-to-heading) 
    (org-set-tags arg just-align)))

(defun mm/org-cc-space (&optional arg)
  (interactive "P")
  (if (org-at-table-p) (org-table-blank-field)
    (mm/org-insert-checkbox)))

(defun mm/org-cc-= (&optional arg)
  (interactive "P")
  (if (org-at-table-p) (org-table-eval-formula arg)
    (ding)))

(defun mm/org-at-crap-p ()
  (or (looking-at "^\\([ \t]*\\)\\(:[A-Z]+:\\)")
      (looking-at "^\\([ \t]*\\)\\(SCHEDULED:\\|DEADLINE:\\|CLOSED:\\)")))

(defun mm/at-crap-p ())
(defun mm/org-skip-crap (&optional arg)
  "Skips crap after headline, with arg"
  (interactive)
  (let ((org-show-hierarchy-above t)
        (org-show-following-heading t)
        (org-show-entry-below t)
        done)
    (org-show-context)
    (when (org-at-heading-p)
      (forward-line))
    (while (not done)
      (cond ((looking-at "^\\([ \t]*\\)\\(:[A-Z]+:\\)")
             (if (re-search-forward "^[ \t]*:END:[ \t]*" nil t)
                 (forward-line)
               (error "Missing :END:")))
            ((save-excursion
               (beginning-of-line)
               (looking-at "^\\([ \t]*\\)\\(SCHEDULED:\\|DEADLINE:\\|CLOSED:\\)"))
             (forward-line))
            (t (setq done t))))
    (cond ((org-at-heading-p)
           (progn (beginning-of-line)
                  (insert "\n")
                  (backward-char)
                  (call-interactively (global-key-binding "\t"))))
          ((save-excursion
             (beginning-of-line)
             (looking-at "^[ \t]*$")
             (call-interactively (global-key-binding "\t"))))
          (t (progn (beginning-of-line)
                    (insert "\n")
                    (backward-char)
                    (call-interactively (global-key-binding "\t")))))))

(defun mm/org-insert-checkbox (&optional arg)
  "Insert plain list item checkbox either on current line or next one"
  (interactive "P")
  (let ((itemp (org-in-item-p)))
    (cond (itemp
           (org-insert-item 'checkbox))
          ((or (org-at-heading-p))
           (mm/org-skip-crap)
           (insert "- [ ] "))
          ((save-excursion
             (beginning-of-line)
             (looking-at "^[ \t]*$"))
           (call-interactively (global-key-binding "\t"))
           (insert "- [ ] "))
          ((mm/org-at-crap-p)
           (mm/org-skip-crap)
           (insert "- [ ] "))
          (t (end-of-line)
             (insert "\n")
             (call-interactively (global-key-binding "\t"))
             (insert "- [ ] ")))
    (when (or (not itemp)
              (save-excursion (goto-char itemp)
                              (looking-at "[ \t]*-"))) 
      (mm/insert-parent-headline-cookie 0))
    (unless (evil-insert-state-p)
      (evil-insert-state 1))))

(defun mm/org-m-ret-continue-checkboxes ()
  "If current item has a checkbox, make new item a checkbox"
  (interactive)
  (let ((itemp (org-in-item-p)))
    (when (and itemp
               (save-excursion
                 (goto-char itemp)
                 (org-at-item-checkbox-p)))
      (mm/org-insert-checkbox)
      t)))

(add-to-list 'org-metareturn-hook 'mm/org-m-ret-continue-checkboxes)

(evil-define-key 'normal org-agenda-mode-map ";e" 'org-agenda-set-effort)
;; seems better suited for z map
(evil-define-key 'normal org-mode-map "ze" 'org-set-effort)
(evil-define-key 'normal org-agenda-mode-map "ze" 'org-agenda-set-effort)
;; (vimpulse-define-key 'org-mode nil "\C-h\C-h" 'org-insert-heading-after-current)
;; (vimpulse-define-key 'org-mode nil "\C-hh" 'org-insert-heading-after-current)
(evil-define-key 'normal org-mode-map "g'" 'org-edit-special)
(evil-define-key 'normal org-src-mode-map "g'" 'org-edit-src-exit)

(evil-define-key 'normal org-agenda-mode-map ";w" 'org-agenda-week-view)
(evil-define-key 'normal org-mode-map "Th" 'org-remove-occur-highlights)
(evil-define-key 'normal org-mode-map "zn" 'org-add-note)
(evil-define-key 'normal org-agenda-mode-map "zn" 'org-agenda-add-note)

(define-key org-src-mode-map "\C-c" nil)

(defun mm/org-insert-heading-after-current ()
  "Insert a new heading with same level as current, after current subtree."
  (interactive)
  (org-back-to-heading)
  (org-insert-heading)
  (org-move-subtree-down)
  (end-of-line 1))

(defun my-insert-subheading-after-current ()
  (interactive)
  (org-insert-heading-after-current)
  (org-demote-subtree))

;; (evil-define-key 'normal org-mode-map "\C-h\C-s"
;;                     'my-insert-subheading-after-current)
;; (evil-define-key 'normal org-mode-map "\C-hs"
;;                     'my-insert-subheading-after-current)


(defun mm/org-shift-return (&optional arg)
  (interactive "P")
  (cond ((org-at-table-p)
         (org-table-copy-down arg))
        (t (org-insert-subheading arg))))

(evil-define-key 'normal org-mode-map (kbd "<S-return>") 'mm/org-shift-return)
(evil-define-key 'insert org-mode-map  (kbd "<S-return>") 'mm/org-shift-return)

(evil-define-key 'normal org-mode-map "\C-m" 'org-cycle)
(evil-define-key 'insert org-mode-map "\C-m" 'my-exec-key-in-emacs)


(defun mm/org-insert-mode-tab (&optional arg)
  (interactive "P")
  (cond ((org-at-table-p)
         (org-cycle arg))
        (t (org-cycle arg))))

(evil-define-key 'normal org-mode-map (kbd "<tab>")
                    'org-cycle)

;;; custom agenda keys

(defun my-org-agenda-switch-to (&optional arg)
  (interactive "P")
  (let* ((marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker)))
    (cond ((and 
            (not (eq (selected-window) (next-window)))
            (eq buffer (window-buffer (next-window))))
           (org-agenda-switch-to t))
          (t (org-agenda-switch-to arg)))))

(defun mm/org-agenda-satisfaction-edit ()
  (interactive)
  (or (eq major-mode 'org-agenda-mode) (error "Should be in org-agenda-mode"))
  (org-set-local 'org-agenda-overriding-columns-format
                 "%4TODO %50ITEM(Task) %3SatisfactionEstimate(E) %3SatisfactionActual(A)")
  (org-agenda-columns))

(setq org-agenda-keymap (copy-keymap org-agenda-mode-map))

;; fix copying using blank lines
;; (defun org-copy-subtree (&optional n cut)
;;   "Cut the current subtree into the clipboard.
;; With prefix arg N, cut this many sequential subtrees.
;; This is a short-hand for marking the subtree and then copying it.
;; If CUT is non-nil, actually cut the subtree."
;;   (interactive "p")
;;   (let (beg end folded (beg0 (point)))
;;     (if (interactive-p)
;; 	(org-back-to-heading nil) ; take what looks like a subtree
;;       (org-back-to-heading t)) ; take what is really there
;;     ;;(org-back-over-empty-lines)
;;     (setq beg (point))
;;     (skip-chars-forward " \t\r\n")
;;     (save-match-data
;;       (save-excursion (outline-end-of-heading)
;; 		      (setq folded (org-invisible-p)))
;;       (condition-case nil
;; 	  (outline-forward-same-level (1- n))
;; 	(error nil))
;;       (org-end-of-subtree t t))
;;     (setq end (point))
;;     (goto-char beg0)
;;     (when (> end beg)
;;       (setq org-subtree-clip-folded folded)
;;       (if cut (kill-region beg end)
;;         (copy-region-as-kill beg end))
;;       (setq org-subtree-clip (current-kill 0))
;;       (message "%s: Subtree(s) with %d characters"
;; 	       (if cut "Cut" "Copied")
;; 	       (length org-subtree-clip)))))

;; highlight current line in agenda
(add-hook 'org-agenda-mode-hook 
          '(lambda () (hl-line-mode 1)))

;; remember setup

(require-if-available 'org-capture)


(defun my-org-maybe-go-insert ()
  (when (and evil-mode (org-mode-p)
             (or (string-match "^\\*Org Note" (buffer-name)) 
                 (string-match "^\\*Capture" (buffer-name))
                 (string-match "^CAPTURE" (buffer-name))))
    (evil-insert-state)))

(defun mgm-after-org-mode ()
  (my-org-maybe-go-insert))


(defun mm/org-capture-mode-hook ()
  (when (equal (org-capture-get :key 'local) "c") 
    (org-add-log-setup 'state "TODO" "NEW" 'findpos 'state) 
    (save-excursion (org-add-log-note)) 
    (org-cycle-hide-drawers 'children)))

(add-hook 'org-capture-before-finalize-hook 'mm/org-capture-mode-hook)

(setq org-directory "~/my-org") 
(setq org-default-notes-file (concat org-directory "/Diary.org")) 
(setq org-capture-templates
      '(
        ("c" "New TODO" entry (file+datetree "Diary.org")
         "* TODO %?")
        ("d" "Diary Entry" entry (file+datetree "Diary.org")
         "* %U %?" :unnarrowed t)
        ("m" "Music" entry (file+headline "Music.org" "Music")
         "** %?")
        ;; ("d" "Diary" entry (file+headline "Diary.org" "Diary")
        ;;  "* %?\n%U\n%a\n  %i" :clock-in nil :clock-keep nil :clock-resume nil)
        ("D" "Clocked Diary" entry (file+headline "Diary.org" "Diary")
         "* %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
        ("i" "Info" entry (file+headline "Assorted_Info.org" "Assorted Info")
         "* %?\n%U\n%a\n  %i" :clock-in nil :clock-resume nil)
        ("a" "Account" entry (file+headline "Assorted_Accounts.org" "Assorted Accounts")
         "* %?\n%U\n%a\n  %i" :clock-in nil :clock-resume nil)))

(when (fboundp 'org-capture-refile) 
  (define-key global-map "\C-cc" 'org-capture) 
  (define-key org-capture-mode-map "\C-cr" 'org-capture-refile) 
  (define-key global-map "\C-cd" 'mm/jump-to-diary))

(defun mm/jump-to-diary ()
  (interactive)
  (org-switch-to-buffer-other-window
   (find-file-noselect org-agenda-diary-file))
  (require 'org-datetree)
  (let ((post-date (format-time-string (org-time-stamp-format)
                                       (seconds-to-time (float-time (org-current-time))))))
    (setq post-date (nthcdr 3 (parse-time-string post-date)))
    (setq post-date (list (cadr post-date) 
                          (car post-date) 
                          (caddr post-date))) 
    (org-datetree-find-date-create post-date)
    (org-reveal t)))

(defadvice org-capture-place-template (after go-viper-insert-mode activate)
  (evil-insert-state))

(defun capture-divaradio (artist title album year)
  "Called by capture-divaradio shell script"
  (let ((heading
         (if (plusp year) (format "%s - %s | %s | %s" artist title album year)
           (format "%s - %s | %s" artist title album)))) 
    (log-sexp artist title album year)
    (org-capture nil "m")
    (insert heading)
    (org-capture-finalize nil)
    (shell-command (format "DISPLAY=:0.0 notify-send %s %s"
                           (shell-quote-argument "Diva Radio")
                           (shell-quote-argument heading)))))

;; My own idle time detection
(setq org-clock-idle-time nil)

(defvar my-idle-timer nil
  "Idle timer to for auto clock in/out")

(defvar my-idle-time-seconds nil
  "Idle time for auto-clock out")

(defvar my-idle-clock-out nil
  "Set to non-nil if org mode should auto-clock out")

(defvar my-idle-clock-back nil
  "Set to non-nil if org mode should auto-clock back in")

(defun my-clock-clock-in (clock &optional resume start-time)
  "Clock in to the clock located by CLOCK.
If necessary, clock-out of the currently active clock."
  (org-with-clock-position clock
    (let ((org-clock-in-resume (or resume org-clock-in-resume)))
      (org-clock-in nil start-time))))

(defvar my-idle-tried-clocking-in nil)
(defvar my-idle-clocked-out nil)

(defun mm/org-clock-on-default-task-p ()
  (and (org-clocking-p)
       org-clock-default-task
       (marker-buffer org-clock-default-task)
       (= (marker-position org-clock-default-task)
          (with-current-buffer (org-clocking-buffer)
            (save-excursion
              (goto-char org-clock-marker)
              (org-back-to-heading)
              (point))))))

;;
;; Fix this function to ignore default task
;; 
(defadvice org-agenda-mark-clocking-task (around mm/ignore-default-task activate)
  "Do not highlight default task"
  (unless (equal org-clock-default-task org-clock-hd-marker)
    (setq ad-return-value ad-do-it)))

(defun mm/auto-clock-in ()
  nil
  ;; (when (and (not my-idle-tried-clocking-in)
  ;;            my-idle-clock-out
  ;;            my-idle-clock-back
  ;;            my-idle-clocked-out)
  ;;   (setq my-idle-tried-clocking-in t)
  ;;   (setq my-idle-clocked-out nil)
  ;;   (cond ((and (car org-clock-history)
  ;;               (marker-buffer (car org-clock-history))
  ;;               (with-current-buffer (marker-buffer (car org-clock-history))
  ;;                 (save-excursion
  ;;                   (ignore-errors
  ;;                     (goto-char (marker-position (car org-clock-history)))
  ;;                     (not (org-entry-is-done-p))))))
  ;;          (with-current-buffer (marker-buffer (car org-clock-history))
  ;;            (save-excursion
  ;;              (ignore-errors
  ;;                (goto-char (marker-position (car org-clock-history)))
  ;;                (org-clock-in)))))
  ;;         ((and org-clock-default-task (marker-buffer org-clock-default-task))
  ;;          (with-current-buffer (marker-buffer org-clock-default-task)
  ;;            (save-excursion
  ;;              (ignore-errors
  ;;                (goto-char (marker-position org-clock-default-task))
  ;;                (org-clock-in)))))
  ;;         (t (ignore-errors
  ;;              (mm/clock-in-organization-task-as-default))))
  ;;   ;; only do this once
  ;;   (when (org-clocking-p)
  ;;     (setq my-idle-tried-clocking-in nil)))
  )

(defvar my-doing-auto-clock-in nil)

(defun my-idle-post-command-hook ()
  nil
  ;; (condition-case e
  ;;     (progn
  ;;       (setq mm/did-auto-mobile-push nil)
  ;;       (if (org-clocking-p)
  ;;           (setq my-idle-tried-clocking-in nil)
  ;;         (unless my-doing-auto-clock-in
  ;;           (let ((my-doing-auto-clock-in t))
  ;;             (mm/auto-clock-in)))))
  ;;   (error (message "Error doing my-idle-post-command-hook %s" e)))
  )

(defun my-idle-clock-out ()
  (when (and (org-clocking-p)
             (not org-clock-resolving-clocks)
             (not (string-match "Diary"
                                (buffer-name (marker-buffer (first org-clock-history))))))
    (org-clock-out t)
    (setq my-idle-clocked-out t)
    (message "Idle auto clock-out")))

(defvar my-auto-mobile-push-seconds 320
  "Auto push to org-mobile time, set to nil to disable")

(defvar mm/did-auto-mobile-push nil
  "True when did mobile push on idle")

(defun my-idle-timer ()
  (condition-case e
      (let* ((emacs-idle (current-idle-time))
           (idle (if (null emacs-idle) 0
                   (org-float-time emacs-idle))))
      ;; (message "my-idle-timer idle=%s" idle)
      (cond ((and my-idle-clock-out
                  my-idle-time-seconds
                  (> idle my-idle-time-seconds))
             (let ((org-clock-resolving-clocks-due-to-idlenesst))
               (my-idle-clock-out))))
      (cond ((and my-auto-mobile-push-seconds
                  (> idle my-auto-mobile-push-seconds)
                  (not mm/did-auto-mobile-push))
             (setq mm/did-auto-mobile-push t)
             (ignore-errors
               (org-mobile-push)))))
    (error (message "Error doing my-idle-timer %s" e))))

(defun my-start-idle-timer ()
  (when my-idle-timer
    (cancel-timer my-idle-timer)
    (setq my-idle-timer nil))
  (setq my-idle-timer
        (run-with-timer 5 5 'my-idle-timer)))

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
;; Small windows on my Eee PC displays only the end of long lists which isn't very useful
(setq org-clock-history-length 10)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Do not change task states when clocking in
(setq org-clock-in-switch-to-state nil)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done nil) ; we do it on our own
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; automatic clock
(setq my-idle-time-seconds 600
      my-idle-clock-out t
      my-idle-clock-back nil
      my-auto-mobile-push-seconds nil)

(setq mm/keep-clock-running nil)

(defvar mm/organization-id "8069b654-0209-4003-83ad-41d9b9058b6b")
(defvar mm/playtime-id "8eb73ad1-fe70-417b-8631-b0937c862624")
(defvar mm/idletime-id "1de9d8f8-0e77-4e61-be34-79d24ac03442")


(defun mm/is-project-p ()
  "Return true if heading is a project. Heading is considered a
  project if it has :proj: tag or is a top level heading in a
  file that has :proj: file tag. Point must be at the beginning
  ot the heading"
  (or (and (member "proj" org-file-tags)
           (= (org-outline-level) 1))
      (member "proj" (org-get-tags-at nil t))))

(defun mm/find-project-task ()
  "Move point to the parent (project) task if any"
  (let ((parent-task (save-excursion (org-back-to-heading) (point)))
        (done nil)
        (proj-file-p (member "proj" org-file-tags)))
    (while (not done)
      (if (or (member "proj" (org-get-tags-at nil t))
              (and proj-file-p
                   (= (org-outline-level) 1)))
          (progn
            (setq parent-task (point))
            (setq done t)))
      (unless (org-up-heading-safe)
        (setq done t)))
    (goto-char parent-task)
    parent-task))

(defun mm/clock-in-and-set-project-as-default (pom)
  "Clock in the current task and set the parent project (if any) as the
default clocking task.  Agenda filter tags are set from the default task"
  ;; Find the parent project task if any and set that as the default
  (save-excursion
    (save-excursion
      (org-with-point-at pom
        (mm/find-project-task)
        (org-clock-in '(16))))
    (save-excursion
      (org-with-point-at pom
        (org-clock-in nil)))))

(defun mm/set-agenda-restriction-lock ()
  "Set filter to tags of POM, current task, or current project and refresh"
  (interactive)
  ;;
  ;; We're in the agenda
  ;;
  (if (or (org-mode-p)
          (eq major-mode 'org-agenda-mode))
      (let* ((pom (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at pom (org-get-tags-at))))
        (if (equal major-mode 'org-agenda-mode)
            (if tags
                (org-with-point-at pom
                  (mm/find-project-task)
                  (org-agenda-set-restriction-lock))
              (org-agenda-remove-restriction-lock))
          (org-with-point-at pom
            (mm/find-project-task)
            (org-agenda-set-restriction-lock))))
    (error "Must be in org-mode or org-agenda mode")))

(defun mm/remove-restriction-lock ()
  "Set filter to tags of POM, current task, or current project and refresh"
  (interactive)
  ;;
  ;; We're in the agenda
  ;;
  (org-agenda-remove-restriction-lock))

(defun mm/punch-in ()
  "Start continuous clocking and set the default task to the project task
of the selected task.  If no task is selected set the Organization task as
the default task."
  (interactive)
  (setq mm/keep-clock-running t)
  (let ((organization (org-id-find mm/organization-id 'marker)))
    (if (equal major-mode 'org-agenda-mode)
        ;;
        ;; We're in the agenda
        ;;
        (let* ((marker (org-get-at-bol 'org-hd-marker))
               (tags (org-with-point-at marker (org-get-tags-at))))
          (if tags
              (mm/clock-in-and-set-project-as-default marker)
            (mm/clock-in-organization-task-as-default)))
      ;;
      ;; We are not in the agenda
      ;;
      (save-restriction
        (widen)
        (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)))
            (mm/clock-in-and-set-project-as-default nil)
          (mm/clock-in-organization-task-as-default))))
    (when (and (org-clocking-p)
               (not (equal organization org-clock-hd-marker)))
      (mm/set-agenda-restriction-lock))))

(defun mm/punch-out ()
  (interactive)
  (setq mm/keep-clock-running nil)
  (move-marker org-clock-default-task nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun mm/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun mm/clock-in-organization-task-as-default ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find mm/organization-id 'marker)
      (mm/punch-in))))

(defun mm/start-playtime ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find mm/playtime-id 'marker)
      (let ((headline (format "Playtime %s"
                              (format-time-string "%Y-%m-%d" (current-time))))
            (subtree-end (save-excursion (org-end-of-subtree t))))
        (unless (re-search-forward (format "^\\*\\*\\* %s" headline) subtree-end t)
          (my-insert-subheading-after-current)
          (insert headline)
          (insert "\n"))
        (org-back-to-heading t)
        (mm/clock-in)))))

(defun mm/start-idletime ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find mm/playtime-id 'marker)
      (mm/clock-in))))

(defun mm/clock-out-maybe ()
  (when (and mm/keep-clock-running (not org-clock-clocking-in))
    (cond ((and (second org-clock-history)
                (or
                 (equal (file-name-nondirectory (buffer-file-name))
                        "Diary.org")
                 (org-with-point-at (first org-clock-history) 
                   (org-back-to-heading t)
                   (looking-at "^\*+ Playtime"))))
           (run-with-timer
            0 nil (lambda (&optional arg)
                    (org-with-point-at (second org-clock-history)
                      (mm/clock-in)))))
          ((and (marker-buffer org-clock-default-task)
                (not org-clock-resolving-clocks-due-to-idleness))
           (mm/clock-in-default-task)))))

(defun mm/clock-out (&optional arg)
  "Clock out from currently clocked task, if it was Organization
  task, also punch out"
  (interactive)
  (cond ((mm/org-clock-on-default-task-p)
         (mm/punch-out))
        (t (org-clock-out))))

(define-key evil-normal-state-map ";g" (make-sparse-keymap))
(define-key evil-normal-state-map ";gg" 'mm/org-clock-goto)
(define-key evil-normal-state-map ";gG" (lambda (&optional arg)
                                      (interactive)
                                      (org-clock-goto '(4))))
(define-key evil-normal-state-map ";gO" 'mm/clock-in-organization-task-as-default)
(define-key evil-normal-state-map ";go" 'mm/clock-out)
(define-key evil-normal-state-map ";gi" 'mm/clock-in)
(define-key evil-normal-state-map ";gI" 'mm/punch-in)
(define-key evil-normal-state-map ";gr" 'mm/set-agenda-restriction-lock)
(define-key evil-normal-state-map ";gR" 'mm/remove-restriction-lock)
(define-key evil-normal-state-map ";gdi" 'mm/toggle-auto-clock-out)
(define-key evil-normal-state-map ";gm" 'org-mark-ring-goto)

(defun mm/clock-in ()
  (interactive)
  (cond ((org-mode-p)
         (org-clock-in))
        ((eq major-mode 'org-agenda-mode)
         (org-agenda-clock-in))
        (t (org-clock-in '(4)))))

(defvar my-saved-idle-time-seconds nil)

(defun mm/toggle-auto-clock-out (&optional arg)
  (interactive)
  (cond ((and my-idle-time-seconds my-idle-clock-out)
         (setq my-idle-clock-out nil)
         (message "Idle clock-out disabled"))
        ((and my-idle-time-seconds (not my-idle-clock-out))
         (setq my-idle-clock-out t)
         (message "Idle clock out set to %s seconds" my-idle-time-seconds)
         (my-start-idle-timer))
        ((null my-idle-time-seconds)
         (message "my-idle-time-seconds is not set"))))

;;
;; Fix undo to reveal stuff if it undid hidden shit
;;
(defadvice undo-tree-undo (around org-reveal activate)
  (if (not (org-mode-p)) (setq ad-return-value ad-do-it)
    (let ((old-reveal-mode reveal-mode))
      ;; (reveal-post-command)
      (setq ad-return-value ad-do-it)
      (reveal-post-command))))

(defadvice undo-tree-redo (around org-reveal activate)
  (if (not (org-mode-p)) (setq ad-return-value ad-do-it)
    (let ((old-reveal-mode reveal-mode))
      ;; (reveal-post-command)
      (setq ad-return-value ad-do-it)
      (reveal-post-command))))

;; fix wrong display of org-column face
(when (fboundp 'set-face-attribute)
  ;; Make sure that a fixed-width face is used when we have a column table.
  (set-face-attribute 'org-column nil
		      :height 'unspecified
		      :family 'unspecified))

;; (copy-face 'org-scheduled-today 'org-scheduled-previously)

(setq org-mobile-directory "~/Dropbox/org-mobile")
(setq org-mobile-files '(org-agenda-files))
(setq org-mobile-inbox-for-pull "~/my-org/Mobile_Inbox.org")
(setq org-mobile-agendas '("N"))

;; auto-insert TRIGGER when making entry next
(defun mm/org-insert-trigger ()
  (cond ((equal org-state "NEXT")
         (unless org-depend-doing-chain-find-next
           (org-set-property "TRIGGER" "chain-find-next(NEXT,from-current,priority-up,effort-down)")))
        ((not (member org-state org-done-keywords))
         (org-delete-property "TRIGGER"))))

(add-hook 'org-after-todo-state-change-hook 'mm/org-insert-trigger)

(defun mm/orgmode-startup ()
  (cond ((and (boundp 'server-name)
              (equal server-name "emacs"))
         (my-start-idle-timer)
         (add-hook 'org-clock-out-hook 'mm/clock-out-maybe 'append)
         (add-hook 'post-command-hook 'my-idle-post-command-hook)
         (message "Idle auto clock-out started"))
        (t (message "Idle auto clock-out disabled because server is %s"
                    (if (boundp 'server-name) server-name "unknown")))))

(defadvice org-store-log-note (around maybe-clock-out activate)
  (let ((mm/marker (copy-marker org-log-note-marker)))
    (setq ad-return-value ad-do-it)
    (when (and (org-clocking-p)
               (equal
                (with-current-buffer (marker-buffer mm/marker)
                  (goto-char mm/marker)
                  (org-back-to-heading t)
                  (point-marker))
                org-clock-hd-marker))
      (with-current-buffer (marker-buffer mm/marker)
        (org-with-point-at org-clock-hd-marker)
        (org-clock-out)))))

(defun mm/clock-out-when-done-hook ()
  (when (and (org-clocking-p)
             (not (member 'org-add-log-note post-command-hook)))
    (let ((org-clock-out-when-done t))
      (org-clock-out-if-current))))

(add-hook 'org-after-todo-state-change-hook 'mm/clock-out-when-done-hook)

(if nil 
    (add-to-list
     'auto-insert-alist
     '(("\\.org" . "Org-Mode file")
       nil
       "#+STARTUP: lognotedone\n"
       "#+FILETAGS: :proj:\n"
       "#+Category: " (file-name-nondirectory
                       (file-name-sans-extension buffer-file-name))
       "\n\n"
       "* " (file-name-nondirectory
             (file-name-sans-extension buffer-file-name))
       "\n")))


(defun mm/insert-parent-headline-cookie (&optional arg)
  "Insert [0/3] statistics cookie at the end of the current
headline. With C-u argument use parent headline, with C-u C-u use
grandparent and so on"
  (interactive "p")
  (save-excursion
    (org-back-to-heading)
    (decf arg 4)
    (while (>= arg 0)
      (org-up-heading-safe)
      (decf arg 4))
    (let ((pos (re-search-forward "\\([[:space:]]*:\\|[[:space:]]*$\\)" (line-end-position) t)))
      (when pos
        (goto-char (match-beginning 1))
        (unless (looking-back "\\(\\(\\[[0-9]*%\\]\\)\\|\\(\\[[0-9]*/[0-9]*\\]\\)\\)")
          (insert " [0/0]")
          (call-interactively 'org-update-statistics-cookies))))))


(setq org-clock-in-re-clock 1)

(evil-define-key 'normal org-mode-map ";ic" 'mm/insert-parent-headline-cookie)

(defun mm/org-clock-goto (&optional select)
  "Same as `org-clock-goto' but first check if item is in Agenda,
and if so go there"
  (interactive "@P")
  (let* ((recent nil)
         agenda-buffer agenda-pos mm
         capture-buffer pos pos-heading
	 (m (cond
	     (select
	      (or (org-clock-select-task "Select task to go to: ")
		  (error "No task selected")))
	     ((org-clocking-p) org-clock-marker)
	     ((and org-clock-goto-may-find-recent-task
		   (car org-clock-history)
		   (marker-buffer (car org-clock-history)))
	      (setq recent t)
	      (car org-clock-history))
	     (t (error "No active or recent clock task"))))
         (m-heading (with-current-buffer (marker-buffer m)
                      (save-excursion
                        (goto-char m)
                        (org-back-to-heading t)
                        (point-marker)))))
    (cond
     ((dolist (buffer (buffer-list) capture-buffer)
        (if (and (buffer-base-buffer buffer)
                 (eq (buffer-base-buffer buffer) (marker-buffer m-heading)))
            (setq capture-buffer buffer)))
      (with-current-buffer (marker-buffer m-heading)
        (save-excursion
          (save-restriction
            (goto-char m)
            (setq pos (point))
            (goto-char m-heading)
            (setq pos-heading (point)))))
      (org-pop-to-buffer-same-window capture-buffer)
      (save-excursion
        (save-restriction
          (goto-char pos)
          (setq m (point-marker))
          (goto-char pos-heading)
          (setq m-heading (point-marker))))
      (if (or (< m (point-min)) (> m (point-max))) (widen))
      (goto-char m)
      (org-show-entry) 
      (org-back-to-heading t) 
      (org-cycle-hide-drawers 'children)
      (recenter)
      (org-reveal))
     ;; ((and (setq agenda-buffer (get-buffer org-agenda-buffer-name))
     ;;       (with-current-buffer agenda-buffer
     ;;         (save-excursion
     ;;           (goto-char (point-min))
     ;;           (while (and (not (eobp))
     ;;                       (not agenda-pos))
     ;;             (when (setq mm (or (org-get-at-bol 'org-hd-marker)
     ;;                                (org-get-at-bol 'org-marker)))
     ;;               (when (and (equal (marker-buffer mm)
     ;;                                 (marker-buffer m-heading))
     ;;                          (equal (marker-position mm)
     ;;                                 (marker-position m-heading))
     ;;                          (not (equal (org-get-at-bol 'type) "closed")))
     ;;                 (setq agenda-pos (point))))
     ;;             (beginning-of-line 2)))
     ;;         agenda-pos))
     ;;  (org-pop-to-buffer-same-window agenda-buffer)
     ;;  (with-current-buffer agenda-buffer
     ;;    (goto-char agenda-pos)))
     (t
      (org-pop-to-buffer-same-window (marker-buffer m))
      (if (or (< m (point-min)) (> m (point-max))) (widen)) 
      (let ((orig-point (point))
            (end (save-excursion (org-end-of-subtree t)))
            new-point)
        (save-excursion
          (goto-char m)
          (org-show-entry) 
          (org-back-to-heading t)
          (org-cycle-hide-drawers 'children)
          (recenter)
          (org-reveal)
          (setq new-point (point)))
        (if (and (> orig-point m)
                 (< orig-point end))
            (goto-char orig-point)
          (goto-char new-point)))))
    (if recent
        (message "No running clock, this is the most recently clocked task")) 
    (run-hooks 'org-clock-goto-hook)))

(defalias 'org-clock-goto 'mm/org-clock-goto)


(defun mm/org-clock-update-headline ()
  "Update the `org-clock-heading' variable"
  (when (org-clocking-p)
    (with-current-buffer (org-clocking-buffer)
      (save-excursion
        (goto-char org-clock-marker)
        (org-back-to-heading t)
        (setq org-clock-heading
              (cond ((and org-clock-heading-function
                          (functionp org-clock-heading-function))
                     (funcall org-clock-heading-function))
                    ((looking-at org-complex-heading-regexp)
                     (replace-regexp-in-string
                      "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1"
                      (match-string 4)))
                    (t "???")))
        (org-clock-update-mode-line)))))

(add-hook 'org-capture-after-finalize-hook 'mm/org-clock-update-headline)

(add-hook 'emacs-startup-hook 'mm/orgmode-startup)

(defun org-clocktable-sort-clock-data (tables params)
  "TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property
list obtained from the dynamic block defintion.

When PARAMS contains a :SORT entry, sort the tables and the entries
inside them accordnly:

:SORT t, sorts by most time spent on top
:SORT time-up, sorts by most time spent on top
:SORT time-down, sorts by least time spent on top

Returns the sorted table list"
  (let ((sort (plist-get params :sort)))
    (if (not sort) tables
      (sort (mapcar
             (lambda (table)
               (list (nth 0 table)
                     (nth 1 table)
                     (sort
                      (third table)
                      (lambda (elem1 elem2)
                        (let ((d1 (nth 3 elem1))
                              (d2 (nth 3 elem2)))
                          (cond ((memq sort '(t time-up))
                                 (> d1 d2))
                                ((eq sort 'time-down)
                                 (< d1 d2))
                                (t (error "Invalid :sort parameter %s" sort))))))))
             tables)
            (lambda (table1 table2)
              (let ((d1 (nth 1 table1))
                    (d2 (nth 1 table2)))
                (cond ((memq sort '(t time-up))
                       (> d1 d2))
                      ((eq sort 'time-down)
                       (< d1 d2))
                      (t (error "Invalid :sort parameter %s" sort)))))))))

(defun mm/clocktable-formatter (pos tables props)
  (org-clocktable-write-default pos (org-clocktable-sort-clock-data tables props) props))

(defun mm/org-clock-decrement-start (arg)
  (interactive "P")
  (if (not (org-clocking-p))
      (error "Need to be clocking")
    (if (or (not (second org-clock-history))
            (not (marker-buffer (second org-clock-history))))
        (error "Need to have previous task"))))


(defvar mm/ib-main-id "05ea068f-5736-4476-9567-38db526fb40f")
(defvar mm/ib-sep-id "88567b9e-955d-490e-ba29-8f7bf22afd88")

(defun mm/ib-numbers (&optional id)
  (or id (setq id mm/ib-main-id))
  (org-with-point-at (org-id-find id 'marker)
    (save-restriction
      (org-narrow-to-subtree)
      (let ((done nil)
            (expr nil))
        (while (and (not done)
                    (not (eobp)))
          (forward-line 1)
          (when (looking-at "^[ \t]*(")
            (setq expr (read (current-buffer)))
            (setq done t)))
        expr))))

(defun mm/ib-id-copy (n1 n2)
  (interactive "nEnter first number: \nnEnter second number: ")
  (let* ((list (mm/ib-numbers))
         (c1 (plist-get list n1))
         (c2 (plist-get list n2)))
    (when (and c1 c2)
      (message "Code is: %s %s" c1 c2)
      (x-set-selection 'CLIPBOARD (format "%s%s" c1 c2)))))


;; (require 'org-agenda-cache)

(setq org-agenda-window-setup 'other-window
      org-agenda-restore-windows-after-quit t)

(defun org-fast-todo-selection ()
  "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur."
  (let* ((fulltable org-todo-key-alist)
	 (done-keywords org-done-keywords) ;; needed for the faces.
	 (maxlen (apply 'max (mapcar
			      (lambda (x)
				(if (stringp (car x)) (string-width (car x)) 0))
			      fulltable)))
	 (expert t)
	 (fwidth (+ maxlen 3 1 3))
	 (ncol (/ (- (window-width) 4) fwidth))
	 tg cnt e c tbl
	 groups ingroup)
    (save-excursion
      (save-window-excursion
	(if expert
	    (set-buffer (get-buffer-create " *Org todo*"))
	  (org-switch-to-buffer-other-window (get-buffer-create " *Org todo*")))
	(erase-buffer)
	(org-set-local 'org-done-keywords done-keywords)
	(setq tbl fulltable cnt 0)
	(while (setq e (pop tbl))
	  (cond
	   ((equal e '(:startgroup))
	    (push '() groups) (setq ingroup t)
	    (when (not (= cnt 0))
	      (setq cnt 0)
	      (insert "\n"))
	    (insert "{ "))
	   ((equal e '(:endgroup))
	    (setq ingroup nil cnt 0)
	    (insert "}\n"))
	   ((equal e '(:newline))
	    (when (not (= cnt 0))
	      (setq cnt 0)
	      (insert "\n")
	      (setq e (car tbl))
	      (while (equal (car tbl) '(:newline))
		(insert "\n")
		(setq tbl (cdr tbl)))))
	   (t
	    (setq tg (car e) c (cdr e))
	    (if ingroup (push tg (car groups)))
	    (setq tg (org-add-props tg nil 'face
				    (org-get-todo-face tg)))
	    (if (and (= cnt 0) (not ingroup)) (insert "  "))
	    (insert "[" c "] " tg (make-string
				   (- fwidth 4 (length tg)) ?\ ))
	    (when (= (setq cnt (1+ cnt)) ncol)
	      (insert "\n")
	      (if ingroup (insert "  "))
	      (setq cnt 0)))))
	(insert "\n")
	(goto-char (point-min))
	(if (not expert) (org-fit-window-to-buffer))
	(message "[a-z..]:Set [SPC]:clear")
	(setq c (let ((inhibit-quit t)) (read-char-exclusive)))
	(cond
	 ((or (= c ?\C-g)
	      (and (= c ?q) (not (rassoc c fulltable))))
	  (setq quit-flag t))
	 ((= c ?\ ) nil)
	 ((setq e (rassoc c fulltable) tg (car e))
	  tg)
	 (t (setq quit-flag t)))))))

(remove-hook 'org-tab-first-hook 'org-babel-header-arg-expand)

(defadvice org-agenda-quit (around dont-do-it-on-dedicated-window activate)
  (if (window-dedicated-p)
      (let* ((arg 1)
             (frame (selected-frame)))
        (while (> arg 0)
          (setq frame (next-frame frame))
          (while (or (not (eq (frame-visible-p frame) t))
                     (window-dedicated-p (frame-first-window frame)))
            (setq frame (next-frame frame)))
          (setq arg (1- arg)))
        (while (< arg 0)
          (setq frame (previous-frame frame))
          (while (or (not (eq (frame-visible-p frame) t))
                     (window-dedicated-p (frame-first-window frame)))
            (setq frame (previous-frame frame)))
          (setq arg (1+ arg)))
        (select-frame-set-input-focus frame))
    (setq ad-return-value ad-do-it)))

(defun mm/refile-to-diary (&optional date)
  "Refile an entry to journal file's date-tree"
  (interactive)
  (require 'org-datetree)
  (let ((journal "~/my-org/Diary.org")
        post-date)
    (setq post-date (or date
                        (org-entry-get (point) "TIMESTAMP_IA")
                        (org-entry-get (point) "TIMESTAMP")))
    (setq post-date (nthcdr 3 (parse-time-string post-date)))
    (setq post-date (list (cadr post-date) 
                          (car post-date) 
                          (caddr post-date)))
    (org-cut-subtree)
    (with-current-buffer (or (find-buffer-visiting journal)
                             (find-file-noselect file))
      (save-excursion
        (let ((org-blank-before-new-entry
               '((heading . t)))) 
          (org-datetree-file-entry-under (current-kill 0) post-date) 
          (bookmark-set "org-refile-last-stored"))))
    (message "Refiled to %s" journal)))

(defun mm/fixup-good-bad ()
  (interactive)
  (let* ((start (save-excursion (org-back-to-heading t)
                                (point-marker)))
         (level (save-excursion
                  (goto-char start)
                  (org-outline-level)))
         (end (save-excursion
                (goto-char start)
                (org-end-of-subtree t)
                (point-marker)))
         (case-fold-search t)
         (sublevel (make-string (1+ level) ?\*)))
    (save-excursion
      (goto-char start)
      (forward-line 1) 
      (while (< (point) end)
        (cond
         ((looking-at "^\\( *-?good \\)")
          (replace-match (format "%s GOOD " sublevel) nil nil nil 1))
         ((looking-at "^\\( *-?bad \\)")
          (replace-match (format "%s BAD " sublevel) nil nil nil 1)))
        (forward-line 1)))))

(defun mm/refile-mobile ()
  "Refile the mobile entry into diary, and fix it up"
  (interactive)
  ;; Find date
  (let* ((start (save-excursion
                  (org-back-to-heading)
                  (while (> (org-outline-level) 1)
                    (org-up-heading-safe))
                  (point-marker)))
         (end (save-excursion
                (goto-char start)
                (org-end-of-subtree t)
                (point-marker)))
         (now (format-time-string (org-time-stamp-format)
                                  (seconds-to-time (float-time (org-current-time)))))
         (yesterday (format-time-string (org-time-stamp-format)
                                        (seconds-to-time
                                         (- (float-time (org-current-time))
                                            (* 60 60 24)))))
         (case-fold-search t)
         (ts now))
    (save-excursion
      (goto-char start)
      (when (looking-at "^\* \\(NEW \\|TODO \\)?\\(Yesterday\\|Today\\)")
        (setq ts yesterday)
        (when (match-string 1) 
          (replace-match "" nil nil nil 1)))
      (goto-char start)
      (mm/fixup-good-bad) 
      (goto-char start) 
      (mm/refile-to-diary ts))))

(require-if-available 'ox-gfm)

(provide 'my-orgmode-setup)

