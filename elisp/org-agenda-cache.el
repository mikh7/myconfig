;;; org-agenda-cache.el --- Cache agenda buffers instead of regenerating them each time

;; Cached agenda buffers
;;
;; Toggle with M-x toggle-org-agenda-caching
;;
;; If you include `org-agenda-buffer-name' into the list of bound
;; variables for a specific agenda view, then these agenda views
;; will be cached independently each in their own buffer
;;
;; Example setup:
;;
;; (setq org-agenda-custom-commands
;;       '(("a" "Agenda and NEXT (priority)"
;;          ((agenda ""
;;                   ((org-agenda-span 'day)))
;;           (tags-todo "/!NEXT"
;;                      ((org-agenda-overriding-header "Next Tasks")
;;                       (org-agenda-tags-todo-honor-ignore-options t)
;;                       (org-agenda-todo-ignore-scheduled t)
;;                       (org-agenda-todo-ignore-deadlines t)
;;                       (org-tags-match-list-sublevels t)
;;                       (org-agenda-sorting-strategy
;;                        '(priority-down category-keep)))))
;;          ((org-agenda-buffer-name "*Agenda*")))
;;         ("t" "TODO entries"
;;          todo ""
;;          ((org-agenda-buffer-name "*Todo List*")))))
;;
;; Note that C-c a and C-c t will use two different agenda buffers
;; that are indecent of each other, and each can have different restrictions
;; or tag filter

(defvar org-agenda-use-caching nil
  "When non-nil, org-agenda will show existing agenda buffer when
its available and org-agenda-exit will bury the agenda buffer
instead of destroying it. ")

(defun toggle-org-agenda-caching (&optional arg)
  (interactive)
  (setq org-agenda-use-caching (or arg (not org-agenda-use-caching)))
  (message "Agenda caching was %s" (if org-agenda-use-caching "enabled" "disabled")))

(defvar org-agenda-inside-org-agenda nil)
(defvar org-cached-agenda-last-prefix-arg nil)
(defvar org-this-agenda-buffer-name nil)

(dolist (var '(org-cached-agenda-last-prefix-arg
               org-this-agenda-buffer-name
               org-agenda-redo-command
               org-agenda-query-string
               org-agenda-tag-filter
               org-agenda-tag-filter-overlays
               org-agenda-category-filter
               org-agenda-columns-active
               org-agenda-marker-table
               org-agenda-markers
               org-agenda-restrict
               org-agenda-restrict-begin
               org-agenda-overriding-restriction
               org-agenda-archives-mode
               org-agenda-current-span))
  (make-variable-buffer-local var))

(defadvice org-agenda (around cached-agenda-buffer activate)
  (if org-agenda-use-caching
      (let ((org-agenda-inside-org-agenda t))
        (catch 'org-agenda-used-existing-buffer
          (setq ad-return-value ad-do-it)))
    (setq ad-return-value ad-do-it)))

(defadvice org-agenda-redo (around cached-agenda-buffer activate)
  (if org-agenda-use-caching
      (let ((org-agenda-buffer-name (or org-this-agenda-buffer-name
                                        org-agenda-buffer-name)))
        (setq ad-return-value ad-do-it))
    (setq ad-return-value ad-do-it)))

(defadvice org-prepare-agenda (around cached-agenda-buffer activate)
  (if org-agenda-use-caching
      (if (or (not org-agenda-inside-org-agenda)
              org-agenda-multi
              (not (get-buffer org-agenda-buffer-name))
              (with-current-buffer (get-buffer org-agenda-buffer-name)
                (not (equal
                      current-prefix-arg
                      org-cached-agenda-last-prefix-arg))))
          (progn
            (setq ad-return-value ad-do-it)
            (with-current-buffer (get-buffer org-agenda-buffer-name)
              (setq org-cached-agenda-last-prefix-arg current-prefix-arg)
              (setq org-this-agenda-buffer-name org-agenda-buffer-name)))
        ;; Below block cut-n-paste copied from real `org-prepare-agenda' function
        (let* ((abuf (get-buffer-create org-agenda-buffer-name))
               (awin (get-buffer-window abuf)))
          (cond
           ((equal (current-buffer) abuf) nil)
           (awin (select-window awin))
           ((not (setq org-pre-agenda-window-conf (current-window-configuration))))
           ((equal org-agenda-window-setup 'current-window)
            (org-pop-to-buffer-same-window abuf))
           ((equal org-agenda-window-setup 'other-window)
            (org-switch-to-buffer-other-window abuf))
           ((equal org-agenda-window-setup 'other-frame)
            (switch-to-buffer-other-frame abuf))
           ((equal org-agenda-window-setup 'reorganize-frame)
            (delete-other-windows)
            (org-switch-to-buffer-other-window abuf)))
          ;; additional test in case agenda is invoked from within agenda
          ;; buffer via elisp link
          (unless (equal (current-buffer) abuf)
            (org-pop-to-buffer-same-window abuf))
          ;; make org-agenda exit early
          (throw 'org-agenda-used-existing-buffer nil)))
    (setq ad-return-value ad-do-it)))

(defadvice org-agenda-quit (around cached-agenda-buffer activate)
  (if org-agenda-use-caching
      (org-agenda-bury)
    (setq ad-return-value ad-do-it)))

(defun org-agenda-bury ()
  "Restore window or frame configuration just like `org-agenda-quit' does
but bury the agenda buffer rather then killing it."
  (interactive)
  (let ((buf (current-buffer)))
    (if (eq org-agenda-window-setup 'other-frame)
        (progn
          (delete-frame))
      (and (not (eq org-agenda-window-setup 'current-window))
           (not (one-window-p))
           (delete-window)))
    (with-current-buffer buf
        (bury-buffer))))

(provide 'org-agenda-cache)
