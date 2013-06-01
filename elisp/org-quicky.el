
(defvar org-quicky-target-file nil
  "Org file into which a quicky will be inserted")

(defvar org-quicky-target-regexp nil
  "Regexp to search the place to insert the quicky in the
  `org-quicky-target-file'. Quicky will be inserted as an element
  after the regexp")

(defun org-quicky-add-todo (&optional arg)
  "Add a todo item to Quick items"
  (interactive)
  (unless (and org-quicky-target-file org-quicky-target-regexp)
    (error "Please customize org-quicky-target-file and org-quicky-target-regexp"))
  
  (let* ((orig-buffer (find-file-noselect org-quicky-target-file))
         (buffer (make-indirect-buffer orig-buffer "*org-quicky*"))
         pos level)
    (with-current-buffer buffer
     (atomic-change-group
       (org-mode)
       ;; coz we don't want to modify the org-mode-map
       (use-local-map (copy-keymap (current-local-map)))
       (goto-char (point-min))
       (unless (setq pos (re-search-forward org-quicky-target-regexp))
         (error "Regexp not found"))
       (beginning-of-line)
       (unless (looking-at outline-regexp)
         (error "Regexp did not find a heading"))
       (setq level (org-outline-level))
       (setq pos (save-excursion 
                   (outline-get-next-sibling)))
       ;; if not found go to the end of buffer
       (unless pos
         (goto-char (point-max))
         (beginning-of-line)
         (when (looking-at outline-regexp) ; if last line is a header
           (end-of-line)                ; insert the newline after it
           (newline))
         (setq pos (point)))
       (goto-char pos)
       (org-insert-todo-subheading nil)
       (org-narrow-to-subtree)
       (pop-to-buffer buffer)
       (viper-change-state-to-insert)
       (end-of-line)
       (local-set-key "\C-c\C-c" 'org-quicky-finish)
       (local-set-key "\C-c\C-K" 'org-quicky-cancel)
       ))))

(defun org-quicky-finish (&optional arg)
  "Close the quick todo window and save the file"
  (interactive)
  (save-buffer)
  (kill-buffer nil)
  (delete-window))

(defun org-quicky-cancel (&optional arg)
  "Close the quick todo window and undo changes"
  (interactive)
  (save-buffer)
  (kill-buffer nil)
  (delete-window))



(defun org-quicky-get-heading (&optional no-props)
  (if (looking-at "^\\*+[ \t]+\\([^\r\n]*?\\)[ \t]*\\(:[a-zA-Z0-9:_@]+\\)?[ \t]*[\r\n]")	
      (if no-props (org-match-string-no-properties 1) 
        (match-string 1)) ""))

(defun org-quicky-get-toplevel-headings ()
  "Return a list of top level headings"
  (let (headings) 
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when (and (looking-at outline-regexp)
                   (= (org-outline-level) 1))
          (push (cons (org-quicky-get-heading t) (point-marker)) headings))
        (forward-line)))
    (nreverse headings)))

(defmacro when* (expr &rest body)
  `(let ((it ,expr))
     (when it
       ,@body)))


(defvar org-quicky-refile-history nil)

(defun org-quicky-refile (&optional arg)
  (interactive)
  (let* ((headings (org-quicky-get-toplevel-headings))
         (completion-ignore-case t)
         pos)
    (when* (completing-read 
            "Project: " (mapcar #'car headings)
            nil t nil 'org-quicky-refile-history)
           (setq pos (cdr (assoc it headings)))
           (org-cut-special)
           (save-excursion 
             (goto-char pos)
             (setq pos (or (save-excursion 
                             (outline-get-next-sibling))
                           (point-max)))
             (goto-char pos)
             (org-paste-subtree 2)))))

(provide 'org-quicky)
