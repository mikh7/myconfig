(defun make-undo-checkpoint ()
  (let ((checkpoint (gensym)))
    (push checkpoint buffer-undo-list)
    checkpoint))

(defun do-undo-until-checkpoint (checkpoint)
  (let ((ptr buffer-undo-list) prev found rest-of-undo)
    (while (and ptr (not found))
      (if (not (eq (car ptr) checkpoint))
          (setq prev ptr ptr (cdr ptr))
        (setq found t)))
    (when found
      ;; rest of undo list after our tag
      (setq rest-of-undo (cdr ptr))
      ;; split the undo list by putting NIL at the place of 
      ;; our tag
      (if (not prev)
          (setq buffer-undo-list nil)) ;; our tag was the 1st element
      (setcdr prev nil)                ;; previous elemen is now last
      (undo-start)
      (while (listp pending-undo-list)
        (undo-more 1)))))

(defvar checkpoints nil)
(make-variable-buffer-local 'checkpoints)

(defun record-undo-checkpoint (&optional arg)
  (interactive)
  (push (make-undo-checkpoint) checkpoints))


(defun undo-until-checkpoint (&optional arg)
  (interactive)
  (do-undo-until-checkpoint (pop checkpoints)))
  
(defun check-undo ()
  (interactive)
  (message "len=%s" (length buffer-undo-list))
  (message "copy=%s" (length (undo-copy-list buffer-undo-list))))

