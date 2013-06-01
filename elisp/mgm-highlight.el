;;; 
;;; Flexible highlighting thing
;;;

;;; temporary highlight region, until timeout or next command
(defvar mgm-highlight-overlay nil)

(defun mgm-highlight-region (face start end buffer timeout)
  (if mgm-highlight-overlay
      (move-overlay mgm-highlight-overlay start end buffer)
    (setq mgm-highlight-overlay (make-overlay start end buffer)))
  (overlay-put mgm-highlight-overlay 'face face)
  (add-hook 'pre-command-hook 'mgm-highlight-remove)
  (when timeout
    (run-at-time timeout nil 'delete-overlay mgm-highlight-overlay)))

(defun mgm-highlight-remove ()
  (when (overlay-buffer mgm-highlight-overlay)
    (delete-overlay mgm-highlight-overlay)
    (remove-hook 'pre-command-hook 'mgm-highlight-overlay)))

(provide 'mgm-highlight)
