;; parse treasury auction schedule file
;; and produce org-mode tree of auction 
;; dates in the buffer called "out"

(defun auc-string-to-date (string)
  (let ((time (parse-time-string string)))
    (encode-time 0 0 0 (elt time 3) (elt time 4) (elt time 5))))

(defun parse-auction-skd (file-name)
  (interactive "f")
  (let (auc-list) 
    (with-current-buffer (generate-new-buffer "out")
      (insert-file-contents file-name)
      (dolist (op '(("January" "Jan") ("February" "Feb") ("March" "Mar")
		    ("April" "Apr") ("May" "May") ("June" "Jun")
		    ("July" "Jul") ("August" "Aug") ("September" "Sep")
		    ("October" "Oct") ("November" "Nov") ("December" "Dec")
		    ))
	(beginning-of-buffer)
	(perform-replace (first op) (second op) nil nil nil))
      (beginning-of-buffer)
      (while (not (eobp))
	(when (looking-at "^,\\([^,]+\\),\"\\([^\"]+\\)\",\"\\([^\"]+\\)\",\"\\([^\"]+\\)\"")
	  (let* ((dsc (match-string 1))
		 (ann-date-str (match-string 2))
		 (auc-date-str (match-string 3))
		 (set-date-str (match-string 4))
		 (ann-date (auc-string-to-date ann-date-str))
		 (auc-date (auc-string-to-date auc-date-str))
		 (set-date (auc-string-to-date set-date-str)))
	    (push (list dsc ann-date auc-date set-date) auc-list)))
	(next-line 1))
      (erase-buffer)
      (insert "* Treasury Auctions\n")
      (dolist (auc (reverse auc-list))
	(insert (format "** %s\n" (first auc)))
	(insert (format "*** %s Annoncement\n    <%s>\n"
			(first auc)
			(format-time-string "%Y-%m-%d" (second auc))))
	(insert (format "*** %s Auction\n   <%s>\n"
			(first auc)
			(format-time-string "%Y-%m-%d" (third auc))))
	(insert (format "*** %s Settlement\n    <%s>\n"
			(first auc)
			(format-time-string "%Y-%m-%d" (fourth auc)))))
      )))
      
(provide 'parse-auction-skd)




