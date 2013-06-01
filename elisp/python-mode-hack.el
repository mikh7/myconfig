;; Evaluation of code in the python.el which comes with Emacs 22 sends
;; the result to the emacs namespace. Not good. Hack around this, by
;; passing in the global namspace into the emacs module from
;; outside. Requires changes to python-send-region in python.el, and
;; eexecfile in emacs.py

;; Here is the hacked portion of python.el
(defun python-send-region (start end)
  "Send the region to the inferior Python process."
  (interactive "r")
  (let* ((f (make-temp-file "py"))
	 ;;(command (format "emacs.eexecfile(%S)" f)) ; Here we must allow passing in of the scopes
	 (command (format "emacs.eexecfile(%S, globals(), globals())" f))
	 (orig-start (copy-marker start)))
    (when (save-excursion
	    (goto-char start)
	    (/= 0 (current-indentation)))
      (save-excursion
	(goto-char orig-start)
	(set-marker orig-start (line-beginning-position 0)))
      (write-region "if True:\n" nil f nil 'nomsg))
    (write-region start end f t 'nomsg)
    (with-current-buffer (process-buffer (python-proc))
      (python-send-command command)
      (compilation-fake-loc orig-start f))))

;; And now inject the hacked portion of emacs.py into the emacs namespace
(defun python-replace-emacs-eexecfile ()
  (interactive)
  (message "Working around bug in python.el :replacing emacs.eexecfile and python-send-region")
  (python-send-string
"def eexecfile (file, glbl, locl):
  try:
    execfile (file, glbl, locl) # Here is our hack
  except:
    (type, value, tb) = emacs.sys.exc_info ()
    tb = tb.tb_next
    if tb is None:
      print 'Traceback (most recent call last):'
      emacs.traceback.print_exception (type, value, tb)
  finally:
    emacs.os.remove (file)

emacs.eexecfile = eexecfile
del eexecfile
"))

(defadvice run-python (after replace-eexecfile first () activate)
  (python-replace-emacs-eexecfile))

