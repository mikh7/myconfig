
;; in customize instead, also see guess-python-offset
;; which autoguesses it for every opened .py file
;; (setq python-indent-offset 2)

(require 'python)
(require 'comint)
(require 'shell)

;; fix for the namespaces 

;; Here is the hacked portion of python.el
(when (< emacs-major-version 23)
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

  ;; hacked version also with fix for namespaces
  (defun python-load-file (file-name)
    "Load a Python file FILE-NAME into the inferior Python process.
If the file has extension `.py' import or reload it as a module.
Treating it as a module keeps the global namespace clean, provides
function location information for debugging, and supports users of
module-qualified names."
    (interactive (comint-get-source "Load Python file: " python-prev-dir/file
                                    python-source-modes
                                    t))	; because execfile needs exact name
    (comint-check-source file-name) ; Check to see if buffer needs saving.
    (setq python-prev-dir/file (cons (file-name-directory file-name)
                                     (file-name-nondirectory file-name)))
    (with-current-buffer (process-buffer (python-proc)) ;Runs python if needed.
      ;; Fixme: I'm not convinced by this logic from python-mode.el.
      (python-send-command
       (if (string-match "\\.py\\'" file-name)
           (let ((module (file-name-sans-extension
                          (file-name-nondirectory file-name))))
             (format "emacs.eimport(%S,%S, globals(), globals())"
                     module (file-name-directory file-name)))
         (format "emacs.eexecfile(%S, globals(), globals())" file-name)))
      (message "%s loaded" file-name)))

  ;; And now inject the hacked portion of emacs.py into the emacs namespace
  (defun python-replace-emacs-eexecfile ()
    (interactive)
    (message "Working around bug in python.el :replacing emacs.eexecfile and python-send-region")
    (python-send-string
     "
import emacs

def eexecfile (file, glbl, locl):
  try:
    execfile (file, glbl, locl) # Here is our hack
  except:
    (type, value, tb) = emacs.sys.exc_info ()
    #tb = tb.tb_next
    #if tb is None:
    print 'Traceback (most recent call last):'
    emacs.traceback.print_exception (type, value, tb)
  finally:
    emacs.os.remove (file)
  return

def eimport (mod, dir, glbl, locl):
    path0 = emacs.sys.path[0]
    emacs.sys.path[0] = dir
    try:
        try:
            if glbl.has_key(mod) and emacs.inspect.ismodule (eval (mod, glbl, locl)):
                reload(eval (mod, glbl, locl))
            else:
                glbl[mod] = __import__ (mod, glbl, locl)
        except:
            (type, value, tb) = emacs.sys.exc_info ()
            print 'Traceback (most recent call last):'
            emacs.traceback.print_exception (type, value, tb.tb_next)
    finally:
        emacs.sys.path[0] = path0

emacs.eexecfile = eexecfile
emacs.eimport = eimport

"))

  (defadvice run-python (after replace-eexecfile first () activate)
    (python-replace-emacs-eexecfile)))

;; fix the viper key bindings
(vimpulse-define-key 'inferior-python-mode 'vi-state 
                    "\C-m" 'viper-comint-enter)
(vimpulse-define-key 'inferior-python-mode 'insert-state 
                    "\C-m" 'viper-exec-key-in-emacs)
(vimpulse-define-key 'inferior-python-mode 'vi-state "j" 'viper-comint-j)
(vimpulse-define-key 'inferior-python-mode 'vi-state "k" 'viper-comint-k)
(vimpulse-define-key 'inferior-python-mode 'vi-state 
                    "/" 'viper-comint-start-search)
(vimpulse-define-key 'inferior-python-mode 'vi-state 
                    "n" 'viper-comint-search-next)
(vimpulse-define-key 'inferior-python-mode 'vi-state 
                    "N" 'viper-comint-search-prev)


(defun mgm-after-inferior-python-mode ()
  (setq comint-input-ring-file-name "~/.pyhistory")
  (comint-read-input-ring)
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'shell-write-history-on-exit)
  (add-hook 'kill-buffer-hook #'comint-write-input-ring nil t))


(global-set-key "\C-cp" 'python-shell-switch-to-shell)
(define-key python-mode-map "\C-cr" 'python-shell-switch-to-shell)
(define-key inferior-python-mode-map "\C-cr" 'python-shell-switch-to-shell)


(provide 'my-python-setup)
