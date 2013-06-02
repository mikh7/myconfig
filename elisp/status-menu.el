(require 'cl)

(defvar status-menu-callback nil
  "Function to call when selection changes, or timeout occurs The
signature of the function should be:
  (lambda (event) ...) 

Where event is one of (after-init selection-changed timeout dismiss)
  
")

(defvar status-menu-menu nil
  "Currently displayed menu")

(defvar status-menu-keymap nil)

(defvar status-menu-selection nil
  "Currently displayed menu level 0 being top level, 1 the first child etc")

(defvar status-menu-selection nil
  "Current selection in the status menu")

(defvar status-menu-dismissed nil)

(defvar status-menu-allow-esc-key t
  "Allow ESC key to cancel out of the menu")

(defvar status-menu-timeout nil
  "Timeout after which menu will be automatically dismissed. The
callback will be called with the 'timeout event. Can be
overwritten by specifying :timeout options to `status-menu-init' ")

(defvar status-menu-allow-other-commands t
  "When not-nil, status menu is modeless, other commands can be
executed while status menu is running. When nil then status menu
is modal and all the other commands are disabled while menu is
being navigated. Can be overwritten by specifying :allow-other-commands
option to `status-menu-init'")

(defvar status-menu-ignore-keys-silently nil
  "Do not beep when ignoring the keys because of
`status-menu-allow-other-commands' is nil")

(defvar status-menu-allow-enter-key t
  "When not-nil and `status-menu-allow-other-commands' is t, then
status menu will still intercept Enter key. Any other keys would
execute commands that they normally do. Can be overwritten by
specifying :allow-enter-key option to `status-menu-init' ")

(defvar status-menu-allow-arrow-keys nil
  "When not-nil and `status-menu-allow-other-commands' is t, then
status menu will still intercept left/right and home/end
keys. Any other keys would execute commands that they normally
do. Can be overwritten by specifying :allow-arrow-keys option to
`status-menu-init' ")

(defvar status-menu-allow-wrap t
  "When not-nil then `status-menu-forward' and
`status-menu-backward' commands wrap around at the end or start
of the menu")

(defconst status-menu-vars 
  '(status-menu-allow-enter-key status-menu-allow-esc-key
                                status-menu-allow-other-commands
                                status-menu-allow-arrow-keys
                                status-menu-timeout
                                status-menu-callback
                                status-menu-keymap))

(defvar status-menu-last-command nil)
(defvar status-menu-horiz-offset 0)

(defun status-menu-extract-submenu (menu selection)
  "Get a submenu encoded by selection path. selection is a list
where each element selects the n'th element of the menu, so
selection of (0 1 2) means (elt 2 (elt 1 (elt 0 menu)))"
  (do ((menu menu (elt menu (1+ (pop selection)))))
      ((null selection)
       menu)))

(defvar status-menu-current-line nil
  "The currently displayed menu text on the status line")

(defconst status-menu-commands 
  '(status-menu-forward status-menu-backward status-menu-dismiss status-menu-enter))

(defun status-menu-fix-horiz-offset (startpos endpos center)
  (setq status-menu-horiz-offset 0))

(defun status-menu-make-line (elements active center)
  "Generate a line to be displayed by the menu"
  (with-temp-buffer
    (let ((first t)
          (n 0)
          (pos (point-min))
          (startpos 0)
          (endpos 0)
          (display-start-pos 0)
          (deactivate-mark nil))
      (dolist (entry elements)
;;         (cond ((not first)
;;                (insert " ")
;;                (incf pos 1))
;;               (t (setq first nil)))
        (let ((str (format " %s " entry))
              (start pos))
          (insert str)
          (incf pos (length str))
          (when (= n active)
            (set-text-properties start pos '(face highlight))
            (setq startpos start)
            (setq endpos pos)))
        (incf n))
      (status-menu-fix-horiz-offset startpos endpos center)
      (buffer-substring (point-min)
                        (point-max)))))


(defun status-menu-current-item ()
  "Return the currently selected item"
  (elt status-menu-current-level (car (last status-menu-selection))))

(defun status-menu-remove-from-menu (menu item)
  "Remove element from tree"
  (cond ((null menu)
         '())
        ((atom menu)
          menu)
        ((equal (car menu) item)
         (cdr menu))
        (t (cons (status-menu-remove-from-menu (car menu) item)
                 (status-menu-remove-from-menu (cdr menu) item)))))

(defun status-menu-redisplay (&optional center)
  "Display the current status menu"
  (when (and (not status-menu-dismissed)
             status-menu-selection)
    (setq status-menu-current-level 
          (status-menu-extract-submenu status-menu-menu 
                                       (butlast status-menu-selection)))
    (setq status-menu-current-line 
          (status-menu-make-line status-menu-current-level (car (last status-menu-selection))
                                 center))
    (let ((message-log-max nil))
      (message "%s" (substring status-menu-current-line status-menu-horiz-offset
                               (length status-menu-current-line))))))

(defun status-menu-callback (event)
  (when status-menu-callback
    (funcall status-menu-callback event)))

(defvar status-menu-inside-cmdloop nil)

(defun status-menu-read-keys ()
  (let (
         ;minor-mode-map-alist 
         ;emulation-mode-map-alists
        )
    (read-key-sequence
     nil nil 'no-lower-case nil t)))

(defun status-menu-remove-shift (key)
  (when (and (typep key 'vector)
	     (= (length key) 1)
	     (symbolp (aref key 0)))
    (let ((name (symbol-name (aref key 0)))
	  new-name)
      (when (string-match "S-" name)
	(setq new-name 
	      (concat (substring name 0 (match-beginning 0))
		      (substring name (match-end 0))))
	(vector (make-symbol new-name))))))

(defun status-menu-key-binding (key)
  (or (let ((key (lookup-key status-menu-keymap key)))
        (when (and key (not (numberp key)))
          key))
      (key-binding key)
      (let ((key (remove-shift key)))
        (when key
          (key-binding key)))))

(defvar status-menu-default-keymap (make-sparse-keymap))
(define-key status-menu-default-keymap [f10] 'status-menu-forward)
(define-key status-menu-default-keymap [f9] 'status-menu-backward)

(defvar status-menu-esc-keymap (make-sparse-keymap))

(defvar status-menu-esc-maps 
  (list
   (cons 'status-menu-inside-cmdloop status-menu-esc-keymap)))

(defun status-menu-call-command (cmd keyseq)
  (let ((save-last-command-event last-command-event)
        (save-this-command this-command)
        (keys (if (vectorp keyseq)
                  keyseq (string-to-vector keyseq)))) 
      (let ((last-command-event (aref keys (1- (length keys))))) 
        (setq this-command cmd)
        (let ((status-menu-inside-cmdloop nil))
          (run-hooks 'pre-command-hook)
          (call-interactively cmd t keys)
          (cua--post-command-handler-1)
          (run-hooks 'post-command-hook)))))

(defun status-menu-cmdloop ()
  (unless status-menu-inside-cmdloop
    (let ((status-menu-inside-cmdloop t)
          (status-menu-dismissed nil)
          keyseq cmd
          (minor-mode-map-alist 
           (append (when status-menu-allow-esc-key
                     (list (cons 'status-menu-inside-cmdloop status-menu-esc-keymap)))
                   (list (cons 'status-menu-inside-cmdloop status-menu-keymap))
                   minor-mode-map-alist))
          (emulation-mode-map-alists 
           (append (when status-menu-allow-esc-key
                     (list 'status-menu-esc-maps))
                   emulation-mode-map-alists))
          )
      (while (not status-menu-dismissed)
        ;;(when (and (boundp 'cua-mode) cua-mode)
        ;; (cua--select-keymaps))
        ;; (message "cmdloop begin of loop")
        (cond ((and status-menu-timeout 
                    (let ((inhibit-quit t)) 
                      (sit-for status-menu-timeout)))
               ;; timed out
               ;; (message "cmdloop timed out")
               (status-menu-callback 'timeout))
              (t
               ;; have input
               (setq keyseq (status-menu-read-keys))
               (setq cmd (status-menu-key-binding keyseq))
               ;; (message "cmdloop keyseq=%s cmd=%s" keyseq cmd)
               (cond ((and status-menu-allow-esc-key
                           (or (equal keyseq [27])
                               (equal keyseq "\e")))
                      ;; use same idea as viper uses, which is figure out if stuff after ESC key
                      ;; translates into a keyseq, and if so treat it as a function key, otherwise
                      ;; treat it as ESC out of the menu
                      (setq keyseq (read-key-sequence nil))
                      (setq cmd (status-menu-key-binding keyseq))
                      ;; (message "cmdloop ESC key enveloping result keyseq=%s cmd=%s" keyseq cmd)
                      (if (or (equal keyseq [27])
                              (equal keyseq "\e"))
                          (progn 
                            (status-menu-callback 'esc))
                        ;; thing starting with ESC tranlated into a command
                        ;; most likely thru function key
                        (if (and status-menu-allow-other-commands cmd)
                            ;; viper puts a command on ESC key, which 
                            ;; reads rest of the characters and decides on the fly
                            ;; if its en Esc o or a function key, in which case
                            ;; viper calls command bound to the function key itself
                            ;;
                            ;; So when we come here and in viper mode, after having
                            ;; pressed lets say F10, the cmd is viper-intercept-ESC-key. 
                            ;; Calling that will extract rest of the input and find
                            ;; what F10 is bound to.
                            ;; 
                            ;; We need to know if a status menu command was executed using
                            ;; the above round-about method so all status menu commands 
                            ;; set the `status-menu-last-command' to itself.
                            ;;
                            (let ((status-menu-last-command nil))
                              (status-menu-call-command cmd keyseq)
                              (unless status-menu-last-command
                                (status-menu-callback 'other-command)))
                          (unless status-menu-ignore-keys-silently
                            (beep)))
                        (status-menu-redisplay)))
                     ((or (equal keyseq [7]) 
                          (equal keyseq "\C-g"))
                      (status-menu-callback 'quit)
                      (message "Quit from menu")
                      (beep)
                      (setq status-menu-dismissed t))
                     ((memq cmd status-menu-commands)
                      (status-menu-call-command cmd keyseq))
                     ((and status-menu-allow-enter-key
                           (or (equal keyseq [13])
                               (equal keyseq "\C-m")))
                      (let ((status-menu-inside-cmdloop nil))
                        (call-interactively 'status-menu-enter t)))
                     ((and status-menu-allow-other-commands cmd)
                      ;; viper puts a command on ESC key, which 
                      ;; reads rest of the characters and decides on the fly
                      ;; if its en Esc o or a function key, in which case
                      ;; viper calls command bound to the function key itself
                      ;;
                      ;; So when we come here and in viper mode, after having
                      ;; pressed lets say F10, the cmd is viper-intercept-ESC-key. 
                      ;; Calling that will extract rest of the input and find
                      ;; what F10 is bound to.
                      ;; 
                      ;; We need to know if a status menu command was executed using
                      ;; the above round-about method so all status menu commands 
                      ;; set the `status-menu-last-command' to itself.
                      ;;
                      (let ((status-menu-last-command nil))
                        (status-menu-call-command cmd keyseq)
                        (unless status-menu-last-command
                          (status-menu-callback 'other-command)))
                      (status-menu-redisplay))
                     (t 
                      (unless status-menu-ignore-keys-silently
                        (beep))
                      (status-menu-redisplay)))))))))

(defun status-menu-init (menu initial-selection &rest args)
  "Setup and Display the corresponding status menu"
  (progv status-menu-vars (mapcar #'symbol-value status-menu-vars)
    (let (arg value initial-center)
      (while args
        (setq arg (pop args))
        (unless args
          (error "status-menu-init: argument contains odd number of option/value pairs"))
        (setq value (pop args))
        (case arg 
          (:timeout (setq status-menu-timeout value))
          (:allow-other-commands (setq status-menu-allow-other-commands value))
          (:allow-enter-key (setq status-menu-allow-enter-key value))
          (:allow-esc-key (setq status-menu-allow-esc-key value))
          (:callback (setq status-menu-callback value))
          (:initial-center (setq initial-center value))
          (:ignore-keys-silently (setq status-menu-ignore-keys-silently value))
          (:keymap (setq status-menu-keymap value))
          (:allow-wrap (setq status-menu-allow-wrap value))
          (t (message "Unknown status menu option %s" arg))))
      (when (null status-menu-keymap)
        (setq status-menu-keymap status-menu-default-keymap))
      (setq status-menu-last-command 'status-menu-init)
      (setq status-menu-selection (copy-list initial-selection)) ;; because we muck with it
      (setq status-menu-menu menu)
      (setq status-menu-horiz-offset 0)
      (status-menu-redisplay initial-center)
      (status-menu-callback 'init)
      (status-menu-callback 'selection-changed)
      (status-menu-cmdloop))))

(defun status-menu-test (&optional arg)
  "Test the status menu"
  (interactive)
  (status-menu-init '(one two three) '(0)))

(defun status-menu-forward (&optional arg)
  (interactive)
  (setq status-menu-last-command 'status-menu-forward)
  ;; (message "status-menu-forward selection=%s" status-menu-selection)
  (if (= (car (last status-menu-selection)) (1- (length status-menu-current-level)))
      (progn 
        (if status-menu-allow-wrap
            (setf (car (last status-menu-selection)) 0)
          (beep)))
    (incf (car (last status-menu-selection))))
  (status-menu-redisplay)
  (status-menu-callback 'selection-changed))

(defun status-menu-remove (&optional arg)
  "Remove the currently selected item from the menu"
  (interactive)
  (setq status-menu-last-command 'status-menu-remove)
  (let ((item (status-menu-current-item)))
    (setq status-menu-current-level (remove item status-menu-current-level))
    (setq status-menu-menu (status-menu-remove-from-menu status-menu-menu item)))
  ;; (message "status-menu-remove selection=%s" status-menu-selection)
  (when (>= (car (last status-menu-selection)) (length status-menu-current-level))
    (setf (car (last status-menu-selection))
          (if status-menu-allow-wrap 0
            (1- (length status-menu-current-level)))))
  (status-menu-redisplay)
  (status-menu-callback 'selection-changed))

(defun status-menu-backward (&optional arg)
  (interactive)
  (setq status-menu-last-command 'status-menu-backward)
  
  (if (zerop (car (last status-menu-selection)))
      (if status-menu-allow-wrap
          (setf (car (last status-menu-selection)) 
                (1- (length status-menu-current-level)))
        (beep))
    (decf (car (last status-menu-selection))))
  (status-menu-redisplay)
  (status-menu-callback 'selection-changed))

;; (global-set-key [f5] 'status-menu-backward)
;; (global-set-key [f6] 'status-menu-forward)

(defun status-menu-enter ()
  (interactive)
  (setq status-menu-last-command 'status-menu-enter)
  ;; TODO if submenu then enter the submenu, otherwise
  ;; its select and dismiss
  (status-menu-callback 'enter))

(defun status-menu-upward ())
(defun status-menu-dismiss ()
  (setq status-menu-last-command 'status-menu-dismiss)
  (setq status-menu-dismissed t)
  (setq status-menu-current-level nil)
  (setq status-menu-selection nil)
  (message "")
  (status-menu-callback 'dismiss))

(provide 'status-menu)

