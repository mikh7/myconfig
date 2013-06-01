(eval-when-compile (require 'cl))
(require 'status-menu)

(defvar cycle-buffer-follow)
(defvar cycle-buffer-timeout)
(defvar cycle-buffer-timeout-action)
(defvar cycle-buffer-enter-key)
(defvar cycle-buffer-esc-key)
(defvar cycle-buffer-other-command)
(defvar cycle-buffer-start-with-current)
(defvar cycle-buffer-wrap)

(defvar cycle-buffer-keymap (make-sparse-keymap)
  "Keymap in effect when cycling buffers")

(define-key cycle-buffer-keymap [f10] 'status-menu-forward)
(define-key cycle-buffer-keymap [f9] 'status-menu-backward)

(defun cycle-buffer-kill ()
  "Kill the currently selected buffer"
  (interactive)
  (let ((buf (status-menu-current-item)))
    (kill-buffer buf)
    (when (not (buffer-live-p buf))
      (setq status-menu-timeout nil)
      (status-menu-remove))))

(defun cycle-buffer-bury ()
  "Kill the currently selected buffer"
  (interactive)
  (let ((buf (status-menu-current-item)))
    (bury-buffer buf)
    (setq status-menu-timeout nil)
    (status-menu-remove)))

(define-key cycle-buffer-keymap (kbd "\C-k") 'cycle-buffer-kill)
(define-key cycle-buffer-keymap (kbd "d") 'cycle-buffer-kill)
(define-key cycle-buffer-keymap (kbd "b") 'cycle-buffer-bury)

(defgroup cycle-buffer nil
  "Hilighting of replace region, search pattern, minibuffer, etc."
  :prefix "cycle-buffer-"
  :group 'cycle-buffer)

(defcustom cycle-buffer-timeout 5
   "When not nil, the status bar menu times out after this many
seconds.  The effect of the timeout is determined by
`cycle-buffer-timeout-action'"
   :initialize 'custom-initialize-reset
   :type '(radio (const :tag "None" nil)
                 (number :tag "Timeout in seconds"
                         ;; need value to have default when radio
                         ;; button is switched
                         :value 2.5))
   :group 'cycle-buffer) 

(defcustom cycle-buffer-timeout-action 'cancel
  "Indicates an action to take when status bar menu had timed
out (see `cycle-buffer-timeout').

`switch' Switches to the currently selected buffer in the menu,
puts it on top of the buffer list and dismissies the menu.

`cancel' Switches to the original buffer and cancels the menu without changing
the ordering of the buffer list.
"
  :initialize 'custom-initialize-reset
  :type '(radio (const :tag "Switch to selected buffer" switch)
                (const :tag "Cancel" cancel))
  :group 'cycle-buffer)

(defcustom cycle-buffer-follow nil
  "When not nil, current buffer will switch to follow the
selection in the status bar menu, this will be done without
affecting the original ordering the buffer list
"
  :initialize 'custom-initialize-reset
  :type 'boolean
  :group 'cycle-buffer)

(defcustom cycle-buffer-enter-key 'switch
  "Controls the Enter key behavour in the status menu:

`nil' Does not provide any special procesing for the Enter
key. It will be processed as any other key based on the value of
`cycle-buffer-other-commands'.

`switch' Switches to the currently selected buffer in the menu,
puts it on top of the buffer list and dismissies the menu.

`cancel' Switches to the original buffer and cancels the menu
without changing the ordering of the buffer list.
"
  :initialize 'custom-initialize-reset
  :type '(radio (const :tag "No special processing" nil)
                (const :tag "Switch to selected buffer" switch)
                (const :tag "Cancel" cancel))
  :group 'cycle-buffer)

(defcustom cycle-buffer-esc-key 'cancel
  "Controls the Esc key behavour in the status menu:

`nil' Does not provide any special procesing for the Esc key. It
will be processed as any other key based on the value of
`cycle-buffer-other-commands'.

`switch' Switches to the currently selected buffer in the menu,
puts it on top of the buffer list and dismissies the menu.

`cancel' Switches to the original buffer and cancels the menu
without changing the ordering of the buffer list.
"
  :initialize 'custom-initialize-reset
  :type '(radio (const :tag "No special processing" nil)
                (const :tag "Switch to selected buffer" switch)
                (const :tag "Cancel" cancel))
  :group 'cycle-buffer)

(defcustom cycle-buffer-other-keys 'switch
  "Controls the Esc key behavour in the status menu:

`switch' Executes the command bound to the key and then switches
to the selected buffer in the menu, dismissing the menu.

`cancel' Executes the command bound to the key and switches to
the original buffer and cancels the menu without changing the
ordering of the buffer list.

`ignore-and-beep' Ignores the key and beeps.

`ignore' Ignores the key silently
"
  :initialize 'custom-initialize-reset
  :type '(radio 
          (const :tag "Switch to selected buffer and ignore the key" switch)
          (const :tag "Cancel" cancel)
          (const :tag "Ignore and Beep" ignore-and-beep)
          (const :tag "ignore silently" ignore))
  :group 'cycle-buffer)

(defcustom cycle-buffer-start-with-current t
  "When not nil, status menu will start with the current buffer
being selected, so that one needs to issue <cycle-buffer>
<status-menu-forward> commands to get to the 2nd most recent
buffer.

When nil then status menu will start with the most recently used buffer
other then the current one"
  :initialize 'custom-initialize-reset
  :type 'boolean
  :group 'cycle-buffer)

(defcustom cycle-buffer-wrap t
  "When not nil, status menu will wrap around once end or start
of the menu is reached"
  :initialize 'custom-initialize-reset
  :type 'boolean
  :group 'cycle-buffer)

(defvar cycle-buffer-orig-buffer nil
  "Buffer to switch to in case cycling in cancelled")

(defvar cycle-buffer-filter
  '((not (eq (aref (buffer-name) 0) ?\ )) ; " buffer"
    (not (member (buffer-name)          ; uninteresting buffers
                 '("lispdir.dat" "*reportmail*" ".newsrc-dribble" "info dir"
                   ".infonotes")))
    (not (string-match "^\\(TAGS\\|\\*?sent\\)" (buffer-name)))
    (or (eq cycle-buffer-allow-visible t) ; visible buffers
        (eq (current-buffer) cycle-buffer-current)
        (not (get-buffer-window (current-buffer) 
                                (if cycle-buffer-allow-visible nil 'visible)))))
  "*A list of forms that determine if a buffer is considered for switching to.
All forms should return non-nil for a buffer to be eligible. The forms are
evaluated in the buffer in question, so they can check its buffer-local
variables (eg major-mode).

You can add more restrictions by consing to the variable from inside
cycle-buffer-load-hook or after (require 'cycle-buffer). For example to
restrict switching from a C++ file only to relevant files, do this:
  (setq cycle-buffer-filter (cons 
     '(or (not (eq major-mode 'c++-mode))
          (string-match \"\\\\.\\\\(cc\\\\|hh\\\\)$\\\\|I?[mM]akefile\" (buffer-name)))
     cycle-buffer-filter))
Unfortunately, in order to *relax* the restrictions, you will probably need to
copy the variable to your .emacs and make changes in the text.")

(defvar cycle-buffer-filter-extra
  '((not (string-match "^\\*.*\\*\\(<[0-9]+>\\)?$" (buffer-name)))
    cycle-buffer-interesting)
  "*List of forms that are evaluated in addition to cycle-buffer-filter for
the non-permissive versions of the cycle-buffer commands.")

(defvar cycle-buffer-allow-visible 'other
  "*Whether to consider visible buffers. nil: ignore them; t: allow them; any
other value: allow buffers visible on other frames, but not on the selected
frame.") 

(defvar cycle-buffer-load-hook nil
  "Hook that is run right after cycle-buffer is loaded.")

;; end of user variables

(defconst cycle-buffer-commands
  '(cycle-buffer cycle-buffer-backward
                 cycle-buffer-permissive cycle-buffer-backward-permissive
		 viper-intercept-ESC-key
                 status-menu-forward
                 status-menu-backward
                 status-menu-dismiss)
  "List of all cycle-buffer commands.")

(defvar cycle-buffer-list nil
  "Buffer list as set by the last cycle-buffer command.")

(defvar cycle-buffer-current nil
  "The value of (current-buffer) before the command was invoked.")

(defvar cycle-buffer-interesting t
  "Whether the current buffer should be considered. Use
cycle-buffer-toggle-interesting to set it interactively.")

(make-variable-buffer-local 'cycle-buffer-interesting)

(defun cycle-buffer-switch-to-buffer (buffer &optional norecord)
  "Like switch-to-buffer but checks if buffer is live"
  (when (buffer-live-p buffer)
    (switch-to-buffer buffer norecord)))

(defvar cycle-buffer-did-switch nil
  "True if last cycle buffer command switched to a new buffer")

(defvar cycle-buffer-switch-hook nil
  "Called after `cycle-buffer' is finished and had switched to the new buffer.")

(defun cycle-buffer-switch-to-selected-buffer ()
  "Switch to selected buffer in the menu (if its live)
or to original buffer if its not."
  (cycle-buffer-switch-to-buffer 
   (if (buffer-live-p cycle-buffer-selected-buffer)  
       cycle-buffer-selected-buffer cycle-buffer-orig-buffer))
  (setq cycle-buffer-did-switch t)
  (with-current-buffer cycle-buffer-selected-buffer
    (run-hooks 'cycle-buffer-switch-hook)))

(defvar cycle-buffer-selected-buffer nil)

(defun cycle-buffer-status-menu-hook (event)
  (case event
    ('selection-changed 
     (setq cycle-buffer-selected-buffer
           (elt status-menu-current-level (car status-menu-selection)))
     (when cycle-buffer-follow
       (cycle-buffer-switch-to-buffer cycle-buffer-selected-buffer t)))
    ('quit 
     (cycle-buffer-switch-to-buffer cycle-buffer-orig-buffer t)
           (status-menu-dismiss)
           (setq cycle-buffer-list nil))
    ('esc  (cond ((eq cycle-buffer-esc-key 'switch)
                  (cycle-buffer-switch-to-selected-buffer)
                  (status-menu-dismiss)
                  (setq cycle-buffer-list nil))
                 ((eq cycle-buffer-esc-key 'cancel)
                  (cycle-buffer-switch-to-buffer cycle-buffer-orig-buffer t)
                  (status-menu-dismiss)
                  (setq cycle-buffer-list nil))
                 (t (error "Invalid value of `cycle-buffer-esc-key' : %s" cycle-buffer-esc-key))))
    ('other-command
     (cond ((eq cycle-buffer-other-keys 'switch)
            ;; need a better solution, basically if 'other-command
            ;; was something like find-file, or anything else that switches
            ;; the current-buffer, then we should not switch to the selected
            ;; buffer here. 
            ;;
            ;; Otherwise user experiences a weirdness, where they cycled to some buffer
            ;; did C-x C-f (before the cycle timeout expired) and then it opens 
            ;; the file fine, but does not (from the user point of view) switch to it
            ;;
            ;; Its because we are called immediately after, and switch buffer to the 
            ;; selected one in the below line
            ;;
            ;; For now, simply donot switch if follow mode is on, this way we know
            ;; selected buffer was already current because of selection change,
            ;; so no need to switch to it again
            (if cycle-buffer-follow
                (setq cycle-buffer-did-switch t)
              (cycle-buffer-switch-to-selected-buffer))
            (status-menu-dismiss)
            (setq cycle-buffer-list nil))
           ((eq cycle-buffer-other-keys 'cancel)
            (cycle-buffer-switch-to-buffer cycle-buffer-orig-buffer t)
            (status-menu-dismiss)
            (setq cycle-buffer-list nil))
           ;; we won't get called if its ignore or ignore-and-beep
           (t (error "Invalid value of `cycle-buffer-other-keys' : %s" cycle-buffer-other-keys))))
    ('enter
     (cond ((eq cycle-buffer-enter-key 'switch)
            (cycle-buffer-switch-to-selected-buffer)
            (status-menu-dismiss)
            (setq cycle-buffer-list nil))
           ((eq cycle-buffer-enter-key 'cancel)
            (cycle-buffer-switch-to-buffer cycle-buffer-orig-buffer t)
            (status-menu-dismiss)
            (setq cycle-buffer-list nil))
           (t (error "Invalid value of `cycle-buffer-enter-key' : %s" cycle-buffer-enter-key))))
    ('timeout 
     (cond ((eq cycle-buffer-timeout-action 'cancel)
            (cycle-buffer-switch-to-buffer cycle-buffer-orig-buffer t)
            (status-menu-dismiss)
            (setq cycle-buffer-list nil))
           ((eq cycle-buffer-timeout-action 'switch)
            (cycle-buffer-switch-to-selected-buffer)
            (status-menu-dismiss)
            (setq cycle-buffer-list nil))))))

(defvar cycle-buffer-cycling nil
  "Non NIL when cycle buffer is cycling. Possible values:
NORMAL: for normal cycling
OTHER-WINDOW: for cycling in other window
OTHER-FRAME: for cycling in other frame")

(defvar cycle-buffer-frame nil
  "The frame which we are cycling in. If `cycle-buffer-cycling' is OTHER-FRAME
then this will be that frame")

(defvar cycle-buffer-orig-frame nil
  "The frame in which cycling originated. If `cycle-buffer-cycling' is OTHER-FRAME
then this will be original frame")

;;;###autoload
(defun cycle-buffer (&optional arg backward permissive)
  "Switch to the next buffer on the buffer list without prompting.
Successive invocations select buffers further down on the buffer list.
A prefix argument specifies the DISTANCE to skip, negative moves back."
  (interactive "P")
  (let (in-other-window
        in-other-frame
        (narg (prefix-numeric-value arg)))
    (cond ((and arg (= narg 4))
           (setq in-other-window t))
          ((and arg (= narg 5))
           (setq in-other-frame t))
          (arg (setq permissive t)))
    (cond (in-other-window
           (let ((created-other-window nil)
                 (other-window 
                  (cycle-buffer-find-other-window))
                 (cycle-buffer-did-switch))
             ;; if other window not found, make one and 
             ;; record the fact that we did, so we can delete it
             ;; if user cancelled or quit
             (unless other-window
               (setq other-window
                     (display-buffer (current-buffer) t)
                     created-other-window t))
             (with-selected-window other-window
               (let ((cycle-buffer-cycling 'other-window)
                     (cycle-buffer-frame (selected-frame))
                     (cycle-buffer-orig-frame (selected-frame)))
                 (cycle-buffer-internal backward permissive)))
             (if cycle-buffer-did-switch
                 (select-window other-window)
               (when created-other-window
                 (delete-window other-window)))))
          (in-other-frame
           (let ((created-other-frame nil)
                 (other-frame 
                  (cycle-buffer-find-other-frame))
                 (cycle-buffer-did-switch)
                 (cycle-buffer-orig-frame (selected-frame)))
             ;; if other frame not found, make one and 
             ;; record the fact that we did, so we can delete it
             ;; if user cancelled or quit
             (unless other-frame
               (setq other-frame (make-frame)
                     created-other-frame t))
             (raise-frame other-frame)
             (let ((cycle-buffer-cycling 'other-frame)
                   (cycle-buffer-frame other-frame))
               (with-selected-frame other-frame
                 (cycle-buffer-internal backward permissive))
               (if cycle-buffer-did-switch
                   (select-frame other-frame)
                 (if created-other-frame
                     (delete-frame other-frame)
                   (raise-frame cycle-buffer-orig-frame))))))
          (t
           (let ((cycle-buffer-cycling 'normal)
                 (cycle-buffer-frame (selected-frame))
                 (cycle-buffer-orig-frame (selected-frame)))
             (cycle-buffer-internal backward permissive))))))

;;;###autoload
(defun cycle-buffer-backward (&optional arg)
  "Switch to the next buffer on the buffer list without prompting.
Successive invocations select buffers further down on the buffer list.
A prefix argument specifies the DISTANCE to skip, negative moves back."
  (interactive "P")
  (cycle-buffer arg t))

(defun cycle-buffer-permissive (&optional arg)
  "Switch to the next buffer, use PERMISSIVE filter"
  (interactive "P")
  (cycle-buffer arg nil t))

(defun cycle-buffer-backward-permissive (&optional arg)
  "Switch to the next buffer, use PERMISSIVE filter"
  (interactive "P")
  (cycle-buffer arg t t))

(defun cycle-buffer-find-other-window ()
 (let ((current (selected-window))
        (other))
    (walk-windows (lambda (window)
                    (if (not (eq window current))
                        (setq other window))))
    other))

(defun cycle-buffer-find-other-frame ()
 (let ((current (selected-window))
       (other))
   (dolist (frame (frame-list))
     (when (and (not (eq frame (selected-frame)))
                (eq (frame-terminal frame)
                    (frame-terminal (selected-frame)))
                (not (let (have-dedicated-p)
                       (dolist (win (window-list frame) have-dedicated-p)
                         (when (window-dedicated-p win)
                           (setq have-dedicated-p t)))
                       have-dedicated-p))
                (window--frame-usable-p frame))
       (setq other frame)))
   other))

(defun cycle-buffer-internal (backward permissive)
  "Switch to the next buffer on the buffer list without prompting.
Successive invocations select buffers further down on the buffer list.
A prefix argument specifies the DISTANCE to skip, negative moves back."
  (let ((list nil)
        (start-index 0))
    ;; Put current buffer on top of Emacs' buffer list
    (cycle-buffer-switch-to-buffer (current-buffer))
    (setq cycle-buffer-orig-buffer (current-buffer))
    ;; so that whoever called us can find out if user action
    ;; had resulted in a switch, or in cancel/quit
    (setq cycle-buffer-did-switch nil)
    ;; regenerate the buffer list
    (setq cycle-buffer-list (cycle-buffer-filter
                             (buffer-list (selected-frame))
                             cycle-buffer-filter))
    (setq list
          (if permissive cycle-buffer-list
            (cycle-buffer-filter cycle-buffer-list cycle-buffer-filter-extra)))
    (if (null list)
        (error "There is no appropriate buffer to switch to."))
    (setq list (remove (current-buffer) list))
    (let* ((len (length list))
           (roll-len 
            (if (> len 2)
                (+ (if (oddp len) 2 1) (/ len 2))
              len))
           (rest-len (- len roll-len)))
      (setq list (append
                  (subseq list roll-len)
                  (list (current-buffer))
                  (subseq list 0 roll-len)))
      (incf start-index rest-len)
      (unless cycle-buffer-start-with-current
        (cond ((and backward (plusp start-index))
               (decf start-index))
              ((and (not backward)
                    (< start-index (1- (length list))))
               (incf start-index)))))
    (setq cycle-buffer-list list)
    (status-menu-init cycle-buffer-list 
                      (list start-index)
                      :initial-center t
                      :callback 'cycle-buffer-status-menu-hook
                      :timeout  cycle-buffer-timeout
                      :allow-enter-key cycle-buffer-enter-key
                      :allow-wrap cycle-buffer-wrap
                      :allow-esc-key cycle-buffer-esc-key
                      :keymap cycle-buffer-keymap
                      :allow-other-commands (not (member cycle-buffer-other-keys
                                                         '(ignore-and-beep ignore)))
                      :ignore-keys-silently (eq cycle-buffer-other-keys 'ignore))))

(defun cycle-buffer-filter (list filter)
  ;; Filter LIST through the variable cycle-buffer-filter
  (let (result)
    (setq cycle-buffer-current (current-buffer))
    (while list
      (set-buffer (car list))
      (if (eval (cons 'and filter))
          (setq result (cons (car list) result)))
      (setq list (cdr list)))
    (set-buffer cycle-buffer-current)
    (nreverse result)))

;; TODO make this work
;;;###autoload
(defun cycle-buffer-toggle-interesting (&optional arg)
  "Toggle the value of cycle-buffer-interesting for the current buffer.
With positive arg set it, with non-positive arg reset it. A buffer is only
considered by cycle-buffer when cycle-buffer-interesting is t."
  (interactive "P")
  (setq cycle-buffer-interesting (if arg (> (prefix-numeric-value arg) 0)
                                   (not cycle-buffer-interesting)))
  (message "This buffer will%s be considered by cycle-buffer."
           (if cycle-buffer-interesting "" " not")))

;; redifine this

(defun remove-shift (key)
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

(defun key-binding-ignoring-modifiers (key)
  "Return key binding for the key ignoring the shift key"
  (let (binding unshifted)
    (setq binding (key-binding key))
    (when (and (null binding)
	       (setq unshifted (remove-shift key)))
      (setq binding (key-binding unshifted)))
    binding))
     

;; (defun viper-intercept-ESC-key ()
;;   "Function that implements ESC key in Viper emulation of Vi."
;;   (interactive)
;;   (let* ((cmd (or (key-binding-ignoring-modifiers (viper-envelop-ESC-key))
;; 		  'viper-intercept-ESC-key)))

;;     ;; fix macros
;;     (when (keymapp cmd)
;;       (setq cmd 'viper-intercept-ESC-key))

;;     ;; call the actual function to execute ESC (if no other symbols followed)
;;     ;; or the key bound to the ESC sequence (if the sequence was issued
;;     ;; with very short delay between characters).
;;     (if (eq cmd 'viper-intercept-ESC-key)
;; 	(setq cmd
;; 	      (cond ((evil-normal-state-p)
;; 		     'viper-ESC)
;; 		    ((evil-insert-state-p)
;; 		     'evil-normal-state)
;; 		    ((evil-replace-state-p)
;; 		     'evil-normal-state)
;; 		    (t 'evil-normal-state))))
;;     (setq this-command cmd)
;;     (run-hooks 'pre-command-hook)
;;     (call-interactively cmd)))


(run-hooks 'cycle-buffer-load-hook)
(provide 'cycle-buffer)

;;; end of cycle-buffer.el

