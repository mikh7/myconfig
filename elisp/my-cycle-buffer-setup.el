;; 
;; My setup for cycle buffer
;;

;; make windows stick to their initial frame

(defvar my-buffer-last-frame nil
  "Last frame in which buffer was last displayed")

(defvar my-buffer-last-frame-terminal-name nil
  "Terminal name in which buffer was last displayed")

(make-variable-buffer-local 'my-buffer-last-frame)
(make-variable-buffer-local 'my-buffer-last-frame-terminal-name)


(defadvice bury-buffer (around cycle-buffer-hide activate)
  (let ((buffer (if buffer-or-name (get-buffer buffer-or-name)
                  (current-buffer))))
    (setq ad-return-value ad-do-it)
    (when buffer
      (with-current-buffer buffer
        (unless (string-match "^\\*Ediff Control Panel" (buffer-name)) 
          (setq my-buffer-last-frame 'buried))))))

(defun my-track-buffer-frame (&optional buffer)
  "Record buffer's current frame"
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let* ((window (get-buffer-window buffer nil))
           (frame (when window (window-frame window))))
      (when frame
        (when (and (not (eq my-buffer-last-frame frame)))
          ;; (message "Changing track in buffer %s to frame %s (old %s)"
          ;;          buffer frame my-buffer-last-frame)
          (setq my-buffer-last-frame frame))
        (setq my-buffer-last-frame-terminal-name 
              (if (featurep 'multi-tty)
                  (terminal-name (frame-terminal frame))
                "default"))))))

(defun my-track-buffer-frames ()
  "For every buffer in the buffer list, record on which frame
and terminal this buffer currently lives"
  (unless cycle-buffer-cycling
    (dolist (buffer (frame-buffer-list))
      (my-track-buffer-frame buffer))))

(add-hook 'window-configuration-change-hook 
          'my-track-buffer-frames)

(add-hook 'cycle-buffer-switch-hook 'my-track-buffer-frame)

(defun my-reconnect-buffer-frame (&optional frame)
  (or frame (setq frame (selected-frame)))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and my-buffer-last-frame 
                 (not (frame-live-p my-buffer-last-frame))
                 (string-match "pts" (terminal-name (frame-terminal frame)))
                 (equal my-buffer-last-frame-terminal-name
                        (terminal-name (frame-terminal frame))))
        ;; We created terminal on the same tty, reconnect it
        (when (not (eq my-buffer-last-frame frame))
          ;; (message "Reconnected buffer %s to frame %s (old %s)"
          ;;          buffer frame my-buffer-last-frame)
          (setq my-buffer-last-frame frame))))))

(add-hook 'after-make-frame-functions 'my-reconnect-buffer-frame)
(my-reconnect-buffer-frame)

(defun buffer-frameless-p (buffer)
  (with-current-buffer buffer
    (not
     (or 
      (string-match "^ " (buffer-name))
      (and (string-match "^\\*" (buffer-name)) 
           (not (string-match "^\\*Shell Command Output\\*" (buffer-name))))
      (eq major-mode 'org-mode)
      (get-buffer-window buffer t)
      (not (and my-buffer-last-frame
                (not (frame-live-p my-buffer-last-frame))))))))

(defun find-frameless-buffers ()
  "Find buffers were either never displayed on any frame
or were displayed on a frame that is not live"
  (remove-if-not 'buffer-frameless-p (buffer-list)))

(defun kill-frameless-buffers (&optional arg)
  "Kill buffers without a frame"
  (interactive)
  (dolist (buffer (find-frameless-buffers))
    (kill-buffer buffer)))

(defun bury-frameless-buffers (&optional arg)
  "Bury buffers without a frame"
  (interactive)
  (dolist (buffer (find-frameless-buffers))
    (bury-buffer buffer)))

(defvar cycle-buffer-force-frame nil
  "Force this buffer to show up in cycling on this frame")

(make-variable-buffer-local 'cycle-buffer-force-frame)
(setq cycle-buffer-force-frame nil)

(defun cycle-buffer-toggle-visibility (&optional arg)
  "Toggle the cycle buffer visibility of on this frame."
  (interactive)
  (if (and (boundp 'cycle-buffer-force-frame)
           (frame-live-p cycle-buffer-force-frame))
      (progn 
        (setq cycle-buffer-force-frame nil)
        (message "Buffer no longer forced to frame"))
    (setq cycle-buffer-force-frame (selected-frame))
    (message "Buffer forced to frame")))

(defun cycle-buffer-is-forced ()
  "Return if current buffer has a forced frame, and whenever that
frame is current, returns one of 'yes 'no or nil if its not
forced"
  (when (and (boundp 'cycle-buffer-force-frame)
             cycle-buffer-force-frame
             (frame-live-p cycle-buffer-force-frame))
    (if (eq (selected-frame) cycle-buffer-force-frame) 'yes 'no)))

(define-key viper-vi-basic-map "g\C-l" 'cycle-buffer-toggle-visibility)

;; this is the filter that should only leave the "permissive" stuff
(defun cycle-buffer-ok? (name)
  (unless (eq (cycle-buffer-is-forced) 'no) 
    (or
     ;; scratch buffer, unless its already visible in a 2nd window
     ;; on the same frame
     (and
      (string= name "*scratch*")
      (not (get-buffer-window (current-buffer) nil)))
     ;; buffer that was either
     (and
      (or
       (not my-buffer-last-frame)       ; never displayed_
       (eq cycle-buffer-orig-frame my-buffer-last-frame) ; last displayed here
       ;; frameless but not buried
       (and (not (eq my-buffer-last-frame 'buried))
            (not (frame-live-p my-buffer-last-frame))
            (null (get-buffer-window (current-buffer) t))))
      (not (eq (aref name 0) ?\ )) ; name does not start with space " buffer"
      (not (member name            ; uninteresting buffers
                   '("lispdir.dat" "*reportmail*" ".newsrc-dribble" "info dir"
                     ".infonotes")))
      (not (string-match "^\\(TAGS\\|\\*?sent\\)" name))
      (or (eq cycle-buffer-allow-visible t) ; visible buffers
          (eq (current-buffer) cycle-buffer-current)
          (not (get-buffer-window 
                (current-buffer)
                (if cycle-buffer-allow-visible nil 'visible))))))))

;; this is filter on top of permissive, for the minimal list of buffers
(defun cycle-buffer-ok-extra? (name)
  (or 
   ;; toggled binding
   (eq (cycle-buffer-is-forced) 'yes)
   ;; allow some specific asterisk buffers
   (string-match "^\\*Ediff Control Panel" name)
   (string-match "^\\*Members\\*" name)
   (string-match "^\\*Tree\\*" name)
   (string-match "^\\*Custom" name)
   (and (string-match "^\\*shell" name)
        (not (string-match "^\\*Shell Command Output\\*" (buffer-name))))
   (string-match "^\\*eshell" name)
   (string-match "^\\*gud" name)
   (string-match "^\\*Eve" name)
   (string-match "^\\*SQL" name)
   ;; (string-match "^\\*help" name)
   (string-match "^\\*info" name)
   ;; (string-match "^\\*scratch" name)
   (string-match "^\\*slime-scratch*" name)
   (string-match "^\\*Org Agenda" name)
   (string-match "^\\*Org Src" name)
   (string-match "^\\*Agenda" name)
   (string-match "^\\*Apropo" name)
   (string-match "^\\*Info" name)
   (string-match "^\\*sldb" name)
   (string-match "^egg.*buffer-mode$" (symbol-name major-mode))
   ;; (string-match "^\\*vc-dir" name)
   ;; (string-match "^\\*vc-diff" name)
   ;; (string-match "^\\*org-quicky" name)
   ;; (string-match "^\\*Man" name)
   (string-match "^\\*Python" name)
   (string-match "^CAPTURE.*org$" name)
   ;; (string-match "^\\*slime-repl" name)
   ;; disallow all other asterisk buffers
   (and (not (eq (aref name 0) ?*)) 
        ;; TODO contrib files are fullpath, need to fix
        (or (member name org-agenda-contributing-files) 
            (not (string-match "\\.\\(org\\|orgg\\|org_archive\\)$" name))))))

(setq cycle-buffer-filter '((cycle-buffer-ok? (buffer-name))))
(setq cycle-buffer-filter-extra '((cycle-buffer-ok-extra? (buffer-name))))

(global-set-key [(f9)]        'cycle-buffer-backward)
(global-set-key [(f10)]       'cycle-buffer)
(global-set-key [(S-f9)] 'cycle-buffer-backward-permissive)
(global-set-key [(S-f10)] 'cycle-buffer-permissive)

(defun filter-by-buffer-name (buffers regexp)
  (remove-if-not (lambda (buffer)
                   (string-match (buffer-name buffer)
                                 regexp))
                 buffers))

(defconst my-cycle-eshell-filter `((string-match "^\\*eshell\\(\\*\\|<.+>\\)" (buffer-name))))
(defconst my-cycle-shell-filter `((string-match "^\\*shell\\(\\*\\|<.+>\\)" (buffer-name))))
(defconst my-cycle-xshell-filter-extra `((or
                                          (not my-buffer-last-frame)
                                          (eq cycle-buffer-orig-frame my-buffer-last-frame))))

(defun cycle-eshell (&optional arg)
  "Cycle buffer to an eshell"
  (interactive "P")
  (let ((cycle-buffer-filter my-cycle-eshell-filter)
        (cycle-buffer-filter-extra my-cycle-xshell-filter-extra))
    (condition-case msg 
        (cycle-buffer arg)
      (error 
       (cond ((string-match "^There is no appropriate" (second msg))
              (let ((n 1)
                    (done nil))
                (while (not done)
                  (let ((eshell-buffer-name (format "*eshell<%d>" n)))
                    (cond ((not (get-buffer eshell-buffer-name))
                           (setq done t)
                           (eshell))
                          (t (incf n)))))))
             (t (error "%s" msg)))))))

(defun cycle-eshell-backwards (&optional arg)
  "Cycle buffer to an eshell"
  (interactive "P")
  (cycle-eshell (- arg)))

(defun cycle-shell (&optional arg)
  "Cycle buffer to an eshell"
  (interactive "P")
  (let ((cycle-buffer-filter my-cycle-shell-filter)
        (cycle-buffer-filter-extra my-cycle-xshell-filter-extra))
    (condition-case msg 
        (cycle-buffer arg)
      (error 
       (cond ((string-match "^There is no appropriate" (second msg))
              (let ((n 1)
                    (done nil))
                (while (not done)
                  (let ((shell-buffer-name (format "*shell<%d>" n)))
                    (cond ((not (get-buffer shell-buffer-name))
                           (setq done t)
                           (let ((buffer (get-buffer-create shell-buffer-name)))
                             (shell buffer)))
                          (t (incf n)))))))
             (t (error "%s" msg)))))))

(defun cycle-eshell-backwards (&optional arg)
  "Cycle buffer to an eshell"
  (interactive "P")
  (cycle-eshell (- arg)))


;; New frames are created by emacsclient displaying most recent buffer
;; by default, only after the frame is created it switches to the
;; *scratch* buffer. That causes our code to record the last buffer as
;; belonging to newly created frame.
;;
;; Fix this by switching to *scratch* buffer before frame creation

(defadvice server-create-tty-frame (around create-frames-with-scratch-buffer activate)
  (with-current-buffer (get-buffer-create "*scratch*")
    (setq ad-return-value ad-do-it)))

(defadvice server-create-window-system-frame (around create-frames-with-scratch-buffer activate)
  (with-current-buffer (get-buffer-create "*scratch*")
    (setq ad-return-value ad-do-it)))

(global-set-key "\C-ce" 'cycle-eshell) (global-set-key "\C-cs"
'cycle-shell)

;; nice!
(global-set-key "\C-l" 'cycle-buffer)
(global-set-key "\C-h" 'cycle-buffer-backward)
(define-key cycle-buffer-keymap "\C-l" 'status-menu-forward)
(define-key cycle-buffer-keymap "\C-h" 'status-menu-backward)
(global-set-key [(f9)] nil)
(global-set-key [(f10)] nil)
(global-set-key [(S-f9)] nil)
(global-set-key [(S-f10)] nil)
(define-key cycle-buffer-keymap [(f9)] nil)
(define-key cycle-buffer-keymap [(f10)] nil)
(setq cycle-buffer-esc-key nil)

(provide 'my-cycle-buffer-setup)
