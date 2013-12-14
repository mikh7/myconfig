
(require 'vc)
(require 'vc-dir)
(require 'log-edit)
(require 'log-view)
(require 'diff-mode)
(require 'vc-hg)
(require 'vc-git)
(require 'egg)

(defvar vc-diff-switches-history nil
  "History for vc-diff command switches")

(defadvice vc-diff (around maybe-ignore-whitespace activate)
  "With prefix argument 1 ignore whitespace, with prefix argument 2 ask
for the diff switches"
  (let ((arg current-prefix-arg))
    (cond ((null arg)
           (setq ad-return-value ad-do-it))
          ((member arg '(1 2))
           (setq current-prefix-arg nil)
           (let ((vc-diff-switches "-w")
                 (vc-git-diff-switches "-w")
                 (vc-hg-diff-switches "-w")
                 (current-prefix-arg nil))
             (setq ad-return-value (vc-diff (equal arg 2) t))))
          ((member arg '(3 4 (16)))
           (setq current-prefix-arg nil)
           (let* ((switches
                   (read-from-minibuffer "Diff switches: " nil nil nil 
                                         'vc-diff-switches-history)))
             (if switches
                 (let ((vc-diff-switches switches)
                       (vc-git-diff-switches switches)
                       (vc-hg-diff-switches switches))
                   (setq ad-return-value (vc-diff (equal arg 4) t)))
               (setq ad-return-value (vc-diff (equal arg 4) t)))))
          (t (setq ad-return-value ad-do-it)))))

(define-key evil-normal-state-map ";v" 'vc-prefix-map)

(dolist (mode '(log-edit-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (add-to-list 'evil-insert-state-modes mode))

(dolist (mode '(vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode
                            diff-mode))
  (remove-from-list 'evil-emacs-state-modes mode)
  (add-to-list 'evil-normal-state-modes mode))


(define-key log-view-mode-map "\C-n" 'log-view-msg-next)
(define-key log-view-mode-map "\C-p" 'log-view-msg-prev)
(define-key log-view-mode-map (kbd "C-M-n") 'log-view-file-next)
(define-key log-view-mode-map (kbd "C-M-p") 'log-view-file-prev)

(define-key diff-mode-shared-map "\C-n" 'diff-hunk-next)
(define-key diff-mode-shared-map "\C-p" 'diff-hunk-prev)
(define-key diff-mode-shared-map (kbd "C-M-n") 'diff-file-next)
(define-key diff-mode-shared-map (kbd "C-M-p") 'diff-file-prev)

(evil-give-back-keys-in-mode '(vc-dir-mode special-mode) evil-give-back-keys-updown-only)

(evil-give-back-keys-in-mode '(vc-git-log-view-mode log-view-mode))
(evil-give-back-keys-in-mode '(vc-hg-log-view-mode log-view-mode))
(evil-give-back-keys-in-mode '(diff-mode diff-mode-shared))

(evil-define-key 'normal diff-mode-map "z" nil)
(evil-define-key 'normal diff-mode-map "zk" 'diff-hunk-kill)


;; Egg mode setup
(evil-define-key 'normal egg-minor-mode-map ";v" egg-file-cmd-map)
(evil-define-key 'motion egg-minor-mode-map ";v" egg-file-cmd-map)
(define-key egg-hide-show-map (kbd "TAB") 'egg-section-cmd-toggle-hide-show)

(dolist (mode '(egg-commit-buffer-mode
                egg-diff-buffer-mode
                egg-status-buffer-mode
                egg-log-buffer-mode
                (egg-file-log-buffer-mode egg-log-buffer-mode)
                (egg-reflog-buffer-mode egg-log-buffer-mode)))
  (let ((modes mode)
        (mode (if (symbolp mode) mode (car mode))))
    (add-to-list 'evil-normal-state-modes mode)
    (remove-from-list 'evil-emacs-state-modes mode)
    (remove-from-list 'evil-insert-state-modes mode)
    (evil-give-back-keys-in-mode modes)
    ;; (evil-define-key 'normal-mode-map ";v" egg-file-cmd-map)
    ))

(dolist (mode '(egg-commit-buffer-mode vc-git-log-edit-mode))
  (remove-from-list 'evil-normal-state-modes mode)
  (add-to-list 'evil-insert-state-modes mode))

(defadvice vc-dir (around use-egg-instead-of-git activate)
  (let ((backend (vc-responsible-backend dir)))
    (if (eq backend 'Git)
        (let ((default-directory dir))
          (my-egg-status))
      (setq ad-return-value ad-do-it))))

(defun my-egg-log (&optional arg)
  "Call `egg-log' with T parameter so it switches to status buffer"
  (interactive)
  (call-interactively 'egg-log)
  ;; (egg-buffer-hide-section-type :diff)
  )

(defadvice vc-print-log-internal (around use-egg-instead-of-git activate)
  (if (eq backend 'Git) (my-egg-log)
    (setq ad-return-value ad-do-it)))

(defun my-egg-status (&optional arg)
  "Call `egg-status' with T parameter so it switches to status buffer"
  (interactive)
  (egg-status t t)
  ;; (egg-buffer-hide-section-type :diff)
  )

(define-key egg-file-cmd-map "d" 'my-egg-status)
(define-key egg-file-cmd-map "s" 'my-egg-status)

(defvar my-egg-stage/unstage-info nil
  "Info on invisibility of file and its hunks before stage or unstage.
Each member of this list is (FILE STAGED-FILE-P UNSTAGED-FILE-P
LINE-NUMBERS)

FILE is the file name

STAGED-FILE-P and UNSTAGED-FILE-P are T if file is invisible in
staged and unstaged sections :no if file is visible and NIL if file
is not present in the section

LINE-NUMBERS are line numbers of invisible hunks.

The reason we use line numbers and not hunk ids, is because under
git hunk headers change slightly as they are staged/unstaged")

(defvar my-egg-stage/unstage-point nil
  "The (TYPE LOCATION &optional HUNK-LINE-NUMBER) where to
restore point after buffer is refreshed.

TYPE is :hunk :diff or :section
LOCATION is section type or file name
HUNK-LINE-NUMBER is real file starting line number of the hunk")


(defvar my-egg-hunk-line-no-cache nil
  "A list of (FILENAME RANGE RANGE ...)) for each file in the
buffer. The RANGE is a list of (L1 S1 L2 S2) from the \"@@
-L1,S1 +L2,S2 @@\" from each hunk header")

(defun my-egg-hunk-line-no-cache ()
  "Returns `my-egg-hunk-line-no-cache' re-creating it if its nil. Must be called
with the current buffer being egg buffer to create cache for."
  (or my-egg-hunk-line-no-cache
      (save-excursion
        (let ((pos (point-min)) nav
              last-file
              list)
          (while (setq pos (next-single-property-change (1+ pos) :navigation))
            (let ((sect (get-text-property pos :section))
                  (type (get-text-property pos :sect-type))
                  (file (first (get-text-property pos :diff)))
                  (nav (get-text-property pos :navigation)))
              (when (and nav file sect)
                (when (and (eq type :hunk)
                           (eq sect 'unstaged))
                  (when (not (equal last-file file))
                    (push (setq list (cons file nil)) my-egg-hunk-line-no-cache)
                    (setq last-file file))
                  (setcdr list (cons (my-egg-hunk-ranges pos)
                                     (cdr list)))))))
          my-egg-hunk-line-no-cache))))

(defun my-egg-hunk-unstaged-before-line (file line)
  "Count how many lines any unstaged patches add before LINE line
number"
  (let ((cnt 0))
    (dolist (range (cdr (assoc file (my-egg-hunk-line-no-cache))))
      (destructuring-bind (l1 s1 l2 s2) range
        ;; We have to compare the old file line,
        ;;
        ;; Consider the situation when old file is 100 line file, it
        ;; has a change at line 10, that adds 500 lines, and another
        ;; change at line 50 that adds 5 lines, and another change at
        ;; line 80 that adds 5 lines
        ;;
        ;; If 1st two changes are unstaged, and 3rd change is staged,
        ;; the status buffer will look like so:
        ;;
        ;; Unstaged:
        ;;
        ;; @@ -10,0 +10,500 @@
        ;; @@ -50,0 +550,5 @@
        ;;
        ;; Staged:
        ;;
        ;; -80,1 +80,5
        ;;
        ;; But if we would like to go to the 3rd change in the file's buffer,
        ;; since file buffer contains both staged and un-staged changes,
        ;; the real line number 3rd hunk is 585, because hunks
        ;; 1 and 2 added 500 and 5 lines before it.
        (when (<= (+ l1 s1) line)
          ;; Increment adjustment by how many lines were added
          (incf cnt (- s2 s1)))))
    cnt))

(defun my-egg-hunk-ranges (pos)
  "Extract 3rd line number from hank"
  (destructuring-bind (file hunk-header hunk-beg &rest ignore)
      (egg-hunk-info-at pos)
    (let* ((ranges (save-match-data
                     (split-string hunk-header "[ @,\+,-]+" t))))
      (mapcar 'string-to-number ranges))))

(defun my-egg-hunk-real-line-no (pos unstaged-p)
  "Return real file number for hunk at POS. If the file is unstaged
then git diff already gives the real file number.

If the file is staged, then git gives the line number as if
the unstaged changes did not exist. But real file has unstaged changes
in it, regardless.

So to calculate real line number for the staged hunk, we need to
add any extra lines that the unstaged changes before that hunk would
have added."
  (destructuring-bind (file hunk-header hunk-beg &rest ignore)
      (egg-hunk-info-at pos)
    (let ((start-line (string-to-number
                       (nth 2 (save-match-data
                                (split-string hunk-header "[ @,\+,-]+" t))))))
      (if unstaged-p start-line
        (+ start-line (my-egg-hunk-unstaged-before-line
                       file
                       start-line))))))

(defun my-egg-save-stage/unstage-visibility ()
  "Save the invisibility status of each file, and each hunk in
the buffer into `my-egg-stage/unstage-info'. Hunks are indexed by
their real file line number.

Also the the first section after the point in `my-egg-stage/unstage-point"
  (setq my-egg-stage/unstage-info nil)
  (setq my-egg-stage/unstage-point nil)
  (setq my-egg-hunk-line-no-cache nil)
  (let ((pos (point-min)) nav
        (orig-type (get-text-property (point) :sect-type)))
    (while (setq pos (next-single-property-change (1+ pos) :navigation))
      (let* ((sect (get-text-property pos :section))
             (type (get-text-property pos :sect-type))
             (file (first (get-text-property pos :diff)))
             (nav (get-text-property pos :navigation))
             (line (when (eq type :hunk)
                     (my-egg-hunk-real-line-no
                      pos (eq sect 'unstaged)))))
        ;; remember a suitable place to return point generally its the
        ;; next section that is visible after the current point,
        ;; except if point is on the :diff section, then do not
        ;; consider :hunks, just files and sections.
        (when (> pos (line-end-position))
          (when (and (null my-egg-stage/unstage-point)
                     (not (invisible-p pos))
                     ;; from file, do not set next pos to be hunk,
                     ;; only files and sections
                     (or (not (eq orig-type :diff))
                         (member type '(:diff :section))))
            (cond ((eq type :section)
                   (setq my-egg-stage/unstage-point (list :section sect)))
                  ((eq type :diff)
                   (setq my-egg-stage/unstage-point (list :diff file)))
                  ((eq type :hunk)
                   (setq my-egg-stage/unstage-point (list :hunk file line))))))
        (when (and nav file sect)
          (let ((info
                 (or (assoc file my-egg-stage/unstage-info)
                     (first (push (list file nil nil nil)
                                  my-egg-stage/unstage-info)))))
            (cond ((and (eq sect 'staged)
                        (eq type :diff))
                   (setf (second info)
                         (if (assoc nav buffer-invisibility-spec) t
                           :no)))
                  ((and (eq sect 'unstaged)
                        (eq type :diff))
                   (setf (third info)
                         (if (assoc nav buffer-invisibility-spec) t
                           :no)))
                  ((and (eq type :hunk) line (assoc nav buffer-invisibility-spec))
                   (push line (fourth info))))))))))

(defun my-egg-restore-stage/unstage-visibility ()
  "Restore the visibility of files and hunks after stage/unstage
operation."
  (interactive)
  (setq my-egg-hunk-line-no-cache nil)
  (let ((restore-pt nil))
    (dolist (elem my-egg-stage/unstage-info)
      (destructuring-bind (file
                           staged-invisible-p
                           unstaged-invisible-p
                           hunk-line-numbers)
          elem
        (let ((pos (point-min)) nav)
          (while (setq pos (next-single-property-change (1+ pos) :navigation))
            (let* ((sect (get-text-property pos :section))
                   (type (get-text-property pos :sect-type))
                   (file (first (get-text-property pos :diff)))
                   (nav (get-text-property pos :navigation))
                   (line (when (eq type :hunk)
                           (my-egg-hunk-real-line-no
                            pos (eq sect 'unstaged)))))
              ;; see if this is section we should restore point 
              (when (and (null restore-pt)
                         my-egg-stage/unstage-point)
                (when (and (eq type :hunk)
                           (equal line 176))
                  (message "here file=%s line=%s" file line))
                (when (or
                       (and (eq :section (first my-egg-stage/unstage-point))
                            (eq type :section)
                            (eq sect (second my-egg-stage/unstage-point)))
                       (and (eq :diff (first my-egg-stage/unstage-point))
                            (eq type :diff)
                            (equal file (second my-egg-stage/unstage-point)))
                       (and (eq :hunk (first my-egg-stage/unstage-point))
                            (eq type :hunk)
                            (equal file (second my-egg-stage/unstage-point))
                            (equal line (third my-egg-stage/unstage-point))))
                  (setq restore-pt pos)))
              (when (and nav file sect)
                (let ((info (assoc file my-egg-stage/unstage-info)))
                  (when info
                    (cond
                     ((eq type :diff)
                      (let* ((was-present-here
                              (if (eq sect 'staged)
                                  (second info)
                                (third info)))
                             (was-present-there
                              (if (eq sect 'staged)
                                  (third info)
                                (second info)))
                             (was-invisible-here (eq t was-present-here))
                             (was-invisible-there (eq t was-present-there)))
                        ;; only make invisible if it was invisible in that section before
                        ;; or if it was not present, and opposite section was invisible
                        (when (and was-invisible-there
                                   (or
                                    was-invisible-here
                                    (not was-present-here))
                                   (not (assoc nav buffer-invisibility-spec)))
                          (add-to-invisibility-spec (cons nav t)))))
                     ;; for hunks, unconditionally restore invisibility
                     ((and (eq type :hunk) line)
                      (let ((was-invisile (member line (fourth info)))
                            (is-invisible (assoc nav buffer-invisibility-spec)))
                        (cond ((and was-invisile (not is-invisible))
                               (add-to-invisibility-spec (cons nav t)))
                              ;; below restores visibility, if it was visible before
                              ;; so that moving folded hunk to staged, then unfolding it
                              ;; and moving it back, moves it back unfolded
                              ((and (not was-invisile) is-invisible)
                               (remove-from-invisibility-spec (cons nav t)))))))))))))
        (when restore-pt
          (goto-char restore-pt))))))

(when (fboundp 'egg-add-log-message)
  (defadvice egg-add-log-message (after evil-go-insert activate)
    (evil-insert-state)))

(when (fboundp 'egg-commit-log-edit)
  (defadvice egg-commit-log-edit (after evil-go-insert activate)
    (evil-insert-state)))

;; done in the end of my-emacs-setup anyway
;; (viper-apply-major-mode-modifiers)

(provide 'my-vc-setup)
