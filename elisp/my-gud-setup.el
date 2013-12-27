
(require 'gud)

(defun my-gdb-input (command callback)
  (if (eql emacs-major-version 24)
      (gdb-input command callback)
    (gdb-input (list command callback))))

(defvar gud-popups t
  "If true then GDB buffers will popup on activity. This is
  default behavour of gdb-mi mode, use toggle-gud-popups to disable/enable")

(defun toggle-gud-popups (&optional arg)
  "Toggle `gud-popups' variable"
  (interactive)
  (setq gud-popups (not gud-popups))
  (message "GUD popups are now %s" (if gud-popups "ON" "OFF")))

(defadvice gdb-display-buffer (around disable-gud-popups activate)
  (when gud-popups
    (setq ad-return-value ad-do-it)))

(defun gud-toggle-breakpoint (&optional arg)
  "Set/clear breakpoint at point. With a numeric argument ARG use
temporary break point."
  (interactive "p")
  (if (or (buffer-file-name) (derived-mode-p 'gdb-disassembly-mode))
      (save-excursion
        (beginning-of-line)
        (if (eq (car (fringe-bitmaps-at-pos (point)))
                'breakpoint)
            (gud-remove nil)
          (gud-break nil)))))

(global-set-key [f6] 'gud-next)
(global-set-key [C-f6] 'gud-finish)

(global-set-key [f5] 'gud-step)
(global-set-key [f8] 'gud-go)
(define-key gud-minor-mode-map [C-S-b] 'gud-toggle-breakpoint)

(define-key gud-minor-mode-map "\C-c\C-c" 'gud-cont)

(global-set-key [S-f5] 'gud-stop-subjob)
(global-set-key [C-f7] 'my-run-gdb)
(global-set-key [C-f5] 'my-kill-gdb)

;; Below implementns these bindings only if gud minor mode is active
;; implement them instead globally

;; (defvar gud-viper-vi-map (make-sparse-keymap))

;; (define-key gud-viper-vi-map ";" (make-sparse-keymap))
;; (define-key gud-viper-vi-map ";b" 'gud-toggle-breakpoint)
;; (define-key gud-viper-vi-map ";w" 'gud-watch)

;; (add-to-list 'vimpulse-extra-minor-mode-maps '(gud-minor-mode vi-state gud-viper-vi-map))

(define-key evil-normal-state-map ",b" 'gud-toggle-breakpoint)
(global-set-key [C-f9] 'gud-toggle-breakpoint)
(define-key evil-normal-state-map ",w" 'gud-watch)

(defun my-gud-find-newest-binary (dir)
  (let ((file nil))
    (dolist (f (directory-files dir t) file)
      (if (and (file-executable-p f)
               (not (file-directory-p f))
               (or (not file)
                   (file-newer-than-file-p f file)))
          (setq file f)))))

(defun my-gud-recursively-find-newest-1 (file depth max-depth
                                              dir name executable-p)
  "Helper for `my-gud-recursively-find-newest' function"
  (dolist (f (directory-files dir t) file)
    (let ((is-dir-p (file-directory-p f))
          (is-exec-p (file-executable-p f)))
      (cond ((and (or (not executable-p) is-exec-p)
                  (not is-dir-p)
                  (or (not name)
                      (equal name (file-name-nondirectory f)))
                  (or (not file)
                      (file-newer-than-file-p f file)))
             (setq file f))
            ((and (or (null max-depth) (< depth max-depth))
                  is-dir-p
                  (not (equal (file-name-nondirectory f) "."))
                  (not (equal (file-name-nondirectory f) "..")))
             (setq file (my-gud-recursively-find-newest-1
                         file
                         (1+ depth) max-depth
                         (file-name-as-directory f) name executable-p)))))))

(defun my-gud-recursively-find-newest (dir name executable-p
                                           &optional max-depth)
  "Try to recursively find newest file in the directory.

When NAME is not NIL only consider files with that name.  When
EXECUTABLE-P is not NIL only check executable files.  When
MAX-DEPTH is not NIL then do not descend more then MAX-DEPTH
subdirectory levels deep (zero MAX-DEPTH will only search DIR)
"
  (my-gud-recursively-find-newest-1 nil 0 max-depth dir name executable-p))


(defun my-gud-find-build-system-program (dir)
  "Find newest executable in the specified directory, handles two special
cases:

If directory contains file Jamfile or Jamroot, then it tries to
find the newest executable in the bin/ subdirectory

If directory contains file CMakeLists.txt then it first tries to
recursively find newest CMakeCache.txt file, and if found, the
newest executable file in the directory containing CMakeCache.txt
"
  (let ((jamfile (concat (file-name-as-directory dir) "Jamfile"))
        (jamroot (concat (file-name-as-directory dir) "Jamroot"))
        (cmakelists (concat (file-name-as-directory dir) "CMakeLists.txt"))
        (bin (concat (file-name-as-directory dir) "bin"))
        (cache))
    (cond ((string-match "/wt[^/]*/examples/\\([^/]+\\)/" default-directory)
           (my-gud-recursively-find-newest
            (concat default-directory "../../debug/examples/"
                    (match-string 1 default-directory))
             nil t))
          ((and (or (file-exists-p jamfile)
                    (file-exists-p jamroot))
                (file-exists-p bin))
           (my-gud-recursively-find-newest bin nil t))
          ((and (file-exists-p cmakelists)
                (setq cache (my-gud-recursively-find-newest
                             dir "CMakeCache.txt" nil 2)))
           (my-gud-recursively-find-newest (file-name-directory cache) nil t)))))

(defvar my-gdb-args nil
  "Debugged program arguments")

(defvar my-gdb-run nil
  "True when my gdb run")

(defun my-gdb-kill-buffer-hook ()
  (ignore-errors
    (comint-write-input-ring)))

(defun my-gdb-send-args-and-run ()
  (when my-gdb-args
    (my-gdb-input (format "-exec-arguments %s" args)
                  'ignore)
    (comint-add-to-input-history (format "set args %s" args)))
  ;; (my-gdb-input "-gdb-set target-async 1"
  ;;               (lambda () (my-gdb-input "-exec-run" 'ignore)))
  )

(defun my-gdb-start-hook ()
  (when my-gdb-run
    (my-gdb-send-args-and-run)
    (add-hook 'kill-buffer-hook 'my-gdb-kill-buffer-hook nil t)))

(defun my-gdb-read-args-from-histfile (dir)
  (let* ((default-directory dir)
         (hfile (expand-file-name (or (getenv "GDBHISTFILE")
                                     (if (eq system-type 'ms-dos)
                                         "_gdb_history"
                                       ".gdb_history")))))
    (dolist (file (append '("~/.gdbinit")
                          (unless (string-equal (expand-file-name ".")
                                                (expand-file-name "~"))
                            '(".gdbinit"))))
      (if (file-readable-p (setq file (expand-file-name file)))
          (with-temp-buffer
            (insert-file-contents file)
            ;; TODO? check for "set history save\\(  *on\\)?" and do
            ;; not use history otherwise?
            (while (re-search-forward
                    "^ *set history filename  *\\(.*\\)" nil t)
              (setq hfile (expand-file-name
                           (match-string 1)
                           (file-name-directory file)))))))
    (when (and (stringp hfile)
               (file-readable-p (expand-file-name hfile)))
      (with-temp-buffer
        (insert-file-contents hfile)
        (goto-char (point-max))
        (when (re-search-backward "^ *set args \\(.*\\)" nil t)
          (match-string 1))))))

(defvar gud-gdb-history nil)
(defvar gud-gdb-args-history nil)
(defvar gud-gdb-dir-history nil)

(defvar my-run-gdb-path-alist nil
  "List of conses, each cons being (PATH . BINARY-PATH)
where path is a string to match against current
directory, and BINARY-PATH is the path where to look for the binary")


(defun my-run-gdb (&optional arg)
  "Starts a new debugging session with GDB.

Automatically figures out or asks from user three pieces of
information.

PROGRAM to run.
PROGRAM working directory
ARGUMENTS program arguments.

"
  (interactive)
  (require 'gdb-mi)
  (let* (dir
         strings
         program args
         short-program
         (minor-mode 'gdb)
         (hist-sym 'gud-gdb-history)
         (dir-hist-sym 'gud-gdb-dir-history)
         (args-hist-sym 'gud-gdb-args-history)
         (cmd-name (gud-val 'command-name minor-mode))
         (my-gdb-run t))
    (when (buffer-file-name)
      (setq dir (file-name-directory (file-truename (buffer-file-name)))))
    (or dir (setq dir (expand-file-name default-directory)))
    (setq program
          (or (gud-val dir 'program)
              (my-gud-find-build-system-program dir)))
    (when program (setq program (expand-file-name program)))
    ;; try the directory where we last did make command
    (when (and (null program)
               compilation-directory)
      (setq dir (expand-file-name compilation-directory))
      (setq program
            (or (gud-val dir 'program)
                (my-gud-find-build-system-program dir)))
      (when program (setq program (expand-file-name program))))

    (cond ((not program)
           ;; Unable to figure out the program to run, ask user
           (setq cmdline
                 (gud-query-cmdline 'gdb)))
          (t
           ;; Still ask the user, but provide binary that we found as a default
           (let ((pos (search dir program)))
             (if pos
                 (setq short-program (substring program (length dir)))
               (setq short-program program))
             (setq cmdline
                   (read-from-minibuffer
                    (format "Run %s (like this): " minor-mode)
                    (concat (or cmd-name (symbol-name minor-mode))
                            " "
                            short-program)
                    gud-minibuffer-local-map nil
                    hist-sym)))))
    (when cmdline
      ;; split cmdline into gdb and args, and program name, and then
      ;; re-join them making program name absolute. Its needed because
      ;; program name can be relative to current directory, and we may
      ;; be running gdb from a different directory then current
      (let ((strings (split-string-and-unquote cmdline))
            (new-cmdline ""))
        (setq program (expand-file-name (car (last strings))))
        (dolist (s (butlast strings))
          (unless (equal new-cmdline "")
            (setq new-cmdline (concat new-cmdline " ")))
          (if (string-match "[[:space:]]" s)
              (setq new-cmdline (concat new-cmdline (format "%S" s)))
            (setq new-cmdline (concat new-cmdline s))))
        (setq new-cmdline (concat new-cmdline " "
                                  (if (string-match "[[:space:]]" program)
                                      (format "%S" program)
                                    program)))
        ;; Now fix gud-gdb-history by putting absolute pathname cmdline there,
        ;; even if user had browsed/completed a relative pathname
        ;;
        ;; The justification for this is that initially user would likely run
        ;; gdb from the build directory, and specify relative pathname to the binary
        ;;
        ;; Later after fixing some bugs, user is likely to re-run gdb while some source
        ;; buffer is current, and relative pathname from history will be from the
        ;; wrong directory. So its better to always have absolute pathname in history
        ;;
        ;; TODO: we can actually bind a local copy of fixed gdb
        ;; history based on current directory (making pathnames relative),
        ;; before invoking (gud-query-cmdline). This will show pathnames as relative
        ;; when invoked from the same directory, and as absoulte when invoked from
        ;; any other.
        (when (equal (car gud-gdb-history) cmdline)
          (setcar gud-gdb-history new-cmdline))
        (let ((newdir
               (read-from-minibuffer 
                (format "Directory to run in: ")
                ""
                gud-minibuffer-local-map nil
                dir-hist-sym
                dir)))
          (when (equal newdir "")
            (setq newdir dir))
          (setq dir (expand-file-name newdir)))
       
        (setq args (or (gud-val program 'program-args)
                       (my-gdb-read-args-from-histfile dir)))
       
        (let ((default-directory dir)
              (gud-chdir-before-run nil))
          (setq args
                (read-from-minibuffer
                 (format "Program arguments: ")
                 (or args "")
                 gud-minibuffer-local-map nil
                 args-hist-sym))
          (set (gud-symbol dir nil 'program) program)
          (set (gud-symbol program nil 'program-args) args)
          (add-hook 'gdb-mode-hook 'my-gdb-start-hook)
          (let* ((my-gdb-args args)
                 (filepart (file-name-nondirectory program))
                 (existing-buffer (get-buffer (concat "*gud-" filepart "*"))))
            ;; see if already running
            (if (or (not existing-buffer)
                    (not (get-buffer-process existing-buffer)))
                (gdb new-cmdline)
              (pop-to-buffer existing-buffer)
              (with-current-buffer existing-buffer
                (when gdb-threads-list
                  (if (y-or-n-p "This program is running, kill it? ")
                      (progn
                        (my-gdb-input "kill" 'ignore)
                        (sit-for 0))
                    (error "Program is already running")))
                (my-gdb-input (format "file %S"
                                      program)
                              'ignore)
                (my-gdb-send-args-and-run)))))))))

(defun my-kill-gdb (arg)
  "Kill the current debug session"
  (interactive "P")
  (if (not gud-comint-buffer)
      (error "GDB is not running")
    (let ((kill-buffer-query-functions nil))
      (with-current-buffer gud-comint-buffer
        (let ((kill-buffer-query-functions nil))
          (kill-buffer gud-comint-buffer))))))


(defun my-speedbar-hook ()
  (speedbar-enable-update))

(add-hook 'speedbar-mode-hook 'my-speedbar-hook)

(defun my-gdb-stopped-hook (result)
  (gdb-watch-frame-variables nil))

(add-hook 'gdb-stopped-hooks 'my-gdb-stopped-hook)

(provide 'my-gud-setup)
