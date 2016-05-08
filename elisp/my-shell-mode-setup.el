(require 'shell)
(require 'comint)

;;;
;;; fix comint reading extended history for zsh
;;; 
(setq comint-input-ring-separator "\n\\(: [0-9]+:[0-9]+;\\)?") 

;; comint-write-input-history uses comint-input-ring-separator
;; to separate lines. We can't do this coz its a regexp,
;; so reset it to default value 
(defadvice comint-write-input-ring (around fix-history activate)
  (let ((comint-input-ring-separator "\n"))
    (setq ad-return-value ad-do-it)))

(defadvice comint-read-input-ring (around fix-history activate)
  (let (tmp (comint-input-ring-file-name comint-input-ring-file-name)) 
    (when (and (eq major-mode 'shell-mode)
               (equal (getenv "HISTFILE")
                      comint-input-ring-file-name)
               (setq tmp (getenv "HISTFILE_FOR_EMACS")))
      (setq comint-input-ring-file-name tmp))
    (setq ad-return-value ad-do-it)))

;; viper comint mode history browsing via jk keys

(defun we-are-at-last-line-p ()
  (save-excursion 
    (end-of-line)
    (or (eobp)
	(progn 
	  (next-line)
	  (back-to-indentation)
	  (eobp)))))

(defun viper-comint-k (arg)
  "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
  (interactive "P")
  (if (we-are-at-last-line-p)
      (progn
	(call-interactively 'comint-previous-input)
	(beginning-of-line))
    (call-interactively 'evil-previous-line)))

(defun viper-comint-j (arg)
  "Go to previous line if not on the last line of the
  buffer,otherwise go backward in history"
  (interactive "P")
  (if (we-are-at-last-line-p)
      (progn
	(call-interactively 'comint-next-input)
	(beginning-of-line))
    (call-interactively 'evil-next-line)))

(defun viper-comint-enter (arg)
  "Enter key in comint mode"
  (interactive "P")
  (evil-insert-state 1)
  (call-interactively 'my-exec-key-in-emacs))

;; Esc / search in comint mode
(defvar viper-comint-search-idx 0)
(defvar viper-comint-search-regexp nil)

(defun viper-comint-start-search ()
  (interactive)
  (when (fboundp 'evil-normal-state)
    (evil-normal-state)))
  (setq viper-comint-search-regexp 
	(read-from-minibuffer "Match (regexp): " nil nil nil
			      'minibuffer-history-search-history))
  (setq viper-comint-search-idx 0)
  (viper-comint-search-next))

(defun viper-comint-search-next ()
  (interactive)
  (if (null viper-comint-search-regexp)
      (error "")
  (comint-previous-matching-input 
   viper-comint-search-regexp viper-comint-search-idx)
  (setq viper-comint-search-idx (1+ viper-comint-search-idx))))

(defun viper-comint-search-prev ()
  (interactive)
  (if (null viper-comint-search-regexp)
      (error "")
  (comint-previous-matching-input 
   viper-comint-search-regexp viper-comint-search-idx)
  (setq viper-comint-search-idx (1- viper-comint-search-idx))))

(evil-define-key 'normal shell-mode-map "j" 'viper-comint-j)
(evil-define-key 'normal shell-mode-map "k" 'viper-comint-k)
(evil-define-key 'normal shell-mode-map "\C-m" 'viper-comint-enter)
(evil-define-key 'normal shell-mode-map "/" 'viper-comint-start-search)
(evil-define-key 'insert shell-mode-map "\M-/" 'viper-comint-start-search)
(evil-define-key 'normal shell-mode-map "\M-/" 'viper-comint-start-search)
(evil-define-key 'normal shell-mode-map "n" 'viper-comint-search-next)
(evil-define-key 'normal shell-mode-map "N" 'viper-comint-search-prev)

(defun comint-clear-buffer ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

;; weird key combination, but I'm too used to it from slime
(define-key comint-mode-map "\C-c\M-o" 'comint-clear-buffer)

;;;
;;; Colors
;;;
(require 'ansi-color)

(defun mgm-after-shell-mode ()
  ;; remove weird default highlihitng that shell mode uses
  (font-lock-remove-keywords nil '(("^[^ 	\n]+:.*" . font-lock-string-face)))
  (ansi-color-for-comint-mode-on))

;; redo this function, it seems assumes CDPATH always contains ./
(defun cd (dir)
  "Make DIR become the current buffer's default directory.
If your environment includes a `CDPATH' variable, try each one of
that list of directories (separated by occurrences of
`path-separator') when resolving a relative directory name.
The path separator is colon in GNU and GNU-like systems."
  (interactive
   (list (read-directory-name "Change default directory: "
			 default-directory default-directory
			 (and (member cd-path '(nil ("./")))
			      (null (getenv "CDPATH"))))))
  (if (file-name-absolute-p dir)
      (cd-absolute (expand-file-name dir))
    (if (null cd-path)
	(let ((trypath (parse-colon-path (getenv "CDPATH"))))
	  (setq cd-path (or trypath (list "./")))))
    (or (member "./" cd-path)
        (push "./" cd-path))
    (if (not (catch 'found
	       (mapc
		(function (lambda (x)
			    (let ((f (expand-file-name (concat x dir))))
			      (if (file-directory-p f)
				  (progn
				    (cd-absolute f)
				    (throw 'found t))))))
		cd-path)
	       nil))
	(error "No such directory found via CDPATH environment variable"))))

(provide 'my-shell-mode-setup)

