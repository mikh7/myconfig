(defvar ebl-file-regexp "/myconfig/")

(unless (boundp 'load-source-file-function)
  (error "byte-code-cache requires LOAD-SOURCE-FILE-FUNCTION"))

(defvar ebl-old-load-source-file-function
  load-source-file-function
  "Saved LOAD-SOURCE-FILE-FUNCTION")

(defun ebl-load-source-file (fullname file noerror nomessage)
  (let* ((elc-filename (ebl-get-elc-file-name fullname)))
    (cond ((and elc-filename (file-readable-p elc-filename))
           (load elc-filename))
          (t (funcall ebl-old-load-source-file-function
                      fullname file noerror nomessage)))))

(defun ebl-get-elc-file-name (filename)
  "Return the .elc file name from a per-version directory or nil"
  (setq filename (byte-compiler-base-file-name filename))
  (setq filename (file-name-sans-versions filename))
  (setq filename 
        (cond ((eq system-type 'vax-vms)
               (concat (substring filename 0 (string-match ";" filename)) "c"))
              ((string-match emacs-lisp-file-regexp filename)
               (concat (substring filename 0 (match-beginning 0)) ".elc"))
              (t (concat filename ".elc"))))
  (if (string-match ebl-file-regexp (file-truename filename))
    (let* ((dir (file-name-directory filename))
           (file (file-name-nondirectory filename))
           (elc-dir (format ".emacs%d-%d-elc" 
                            emacs-major-version emacs-minor-version))
           (new-dir (if dir (concat (file-name-as-directory dir)
                                    (file-name-as-directory elc-dir))
                      elc-dir))
           (new-file (concat (file-name-as-directory new-dir) file)))
      (condition-case err
          (progn (make-directory new-dir t)
                 new-file)
        (error
         nil)))
    filename))

(defun byte-compile-dest-file (filename)
  "Convert an Emacs Lisp source file name to a compiled file name."
  (ebl-get-elc-file-name filename))

(defadvice find-function-search-for-symbol (around ebl-fix activate)
  (when (string-match "^\\(.*\\)/\\(.emacs[0-9]+-[0-9]+-elc\\)/\\([^/]+\\)\\.el\\(c\\)" library)
    (setq library (format "%s/%s.el" (match-string 1 library) (match-string 3 library))))
  ad-do-it)

(setq load-source-file-function #'ebl-load-source-file)

(require 'bytecomp)
(provide 'elisp-binary-locations)

