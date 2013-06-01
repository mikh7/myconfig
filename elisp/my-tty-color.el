
(require 'env)
(provide 'my-tty-color)

(make-variable-frame-local 'tty-defined-color-alist)

(defun set-tty-defined-color-alist (list)
  (if (< emacs-major-version 23)
      (setq tty-defined-color-alist list)
    (modify-frame-parameters 
     (selected-frame)
     (list (cons 'tty-defined-color-alist list)))))

(setq xterm-first-16-colors
      '((0 (0 0 0))
        (1 (205 0 0))
        (2 (0 205 0))
        (3 (205 205 0))
        (4 (0 0 238))
        (5 (205 0 205))
        (6 (0 205 205))
        (7 (229 229 229))
        (8 (77 77 77))       ; gray50
	(9 (255 0 0))        ; red
	(10 (0 255 0))       ; green
	(11 (255 255 0))     ; yellow
	(12 (92 92 255))     ; DEF_COLOR12
	(13 (255 0 255))     ; magenta
	(14 (0 255 255))     ; cyan
	(15 (255 255 255)))) ; white


(defun my-define-tty-color (color-num rgb-list)
  "Defines the color naming it color<n>.
   The rgb-list is a list of 3 numbers between 0 and 255"
  (tty-color-define (format "xterm-color-%d" color-num) color-num
		    (mapcar (lambda (x) (* x 257))
			    rgb-list)))

(defun define-color-cube ()
  (let ((red 0))
    (while (< red 6)
      (let ((green 0))
	(while (< green 6)
	  (let ((blue 0))
	    (while (< blue 6)
	      (let ((idx (+ 16 (* red 36) (* green 6) blue))
		    (r (if (> red 0)
                           (+ (* red 40) 55) 0))
		    (g (if (> green 0)
                           (+ (* green 40) 55) 0))
		    (b (if (> blue 0)
                           (+ (* blue 40) 55) 0)))
		(my-define-tty-color idx (list r g b)))
	      (setq blue (1+ blue))))
	  (setq green (1+ green))))
	(setq red (1+ red)))))

(defun parse-xterm-colors-file ()
  (let ((ret nil))
  (with-temp-buffer
    (insert-file-contents "~/myconfig/xterm-colors")
    (while (re-search-forward
	    "xterm\\*color\\([0-9]+\\): #\\(..\\)\\(..\\)\\(..\\)" nil t)
      (let (num color)
	(setq num (string-to-number (match-string 1)))
	(setq color (list (string-to-number (match-string 2) 16)
			  (string-to-number (match-string 3) 16)
			  (string-to-number (match-string 4) 16)))
	(setq ret (cons (list num color) ret)))))
  ret))

(defvar my-tty-colors-cache (make-hash-table :test #'equal))

(when (not (fboundp 'tty-type))
  (defun tty-type (&optional terminal)
    (let ((term (mygetenv "TERM" (selected-frame))))
      (cond ((or (null term) (string= term ""))
             "none")
            (t term)))))


(defun my-define-tty-colors ()
  (let* ((is-putty (not (null (mygetenv "putty" (selected-frame)))))
         (have-custom 
          (and (not is-putty)
               (null (mygetenv "no_myconfig_colors" (selected-frame)))))
         (key (list is-putty have-custom (tty-type)))
         (colors (gethash key my-tty-colors-cache)))
    (if colors
        (set-tty-defined-color-alist colors)
      (tty-color-clear)
      (let ((i 0))
        (while (< i 16)
          (my-define-tty-color i 
                               (cond ((and
                                       is-putty
                                       (= i 7))
                                      '(196 196 196))
                                     (t (cadr (assoc i xterm-first-16-colors)))))
          (setq i (1+ i))))
      (define-color-cube)
      (if have-custom
          (dolist (i (parse-xterm-colors-file))
            (my-define-tty-color (car i) (cadr i)))
        ;; define greyscale cube
        (dotimes (gray 24)
          (let ((level (+ (* gray 10) 8)))
            (my-define-tty-color (+ 232 gray) (list level level level)))))
      (setq colors tty-defined-color-alist)
      (puthash key colors my-tty-colors-cache)
      (set-tty-defined-color-alist tty-defined-color-alist)
      (clear-face-cache))))

