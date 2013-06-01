
(defun mm/browse-url-using-url (url &optional new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "URL " url) nil "url" url))

(defvar mm/local-browse-url-function 'mm/browse-url-using-url
  "Function to use when not running on Cygwin/X remote diplay")

(defun mm/get-cygwinx-helper-window-id ()
  "Retreive cygwinxHelper window id from resources. cygwinxHelper
sets this resource when it starts. This will only work when current
frame is on Cygwin's display, so presense of this resource also acts
as a switch to send URL's to cygwin rather then local browse-url"
  (let* ((x-resource-class "cygwinx")
         (x-resource-name "cygwinx")
         (id (x-get-resource "" "helperWindowId")))
    (when id
      (string-to-int id))))

(defun mm/browse-url-on-cygwin (cygwinx-helper-window-id url)
  "Send URL to cygwinxHelper app on Cygwin by splitting it into
multiple client message (since max client message data size is 20
bytes"
  (let* ((len (length url))
         (msg (string-to-list url))
         (msgs (loop for i = 0 then (+ i 19)
                     while (< i len)
                     as submsg = (subseq msg i (min (+ i 19) len))
                     collect submsg)))
    (when id
      (let ((cnt 0))
        (dolist (msg msgs)
          (x-send-client-message
           nil cygwinx-helper-window-id nil
           (if (zerop cnt) "browseUrlStart" "browseUrlMore")
           8
           (cons (length msg) msg))
          (incf cnt))
        (x-send-client-message
         nil cygwinx-helper-window-id nil
         "browseUrlEnd"
         8
         '(0))))))

(defun mm/browse-url (url &rest args)
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((id (mm/get-cygwinx-helper-window-id)))
    (if id
        (mm/browse-url-on-cygwin id url)
      (funcall mm/local-browse-url-function url args))))

(setq
 mm/local-browse-url-function 'mm/browse-url-using-url
 browse-url-browser-function 'mm/browse-url
 w3m-goto-article-function 'mm/browse-url
 mime-browse-url-function 'mm/browse-url)

(provide 'my-browse-url)
