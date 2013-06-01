;; -*- mode: lisp -*-

(print "Starting CCL-INIT")
(require :asdf)

(load (make-pathname
       :directory
       (append (pathname-directory (user-homedir-pathname)) 
               '("myconfig" "dotfiles"))
       :name "lisp-init-1"
       :type "lisp"))



