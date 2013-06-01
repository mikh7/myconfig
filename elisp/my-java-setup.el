(require 'cc-mode)

(defun mgm-after-java-mode ()
  (c-add-style "java-eclipse" 
               '("java" (tab-width . 4)) 
               t))

(provide 'my-java-setup)
