(labels (((do-stuff something
            (do-more)
            (do-more)
            (if  (other)
                 (cond))) (&boo))
         
         ((:values  shit))
         (on)
         ((do-stuff and more))))



(cond ((condition) (consequent :key value
                               :other-key lalalala))
      ((and (or multi
                line condition)
            (or blah (stuffs) lala
                (more stuff here (and one 
                                      (two three))
                      more stuff)))
       (more long winded
             stuff right here)
       (stuff)
       (here))
       ((and (this is does not look
                   as good as I had hoped)
             (oh noes))
        (loop
          do more fancy stuff irght here)))

(case obj 
  ((one)
   (two))
  (t (three four)))


(typecase (blah)
  ((sometyep other)
   (if one (two)
       (cond ((four)
              (seventeen))
             ((and four five
                   seix seven
                   (50)))))))

(with-accessors ((one two)
                 (three four)) instance
  (more code))

(with-slots (one two three) object
  (bind ((one (one))
         (two (two))
         ((:values a b c)
           (multiple-value-bind (one two)
               (some-call)
             (blah))))))


(if one two three)
(when one two three
      four five sex seven
      eht (123 oeu
                 oeu))
