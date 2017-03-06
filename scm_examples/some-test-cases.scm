; test 105
(define a (make-string 1))
(string-set! a 0
             (integer->char 80))
a
