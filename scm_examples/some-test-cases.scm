; test 105
;(define a (make-string 1))
;(string-set! a 0
;             (integer->char 80))
;a

;(integer->char 80)

;;; torture-test-for-compiler-01.scm
;;; Tests the tail-call optimization. Assumes zeor?, - and #t
;;; The test should return #t
;;;
;;; Programmer: Mayer Goldberg, 2010

((lambda (x) (x x 400))
 (lambda (x n)
   (if  (zero? n) 
	#t
        (x x (- n 1)))))




