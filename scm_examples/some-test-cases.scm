; test 105
(define a (make-string 1))
(string-set! a 0
             (integer->char 80))
a

(integer->char 80)

;;; torture-test-for-compiler-01.scm
;;; Tests the tail-call optimization. Assumes zeor?, - and #t
;;; The test should return #t
;;;
;;; Programmer: Mayer Goldberg, 2010


(apply list 1 '())
(apply list 1 2 '(2))
(apply list 1 2 3 '(3))

#\x
#\X

(let* ((v (make-vector 3))
       (vs! (lambda (i x) (vector-set! v i x))))
  vs!)

(let* ((v (make-vector 3))
       (vs! (lambda (i x) v)))
  (vs! 0 'a)
  (vs! 1 234)
  (vs! 2 #\P))

(let* ((v (make-vector 3))
       (vs! (lambda (i x) vector-set!)))
  (vs! 0 'a)
  (vs! 1 234)
  (vs! 2 #\P))

(let* ((v (make-vector 3))
       (vs! (lambda (i x) (list v i x))))
  (vs! 0 'a)
  (vs! 1 234)
  (vs! 2 #\P))

;(vector-set! '#(0 0 0) 0 2)

;(let* ((v (make-vector 3))
;       (vs! (lambda (i x) (vector-set! v i x))))
;  (vs! 0 'a)
;  (vs! 1 234)
;  (vs! 2 #\P))
