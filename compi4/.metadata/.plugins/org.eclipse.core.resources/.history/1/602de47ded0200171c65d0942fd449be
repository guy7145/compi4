
(define get-inst-read cadr)
(define get-inst-write caddr)
(define intersect
  (lambda (s1 s2)
    (fold-left (lambda (acc el) (append acc (if (member el s2) `(,el) '()))) '() s1)))
(define subtraction
  (lambda (s1 s2)
    (fold-left (lambda (acc el) (append acc (if (member el s2) '() `(,el)))) '() s1)))
(define union
  (lambda (s1 s2)
    (append s1 (subtraction s2 s1))))

(define get-all-reads
  (lambda (code)
    (fold-left (lambda (acc el) (union acc (get-inst-read el))) '() code)))

(define remww
  (letrec ((remove-ww (lambda (code reads)
                        (if (null? code)
                            code
                              (let* ((current-instruction (car code))
                                     (rest-instructions (cdr code))
                                     (vital? (not (null? (intersect (get-inst-write current-instruction) reads)))))
                                (if vital?
                                    (cons current-instruction (remove-ww rest-instructions
                                                               (union (get-inst-read current-instruction)
                                                                      (subtraction reads (get-inst-write current-instruction)))))
                                    (remove-ww rest-instructions reads)))))))
    (lambda (code)
      (reverse (remove-ww (reverse code) (get-all-reads code))))))
