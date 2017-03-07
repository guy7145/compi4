(define l
  (lambda (x y z f t)
    (lambda (y z)
      (lambda (x y t)
        (cond ((eq? t 1) x)
              ((eq? t 2) y)
              ((eq? t 3) z)
              (else f))))))
(((l 1 2 3 4 5) 6 7) 8 9 1)
(((l 1 2 3 4 5) 6 7) 8 9 2)
(((l 1 2 3 4 5) 6 7) 8 9 3)
(((l 1 2 3 4 5) 6 7) 8 9 4)

(define l2
  (lambda (x1 x2 x3 x4 x5 x6 x7)
    (lambda ()
      (list x1 x2 x3 x4 x5 x6 x7))))

((l2 1 2 3 4 5 6 7))

(define l3
  (lambda (x1 x2 x3)
    (lambda (x4 x5 x6)
      (lambda (x7 x8 x9)
        (list x1 x2 x3 x4 x5 x6 x7 x8 x9)))))

(((l3 1 2 3) 4 5 6) 7 8 9)


(define l3
  (lambda (x1 x2 x3)
    (lambda (x4 x5 x6)
      (cons
       ((lambda (x7 x8 x9)
          (list x7 x8 x9 x1)) x1 x4 x5)
       ((lambda (z c v)
          (list z
                c
                v
                c
                v
                z
                ((lambda ()
                   (lambda (x y)
                     (cons x y))
                   x1 c)))) x5 x6 x2)))))

((l3 1 2 3) 2 1 10)
