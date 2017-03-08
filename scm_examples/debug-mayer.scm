(((lambda (x) (x x)) 
(lambda (x) x)) '1234)

(let ((x 1234))
x)

((lambda (x) x) 1234)

(((lambda (x) (x x)) 
(lambda (x) x)) '1234)
(let ((x 1234))
x)
(let ((id (lambda (x) x)))
(let ((x 1234))
(id (let ((x (+ x 1)))
(let ((x (+ x 1))
(y (* x x)))
(id (let ((result (- y x)))
result)))))))

(let ((x 3))
(set! x (+ x 6))
x)

(let ((x 3))
(let ()
(set! x (+ x 6))
x))

(let ((fact 1234))
	(set! fact
		(lambda (n)
			(if 	(zero? n)
				1
				(* n (fact (- n 1))))))
	(fact 0))

(((lambda (x) (x x))
(lambda (fact)
(lambda (n)
(if (zero? n)
1
(* n ((fact fact) (- n 1)))))))
5)

(let ((fact 1234))
(set! fact
(lambda () 'ok))
(fact))

(let ((fact 1234))
(set! fact
(lambda (n)
(+ n 5)))
(fact 10))

(let ((x 3))
(let ((f (lambda () x)))
(set! x 5)
(f)))

(let ((x 3))
(let ((f (lambda ()
(set! x (+ x 1)) x)))
(set! x 5)
(f)))

(let ((x 3))
(let ((f (lambda ()
(set! x (+ x 1)) (set! x (+ x 1)) (set! x (+ x 1)) (set! x (+ x 1)) x)))
(f)))

(define x 3)
((lambda () (set! x (+ x 1)) (set! x (+ x 1)) (set! x (+ x 1)) (set! x (+ x 1)) (set! x (+ x 1)) x))




