(cons (cons (cons 1 2) (cons 0 '())) (cons #\a 10))
(cons #f (cons #f (cons #f (cons #f (cons #f (cons #f (cons #t (cons #f (cons #f '())))))))))
(define x '(1 2 3 4 5 6 "abc" #t #f))

'if
(if 	(null? x)
	x
	(cons	(char? (car x))
		(char? (cdr x))
	)
)

'null?
(null? x)
(map string? x)
(map number? x)
(map boolean? x)
map
(map char? '(a b c))
(list 'a 'b 'c "d" #\e #\f)
(number? 1)
(number? 2)
(number? #f)
