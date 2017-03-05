1/2
4/6
'(1/2 1/4 1/3)
(define x 1/10)
((lambda (x) x) x)
(set! x 1)
(((lambda (i x) 
	(lambda (y) 
		(set! x 15) 
		x
	)) 2 1/4) 2)
((lambda (x) (set! x 15) x) 1/4)
x

