((lambda (x) (x x 10000000))
 (lambda (x n)
   (if  (zero? n) 
	#t
        (x x (- n 1)))))



