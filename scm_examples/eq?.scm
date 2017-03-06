(define order
  (lambda (<)
    (letrec ((loop
	      (lambda (a s)
		(or (null? s)
		    (and (< a (car s))
			 (loop (car s) (cdr s)))))))
      (lambda (a . s)
	(loop a s)))))

(define binary-string=?
  (lambda (str1 str2)
    (let ((n1 (string-length str1))
	  (n2 (string-length str2)))
      (and (= n1 n2)
	   (let ((s1 (string->list str1))
		 (s2 (string->list str2)))
	     (andmap char=? s1 s2))))))

(define string=? (order binary-string=?))

(define equal?
  (let ((void-object (if #f #f)))
    (letrec ((equal?
	      (lambda (a b)
		(cond
		 ;; bool
		 ((and (boolean? a) (boolean? b))
		  (boolean=? a b))
		 ;; char
		 ((and (char? a) (char? b)) (char=? a b))
		 ;; nil
		 ((null? a) (null? b))
		 ;; number
		 ((and (number? a) (number? b)) (= a b))
		 ;; pair
		 ((and (pair? a) (pair? b))
		  (and (equal? (car a) (car b))
		       (equal? (cdr a) (cdr b))))
		 ;; string
		 ((and (string? a) (string? b)) (string=? a b))
		 ;; symbol
		 ((and (symbol? a) (symbol? b)) (eq? a b))
		 ;; vector
		 ((and (vector? a) (vector? b)
		       (= (vector-length a) (vector-length b)))
		  (equal? (vector->list a) (vector->list b)))
		 ;; void
		 ((eq? a void-object) (eq? b void-object))
		 (else #f)))))
      equal?)))

"might fail"
(eq? "abc" "def")
(eq? "def" "def")
(eq? "a" "a")

"shouldnt fail"
(equal? "abc" "def")
(equal? "def" "def")
(equal? "a" "a")


