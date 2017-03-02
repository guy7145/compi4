(define original-list list)
(define list
  (lambda s
    (if (null? s) 
        s
        (fold-left (lambda (acc el) (if (null? el) acc (cons acc el))) (car s) (cdr s)))))
(begin 1 2 3 4 5)
;(begin (+ 1 2))
1
;(void)
;(list 1 2 3 4)
'(1 2 3 4 5)
'(1 2 3)
#f
;(void)
'symbol
'(#t #f #t #f)
(define e 5)
e
;(set! e 6)
e
1
2
'(4 5)
#t
#f
4

"hello"
'is-it
`me
'(youre)
`(looking for)
#\?

`(looking (for (me ?)) ((here) ?))
`(looking (for (me ?)) ((here) ? . ()))
