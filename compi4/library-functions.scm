
;(define apply apply);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define <
  (lambda x
    (cond (((null? (cdr x)) #t)
           ((not (--------ASM_BINARY_LT------- (car x) (cadr x))) #f)
           (else (< (cdr x)))))))
(define = =)
(define > >)
(define + +)
(define / /)
(define * *)
(define - -)
;(define boolean? boolean?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define car car);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define cdr cdr);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define char->integer char->integer);;;;;;;;;;;;;;;;;;;;
;(define char? char?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define cons cons);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define denominator denominator);;;;;;;;;;;;;;;;;;;;;;;;
;(define eq? eq?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define integer? integer?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define integer->char integer->char);;;;;;;;;;;;;;;;;;;;

(define list (lambda elements elements))

;(define make-string make-string);;;;;;;;;;;;;;;;;;;;;;;;
;(define make-vector make-vector);;;;;;;;;;;;;;;;;;;;;;;;

(define map
	(lambda (f lst)
		(if (null? lst)
			'()
			(cons (f (car lst)) (map f (cdr lst))))))

(define not
	(lambda (v)
		(if (eq? v #f)
			#t
			#f)))

;(define null? null?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define number? (lambda (x) 
	(or (integer? x) (fraction? x))))

;(define numerator numerator);;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define pair? pair?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define procedure? procedure?);;;;;;;;;;;;;;;;;;;;;;;;;;
;(define rational? rational?);;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define remainder remainder);;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define set-car! set-car;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define set-cdr! set-cdr;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define string-length string-length);;;;;;;;;;;;;;;;;;;;
;(define string-ref string-ref);;;;;;;;;;;;;;;;;;;;;;;;;;
;(define string-set! string-set!)
;(define string->symbol string->symbol);;;;;;;;;;;;;;;;;;
;(define string? string?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define symbol? symbol?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define symbol->string symbol->string);;;;;;;;;;;;;;;;;;
;(define vector vector)
;(define vector-length vector-length);;;;;;;;;;;;;;;;;;;;
;(define vector-ref vector-ref);;;;;;;;;;;;;;;;;;;;;;;;;;
;(define vector-set! vector-set!)
;(define vector? vector?);;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define zero? (lambda (x) (= 0 x)))