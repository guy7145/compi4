(define org-append append)
(define org-apply apply)
(define org-< <)
(define org-= =)
(define org-> >)
(define org-+ +)
(define org-/ /)
(define org-* *)
(define org-- -)
(define org-boolean? boolean?)
(define org-car car)
(define org-cdr cdr)
(define org-char->integer char->integer)
(define org-char? char?)
(define org-cons cons)
(define org-denominator denominator)
(define org-eq? eq?)
(define org-integer? integer?)
(define org-integer->char integer->char)
(define org-list list)
(define org-make-string make-string)
(define org-make-vector make-vector)
(define org-map map)
(define org-not not)
(define org-null? null?)
(define org-number? number?)
(define org-numerator numerator)
(define org-pair? pair?)
(define org-procedure? procedure?)
(define org-rational? rational?)
(define org-remainder remainder)
(define org-set-car! set-car!)
(define org-set-cdr! set-cdr!)
(define org-string-length string-length)
(define org-string-ref string-ref)
(define org-string-set! string-set!)
(define org-string->symbol string->symbol)
(define org-string? string?)
(define org-symbol? symbol?)
(define org-symbol->string symbol->string)
(define org-vector vector)
(define org-vector-length vector-length)
(define org-vector-ref vector-ref)
(define org-vector-set! vector-set!)
(define org-vector? vector?)
(define org-zero? zero?)
(load "tdd-tools.scm")
(load "../compi4/library-functions.scm")

; append (variadic)
(define append-inputs
	'((() 2)
	  ((4 5) 2)
	  ((3 3 4 3) 33)
	  (() ())
	  ((1 2) (3 4))))

; apply
(define apply-inputs
	'())

(define numeric-inputs
	'())

(define <-inputs numeric-inputs)
(define =-inputs numeric-inputs)
(define >-inputs numeric-inputs)
(define +-inputs numeric-inputs)
(define /-inputs numeric-inputs)
(define *-inputs numeric-inputs)
(define --inputs numeric-inputs)

; boolean?
(define boolean?-inputs
	'())

; car
(define car-inputs
	'())

; cdr
(define cdr-inputs
	'())

; char->integer
(define char->integer-inputs
	'())

; char?
(define char?-inputs
	'())

; cons
(define cons-inputs
	'())

; denominator
(define denominator-inputs
	'((4/5)
	  (7/3)
	  (4/16)
	  (9/81)
	  (15/60)
	  (2/3)
	  (6/4)))
	
; eq?
(define eq?-inputs
	'())

; integer?
(define integer?-inputs
	'())

; integer->char
(define integer->char-inputs
	'())

; list (variadic)
(define list-inputs
	'((1 2 3 4)
      ()
      (1 2 (3 4))))

; make-string
(define make-string-inputs
	'())

; make-vector
(define make-vector-inputs
	'())

; map
(define map-inputs
	`((,(lambda (x) (+ x 1)) (1 2 3))
	  (,(lambda (x) (- x 1)) (1 2 3))
	  (,(lambda (x) (* x -1)) (1 2 3))))

; not
(define not-inputs
	'((#t)
	  (#f)
	  (1)
	  (0)
	  ('hello)))

; null?
(define null?-inputs
	'())

; number?
(define number?-inputs
	'())

; numerator
(define numerator-inputs
	'())

; pair?
(define pair?-inputs
	'())

; procedure?
(define procedure?-inputs
	'())

; rational?
(define rational?-inputs
	'())

; remainder
(define remainder-inputs
	'())

; set-car!
(define set-car!-inputs
	'())

; set-cdr!
(define set-cdr!-inputs
	'())

; string-length
(define string-length-inputs
	'())

; string-ref
(define string-ref-inputs
	'())

; string-set!
(define string-set!-inputs
	'())

; string->symbol
(define string->symbol-inputs
	'())

; string?
(define string?-inputs
	'())

; symbol?
(define symbol?-inputs
	'())

; symbol->string
(define symbol->string-inputs
	'())

; vector
(define vector-inputs
	'())

; vector-length
(define vector-length-inputs
	'())

; vector-ref
(define vector-ref-inputs
	'())

; vector-set!
(define vector-set!-inputs
	'())

; vector?
(define vector?-inputs
	'())

; zero?
(define zero?-inputs
	'((0)
	  (1)))



(define all-ops
	(org-list
	  (org-list org-append append append-inputs)
      (org-list org-apply apply apply-inputs)
      (org-list org-< < <-inputs)
      (org-list org-= = =-inputs)
      (org-list org-> > >-inputs)
      (org-list org-+ + +-inputs)
      (org-list org-/ / /-inputs)
      (org-list org-* * *-inputs)
      (org-list org-- - --inputs)
      (org-list org-boolean? boolean? boolean?-inputs)
      (org-list org-car car car-inputs)
      (org-list org-cdr cdr cdr-inputs)
      (org-list org-char->integer char->integer char->integer-inputs)
      (org-list org-char? char? char?-inputs)
      (org-list org-cons cons cons-inputs)
     ; (org-list org-denominator denominator denominator-inputs)
      (org-list org-eq? eq? eq?-inputs)
      (org-list org-integer? integer? integer?-inputs)
      (org-list org-integer->char integer->char integer->char-inputs)
      (org-list org-list list list-inputs)
      (org-list org-make-string make-string make-string-inputs)
      (org-list org-make-vector make-vector make-vector-inputs)
      (org-list org-map map map-inputs)
      (org-list org-not not not-inputs)
      (org-list org-null? null? null?-inputs)
      (org-list org-number? number? number?-inputs)
      (org-list org-numerator numerator numerator-inputs)
      (org-list org-pair? pair? pair?-inputs)
      (org-list org-procedure? procedure? procedure?-inputs)
      (org-list org-rational? rational? rational?-inputs)
      (org-list org-remainder remainder remainder-inputs)
      (org-list org-set-car! set-car! set-car!-inputs)
      (org-list org-set-cdr! set-cdr! set-cdr!-inputs)
      (org-list org-string-length string-length string-length-inputs)
      (org-list org-string-ref string-ref string-ref-inputs)
      (org-list org-string-set! string-set! string-set!-inputs)
      (org-list org-string->symbol string->symbol string->symbol-inputs)
      (org-list org-string? string? string?-inputs)
      (org-list org-symbol? symbol? symbol?-inputs)
      (org-list org-symbol->string symbol->string symbol->string-inputs)
      (org-list org-vector vector vector-inputs)
      (org-list org-vector-length vector-length vector-length-inputs)
      (org-list org-vector-ref vector-ref vector-ref-inputs)
      (org-list org-vector-set! vector-set! vector-set!-inputs)
      (org-list org-vector? vector? vector?-inputs)
      (org-list org-zero? zero? zero?-inputs)))

(let ((test-op
  (lambda (op)
    (let* ((f2 (car op))
           (f1 (cadr op))
           (id (lambda (x) x))
           (test-input (lambda (input)
                         (ASSERT-EQUAL-ex-apply input id f1 f2))))
      (map test-input (caddr op))))))
  (map test-op all-ops))