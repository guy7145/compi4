;;; pattern-matcher.scm
;;; The pattern-matching package
;;;
;;; Programmer: Mayer Goldberg, 2016

(define match
  (letrec ((match
            (lambda (pat e ret-vals ret-fail)
              (cond ((and (pair? pat) (pair? e))
                     (match (car pat) (car e)
                            (lambda (vals-car)
                              (match (cdr pat) (cdr e)
                                     (lambda (vals-cdr)
                                       (ret-vals
                                        (append vals-car vals-cdr)))
                                     ret-fail))
                            ret-fail))
                    ((and (vector? pat) (vector? e)
                          (= (vector-length pat) (vector-length e))
                          (match (vector->list pat) (vector->list e)
                                 ret-vals ret-fail)))
                    ((procedure? pat)
                     (let ((v (pat e)))
                       (if v (ret-vals v) (ret-fail))))
                    ((equal? pat e) (ret-vals '()))
                    (else (ret-fail))))))
    (lambda (pat e ret-with ret-fail)
      (match pat e
             (lambda (vals) (apply ret-with vals))
             ret-fail))))

(define ?
  (lambda (name . guards)
    (let ((guard?
           (lambda (e)
             (andmap
              (lambda (g?) (g? e))
              guards))))
      (lambda (value)
        (if (guard? value)
            (list value)
            #f)))))

;;; composing patterns

(define pattern-rule
  (lambda (pat handler)
    (lambda (e failure)
      (match pat e handler failure))))

(define compose-patterns
  (letrec ((match-nothing
            (lambda (e failure)
              (failure)))
           (loop
            (lambda (s)
              (if (null? s)
                  match-nothing
                  (let ((match-rest
                         (loop (cdr s)))
                        (match-first (car s)))
                    (lambda (e failure)
                      (match-first e
                       (lambda ()
                         (match-rest e failure)))))))))
    (lambda patterns
      (loop patterns))))

                                        ;(load "pattern-matcher.scm")
;;; cse.scm
;;;
;;; hw2 part 2
;;; Programmer: Guy Danielli, 2016



(define not-list? (lambda (s) (not (list? s))))

(define optimizable-op?
  (lambda (x)
    (and ;(not (number? x))
     (not (equal? 'quote x))
     )
    ))

(define not-optimizable-op?
  (lambda (x)
    (not (optimizable-op? x))))

(define expr-root 'root!)
(define get-sub-name (lambda (sub) (car sub)))
(define get-sub-value (lambda (sub) (cadr sub)))
(define get-sub-parents (lambda (sub) (caddr sub)))
(define first-parent car)
(define rest-parents cdr)
(define initial-instance-count 1)
(define enough-to-optimize (+ initial-instance-count 1))
(define make-sub (lambda (expr parent) (list expr 1 `(,parent))))
(define add-sub-parent
  (lambda (sub parent)
    (let ((sub-parents (get-sub-parents sub)))
      (if (member parent sub-parents)
          sub-parents
          `(,parent ,@sub-parents)))))

(define count-sub
  (lambda (sub parent)
    (list (get-sub-name sub)
          (+ 1 (get-sub-value sub))
          (add-sub-parent sub parent))))

(define modify-subs
  (lambda (subs expr parent)
    (cond ((not-list? expr) subs)
          ((null? subs) `(,(make-sub expr parent)))
          ((equal? (get-sub-name (car subs)) expr) (cons (count-sub (car subs) parent) (cdr subs)))
          (else (cons (car subs) (modify-subs (cdr subs) expr parent))))))

(define evil-quote-case-1?
  (let ((deep-list? (lambda (s) (and (list? s) (list? (car s))))))
    (lambda (f args)
      (and (equal? 'quote f)
           (list? args)
           (deep-list? args)))))

(define evil-quote-case-2?
  (lambda (f args)
    (and (equal? 'quote f)
         (list? args)
         (equal? 1 (length args)))))

(define evil-quote?
  (lambda (f args) (or (evil-quote-case-1? f args) (evil-quote-case-2? f args))))

(define <application-rule>
  (lambda (parent)
    (lambda (subs e)
      ((pattern-rule
        `(,(? 'foo) . ,(? 'args))

        (lambda (foo . args)
          (if (evil-quote? foo (car args))
              subs
              (let* ((this-expr `(,foo ,@(car args)))
                     (subs (modify-subs subs foo this-expr))
                     (subs (fold-left (lambda (acc e) ((<application-rule> this-expr) acc e)) subs (car args)))
                     (subs (modify-subs subs e parent)))
                subs)))

        ) e (lambda () subs)))))

(define expand-subs
  (letrec
      ((expand-subs-with-parent
        (lambda (parent)
          (lambda (subs e)
            (cond ((null? e) subs)
                  ((not-list? e) subs)
                                        ;((equal? 'lambda (car e)) subs)
                                        ;((equal? 'quote (car e)) subs)
                  ((list? e)
                   (let ((subs (fold-left (expand-subs-with-parent e) subs e)))
                     (modify-subs subs e parent))))))))
    (<application-rule> expr-root)))

(define clean-small-subs
  (lambda (subs)
    (cond ((null? subs) subs)
          ((>= (get-sub-value (car subs)) enough-to-optimize) (cons (car subs) (clean-small-subs (cdr subs))))
          (else (clean-small-subs (cdr subs))))))

(define find-sub-by-name
  (lambda (subs sub-name)
    (cond ((null? subs) (format "couldn't find sub named: ~s" sub-name))
          ((equal? (get-sub-name (car subs)) sub-name) (car subs))
          (else (find-sub-by-name (cdr subs) sub-name)))))

(define ^is-sub-necessary?
  (lambda (subs)
    (lambda (sub)
      (if (< (get-sub-value sub) enough-to-optimize)
          #f
          (cond
                                        ; expression has several parents
           ((> (length (get-sub-parents sub)) 1) #t)

                                        ; specific case: parent is root and the expression has several instances
           ((equal? expr-root (first-parent (get-sub-parents sub))) #t)

                                        ; similar to the previous case: expression has several instances inside it's parent expression
           ((> (get-sub-value sub) (get-sub-value (find-sub-by-name subs (first-parent (get-sub-parents sub))))) #t)

                                        ; in any other case the substitution is unnecessary
           (else #f)
           )))))

(define remove-unnecessary-subs
  (lambda (subs)
    (let ((necessary? (^is-sub-necessary? subs)))
      (fold-left (lambda (acc sub)
                   (if (necessary? sub) (append acc `(,sub)) acc)) '() subs))))

(define create-optimizable-subs
  (lambda (expr)
    (let ((subs (expand-subs '() expr)))
      (clean-small-subs (remove-unnecessary-subs subs)))))

(define generate-let-sub
  (lambda (sub)
    `(,(gensym) ,(get-sub-name sub))))

(define rename-subs
  (lambda (subs)
    (if (null? subs)
        subs
        (map generate-let-sub subs))))

(define reverse-list
  (lambda (s)
    (if (null? s) s (append (reverse-list (cdr s)) `(,(car s))))))

(define create-let*-body
  (lambda (expr)
    (rename-subs (create-optimizable-subs expr))))

(define get-rename car)
(define get-expr cadr)
(define apply-sub-to-expr
  (lambda (sub)
    (letrec ((func (lambda (expr)
                     (cond ((null? expr) expr)
                           ((not-list? expr) expr)
                           ((equal? expr (get-expr sub)) (get-rename sub))
                           (else (map func expr))
                           ))))
      func)))

(define my-apply (lambda (x f) (f x)))
(define apply-all-subs-to-expr
  (lambda (subs expr)
    (let ((appliers (map apply-sub-to-expr subs)))
      (fold-left my-apply expr appliers))))

(define apply-sub-on-itself
  (let ((apply-subs-from-parent-to-child
         (lambda (subs)
           (let ((current-sub (car subs))
                 (rest-subs (cdr subs)))
             (if (null? rest-subs)
                 subs
                 (cons (apply-all-subs-to-expr rest-subs current-sub) (apply-sub-on-itself rest-subs)))))))
    (lambda (subs) (reverse-list (apply-subs-from-parent-to-child (reverse-list subs))))))

(define cse
  (lambda (expr)
    (let ((let*-body (create-let*-body expr)))
      (if (null? let*-body)
          expr
          (let ((let*-body (apply-sub-on-itself let*-body))
                (let-op (if (equal? 1 (length let*-body)) 'let 'let*)))
            `(,let-op ,let*-body ,(apply-all-subs-to-expr let*-body expr)))
          ))))


(map cse
     (list '(* (+ 2 (f 3 5) 4) (+ 2 (f 3 5) 4))
           '(+ (+ 1 2 3) (+ 4 5 6) (+ 1 2 3) (+ 4 5 6))
           '(a (a 1) (b 2) (a 1) (b 2) (c 3) (c 3))
           '(f (c (a b)) (a b) (c (a b)))
           '(f (c (a b)) (a b) (c (a b)) (a b))
           '(foo (a b b b b b b))
           '(foo (a (b b) (b c) (b b) (b c) (b b) (b c)))
           '(begin (a) (a) (b) (b) (b) (c) (c) (c) (c))
           '(foo (a) (a) (b) (b) (b) (b) (c) (c) (c))
           '(foo (a) (b) (c) (b) (c) (b) (c) (a))
           '(begin (define goo (a (b b) (b c) (b b) (b c) (b b) (b c))) (a b))
           '(a (f (+ g h) 1 (g (+ g h) (+ g h)) 3 (g (+ g h) (+ g h)) (+ g h)))
           '(f '('(+ x 1)) (f x) (g x) (lambda (x) (f x)) '(+ x 1))
           '(begin '(a b) '(a b))
           '(+ (+ (+ x 2) 1) (+ (+ x 2) 1) (+ (+ x 2) 1) (+ (+ x 2) 1))
           '(let ((a (+ x 1)) (b (+ x 1)))
              (let ((c (+ x 1)) (d (+ x 1)))
                (* a b c d)))
           '(((((((((((+ x 1)))))))))) ((((((((((+ x 1)))))))))))
           '(list (list (list + 2 1)) (list (list + 2 1)))
           '(* (+ (+ 1 (+ 2 (- 3 (+ 4 5))))) (+ (+ 1 (+ 2 (- 3 (+ 4 5))))))
           '(* (+ (* 1 (+ 2 (- 3 (+ 4 5))))) (+ (* 6 (+ 7 (- 8 (+ 4 5))))) (+ (* 9 (+ 10 (- 11 (+ 4 5))))) (+ (* 12 (+ 13 (- 14 (+ 4 5))))))
           '(+ 2 3)
           '(f (f (f (f x))))
           '(* (+ 2 3 4) (+ 2 3 4))
           '(f (g x y) (f (g x y) z))
           '(+ (* (- x y) (* x x)) (* x x) (foo (- x y)) (goo (* (- x y) (* x x))))
           '(f (g x) (g (g x)) (h (g (g x)) (g x)) ((g x) (g x)))
           '(list (cons 'a 'b) (cons 'a 'b) (list (cons 'a 'b) (cons 'a 'b)) (list (list (cons 'a 'b) (cons 'a 'b))))
           '(list '(a b) (list '(a b) '(c d)) (list '(a b) '(c d)))
           '(append '(a b c d e) '(a b c d e) '(g f h) '(a b c d e) '(a b c d e) '(a b c d e) '(g f h))
           '(g (f '('(1 2 3 4 5 6 7 8 9 0) '(a b c d e)) (list f g h) '('(1 2 3 4 5 6 7 8 9 0) '(a b c d e))) (list f g h))
           '(list '(a b) (list '(a b) '(c d)) (list '(a b) '(c d)))
           '(f (+ x 1) (f x) (g x) (lambda (x) (f x)) (+ x 1))
           '(begin '(a b) '(a b))))








