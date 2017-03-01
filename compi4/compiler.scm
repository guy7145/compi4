(load "pc.scm")

(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <line-comment>
  (let ((<end-of-line-comment>
         (new (*parser (char #\newline))
              (*parser <end-of-input>)
              (*disj 2)
              done)))
    (new (*parser (char #\;))

         (*parser <any-char>)
         (*parser <end-of-line-comment>)
         *diff *star

         (*parser <end-of-line-comment>)
         (*caten 3)
         done)))

(define <sexpr-comment>
  (new (*parser (word "#;"))
       (*delayed (lambda () <sexpr>))
       (*caten 2)
       done))

;; ***********************************************
(define <delayed-infix-comment>
  (new (*delayed (lambda () <InfixComment>) ) done))
;; ***********************************************


(define <comment>
  (disj <line-comment>
        <delayed-infix-comment>
        <sexpr-comment>))

(define <skip>
  (disj <comment>
        <whitespace>))

(define ^^<wrapped>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
           (*parser <p>)
           (*parser <wrapper>)
           (*caten 3)
           (*pack-with
            (lambda (_left e _right) e))
           done))))

(define ^<skipped*> (^^<wrapped> (star <skip>)))

;;;;;;;; END OF SKIPPED WHITE SPACE ;;;;;;;;;;;;;;

(define <Boolean>
  (new

   (*parser (char-ci #\#))
   (*parser (char-ci #\f))
   (*parser (char-ci #\t))

   (*disj 2)
   (*caten 2)

   (*pack-with
    (lambda (_ t)
      (eq? t #\t)))

   done))

(define <CharPrefix>
  (new

   (*parser (char #\#))
   (*parser (char #\\))
   (*caten 2)

   done))

(define <VisibleSimpleChar>
  (new

   (*parser (const
             (lambda (ch)
               (char<? #\space ch))))

   done))

(define <NamedChar>
  (new

   (*parser (word-ci "lambda"))
   (*parser (word-ci "newline"))
   (*parser (word-ci "nul"))
   (*parser (word-ci "page"))
   (*parser (word-ci "return"))
   (*parser (word-ci "space"))
   (*parser (word-ci "tab"))

   (*disj 7)

   (*pack (lambda (x)
            (let ((x (list->string x)))
              (cond ((string-ci=? x "lambda")  #\x3bb)
                    ((string-ci=? x "newline") #\newline)
                    ((string-ci=? x "nul")     #\nul)
                    ((string-ci=? x "page")    #\page)
                    ((string-ci=? x "return")  #\return)
                    ((string-ci=? x "space")   #\space)
                    ((string-ci=? x "tab")     #\tab)
                    ))))

   done))

(define <HexChar>
  (let ((zero (char->integer #\0))
        (lc-a (char->integer #\a))
        (uc-a (char->integer #\A)))
    (new (*parser (range #\0 #\9))
         (*pack
          (lambda (ch)
            (- (char->integer ch) zero)))

         (*parser (range #\a #\f))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) lc-a))))

         (*parser (range #\A #\F))
         (*pack
          (lambda (ch)
            (+ 10 (- (char->integer ch) uc-a))))

         (*disj 3)
         done)))

(define <HexUnicodeChar>
  (letrec ((hex->int (lambda (lst c)
                       (if (null? lst)
                           c
                           (hex->int (cdr lst) (+ (car lst) (* 16 c))))
                       )))
    (new
     (*parser (char #\x))
     (*parser <HexChar>) *plus
     (*caten 2)

     (*pack-with
      (lambda (x hc)
        (integer->char (hex->int `(,@hc) 0))))

     done)))

(define <Char>
  (new
   (*parser <CharPrefix>)
   (*parser <NamedChar>)
   (*parser <HexUnicodeChar>)
   (*parser <VisibleSimpleChar>)

   (*disj 3)
   (*caten 2)

   (*pack-with
    (lambda (p cs)
      cs;(string->symbol (string cs))
      ))

   done))

(define <Natural>
  (let ((char->int (lambda (c)
                     (- (char->integer c)
                        (char->integer #\0)))))
    (new
     (*parser (range #\0 #\9)) *plus
     (*pack
      (lambda (lst)
        (letrec ((lst->int (lambda (lst c)
                             (if (null? lst)
                                 c
                                 (lst->int (cdr lst) (+ (* c 10) (char->int (car lst))))))))
          (lst->int lst 0))))
     done)))

(define <Integer>
  (new
   (*parser (char #\+))
   (*parser (char #\-))
   (*disj 2) *maybe
   (*parser <Natural>)
   (*caten 2)
   (*pack-with
    (lambda (s n)
      (if (and (car s) (equal? (cadr s) #\-))
          (- n)
          n)))
   done))

(define <Fraction>
  (new
   (*parser <Integer>)
   (*parser (char #\/))
   (*parser <Natural>)
   (*caten 3)
   (*pack-with
    (lambda (int _ nat)
      (/ int nat)))
   done))

(define <Number>
  (new
   (*parser <Fraction>)
   (*parser <Integer>)
   (*disj 2)
   (*delayed (lambda () <SymbolChar>))
   (*parser (range #\0 #\9))
   *diff
   *not-followed-by
   done))

(define <StringVisibleChar>
  (new
   (*parser (const
             (lambda (ch)
               (char<=? #\space ch))))
   done))

(define <StringMetaChar>
  (new
   (*parser (word-ci "\\\\"))
   (*parser (word-ci "\\\""))
   (*parser (word-ci "\\t"))
   (*parser (word-ci "\\f"))
   (*parser (word-ci "\\n"))
   (*parser (word-ci "\\r"))
   (*disj 6)
   (*pack (lambda (x)
            (let ((c (cadr x)))
              (cond ((char-ci=? c #\t) #\tab)
                    ((char-ci=? c #\f) #\x0c)
                    ((char-ci=? c #\n) #\newline)
                    ((char-ci=? c #\r) #\return)
                    (else c)))))
   done))

(define <StringHexChar>
  (letrec ((hex->int (lambda (lst c)
                       (if (null? lst)
                           c
                           (hex->int (cdr lst) (+ (car lst) (* 16 c))))
                       )))
    (new
     (*parser (word-ci "\\x"))
     (*parser <HexChar>) *star
     (*parser (char #\;))
     (*caten 3)
     (*pack-with
      (lambda (x lst _)
        (integer->char (hex->int lst 0))))
     done)))

(define <StringChar>
  (new
   (*parser <StringHexChar>)
   (*parser <StringMetaChar>)
   (*parser <StringVisibleChar>)
   (*disj 3)
   done))

(define <String>
  (new
   (*parser (char #\"))
   (*parser <StringChar>)
   (*parser (char #\"))
   *diff
   *star
   (*parser (char #\"))
   (*caten 3)
   (*pack-with
    (lambda (< lst >)
      (list->string lst)))
   done))

(define <SymbolChar>
  (new
   (*parser (range #\0 #\9))
   (*parser (range #\a #\z))
   (*parser (range #\A #\Z))
   (*parser (char #\!))
   (*parser (char #\$))
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\-))
   (*parser (char #\_))
   (*parser (char #\=))
   (*parser (char #\+))
   (*parser (char #\<))
   (*parser (char #\>))
   (*parser (char #\?))
   (*parser (char #\/))
   (*disj 15)
   (*pack char-downcase)
   done))

(define <Symbol>
  (new
   (*parser <SymbolChar>) *plus
   (*pack (lambda (lst)
            (string->symbol
             (list->string lst))))
   done))

(define <ProperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>)) *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (_ lst __) lst))
   done))

(define <ImproperList>
  (new
   (*parser (char #\())
   (*delayed (lambda () <sexpr>)) *plus
   (*parser (char #\.))
   (*delayed (lambda () <sexpr>))
   (*parser (char #\)))
   (*caten 5)
   (*pack-with
    (lambda (_ lst __ itm ___)
      (append lst itm)))
   done))

(define <Vector>
  (new
   (*parser (word "#("))
   (*delayed (lambda () <sexpr>)) *star
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (_ lst __)
      (list->vector lst)))
   done))

(define <Quoted>
  (new
   (*parser (char #\'))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'quote exp)))
   done))

(define <QuasiQuoted>
  (new
   (*parser (char #\`))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'quasiquote exp)))
   done))

(define <Unquoted>
  (new
   (*parser (char #\,))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'unquote exp)))
   done))

(define <UnquoteAndSpliced>
  (new
   (*parser (word ",@"))
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (_ exp)
      (list 'unquote-splicing exp)))
   done))

                                        ; ************************************************************************************************************************************************************************************


(define *<InfixExpressionDelayed>
  (*delayed (lambda () <InfixExpression>)))

(define <InfixPrefixExtensionPrefix>
  (^<skipped*>
   (new
    (*parser (char #\#))
    (*parser (char #\#))
    (*caten 2)
    (*parser (char #\#))
    (*parser (char #\%))
    (*caten 2)
    (*disj 2)
    (*pack-with
     (lambda (x1 x2) (list->string `(,x1 (,@x2)))))
    done)))

(define <InfixExtension>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
   *<InfixExpressionDelayed>
   (*caten 2)
   (*pack-with
    (lambda (_ expr) expr))
   done))

(define <PowerSymbol>
  (new
   (*parser (char #\^))
   (*parser (char #\*))
   (*parser (char #\*))
   (*caten 2)
   (*pack-with
    (lambda (x1 x2) (list->string `(,x1 (,@x2)))))
   (*disj 2)
   done))

;; Auxilary Lambdas
;; 

; for backward compatibility (;
(define build-op-formula
  (lambda (<op>)
    (lambda (l r)
      `(,<op> ,l ,r))))

(define <operator-2ops>
  (lambda (<folding> <formula-builder>)
    (lambda (<op-char-parser> <fold-operation> <next-parser>)
      (new
       (*parser <next-parser>)
       (*parser (^<skipped*> <op-char-parser>))
       (*caten 2)
       (*pack-with
        (lambda (expr op-char) expr))
       *plus
       (*parser <next-parser>)
       (*caten 2)
       (*pack-with
        (let ((formula-builder (<formula-builder> <fold-operation>)))
          (lambda (list element)
            (<folding> formula-builder element list))))
       (*parser <next-parser>)
       (*disj 2)
       done))))

(define <operator-2ops-right-lr> (<operator-2ops> fold-right build-op-formula))

;; Level
;;
(define <InfixSymbol>
  (new
   (*parser <SymbolChar>)
   (*parser (char #\+))
   (*parser (char #\-))
   (*parser (char #\*))
   (*parser (char #\/))
   (*parser <PowerSymbol>)
   (*disj 5)
   *diff
   *plus
   (*pack (lambda (lst)
            (string->symbol
             (list->string lst))))
   done))

(define <InfixSexprEscape>
  (new
   (*parser <InfixPrefixExtensionPrefix>)
   (*delayed (lambda () <sexpr>))
   (*caten 2)
   (*pack-with
    (lambda (prefix sexpr) sexpr))
   done))

(define <infix-number> (not-followed-by (disj <Fraction> <Integer>) <InfixSymbol>))

(define <InfixParen>
  (new
   (*parser (char #\())
   *<InfixExpressionDelayed>
   (*parser (char #\)))
   (*caten 3)
   (*pack-with
    (lambda (lp expr rp) expr))
   done))

(define <Primitives-and-such> (disj <infix-number> <InfixSexprEscape> <InfixSymbol>))
(define <Level-Paren-&-Friends> (disj <InfixParen> <Primitives-and-such>))

;; Level
;;
(define <InfixArgList>
  (^<skipped*> (new
                ;; args ;;
                ; first arg
                *<InfixExpressionDelayed>
                                        ; rest args (some or none)
                (*parser (char #\,))
                *<InfixExpressionDelayed>
                (*caten 2)
                (*pack-with (lambda (_ expr) expr))
                *star
                                        ; catenate
                (*caten 2)
                (*pack-with
                 (lambda (first-arg rest-args) `(,first-arg ,@rest-args)))
                ;; or no args ;;
                (*parser <epsilon>)
                ;; args | no args ;;
                (*disj 2)
                done)))

(define <InfixFuncall>
  (new
   (*parser (^<skipped*> <Level-Paren-&-Friends>))
   (*parser (char #\())
   (*parser <InfixArgList>)
   (*parser (char #\)))
   (*caten 4)
   (*pack-with
    (lambda (function lp args rp)
      `(,function ,@args)))
   done))

(define <InfixArrayGet> (^<skipped*>
                         (new
                          (*parser (^<skipped*> <Level-Paren-&-Friends>))
                          (*parser (char #\[))
                          (*delayed (lambda () <InfixArrayGet>))
                          (*delayed (lambda () <InfixExpression>))
                          (*disj 2)
                          (*parser (char #\]))
                          (*caten 3)
                          (*pack-with
                           (lambda (lp expr rp) expr))
                          *plus
                          (*caten 2)
                          (*pack-with
                           (let ((expr-builder (build-op-formula 'vector-ref)))
                             (lambda (arr is)
                               (fold-left expr-builder arr is))))
                          done)))

(define <Level-ArrFun> (disj <InfixArrayGet> <InfixFuncall> <Level-Paren-&-Friends>))

;; Level
;;

(define <InfixNeg>
  (new (*parser (let ((<next-level> (new (*delayed (lambda () <InfixNeg>))
                                         (*parser <Level-ArrFun>)
                                         (*disj 2)
                                         done)))
                  (new
                   ;; (without parentesis) 
                   ;; no spaces between '-' and the following expression 
                   (*parser (char #\-))
                   (*parser <next-level>)
                   (*caten 2)
                   (*pack-with
                    (lambda (minus element)
                      (cond ((number? element) (- element))
                            (else `(- ,element)))))
                   ;; (with parentesis) 
                   ;; spaces between '-' and the following expression
                   (*parser (^<skipped*> (char #\-)))
                   (*parser <next-level>)
                   (*caten 2)
                   (*pack-with
                    (lambda (minus element) `(- ,element)))
                   (*disj 2)
                   done)))
       (*parser <Level-ArrFun>)
       (*disj 2)
       done))

;; Level
;; 
(define <InfixPow> (<operator-2ops-right-lr> <PowerSymbol> (string->symbol "expt") <InfixNeg>))

;; Level
;; 
(define build-op-formula-op1-op2
  (lambda (element op-expr-pair)
    (let ((op (car op-expr-pair)) (expr (cdr op-expr-pair)))
      `(,op ,element ,@expr))))

(define <level-op1-op2>
  (lambda (<next-level> op-char1 op-char2 op-str1 op-str2)
    (new (*parser <next-level>)
         (*parser (^<skipped*> (char op-char1)))
         (*parser (^<skipped*> (char op-char2)))
         (*disj 2)
         (*pack (lambda (op) (cond ((char=? op op-char1) (string->symbol op-str1))
                                   (else (string->symbol op-str2)))))
         (*parser <next-level>)
         (*caten 2)
         *plus
         (*caten 2)
         (*pack-with
          (lambda (element list)
            (fold-left build-op-formula-op1-op2 element list)))
         (*parser <next-level>)
         (*disj 2)
         done)))

;; Level
;; 
(define <Level-MulDiv> (<level-op1-op2> <InfixPow> #\/ #\* "/" "*"))

;; Level
;;
(define <Level-AddSub> (<level-op1-op2> <Level-MulDiv> #\+ #\- "+" "-"))

;; Entry Point
;; 
(define <InfixExpression> (^<skipped*> <Level-AddSub>))

;; Comments
;; 
(define <InfixComment>
  (new (*parser (word "#;"))
       (*parser <InfixExpression>)
       (*caten 2)
       (*pack-with (lambda (_1 _2) (void)))
       done))

                                        ; ************************************************************************************************************************************************************************************

(define <sexpr>
  (^<skipped*>
   (disj <Boolean>
         <InfixComment>
         <InfixExtension>
         <Char>
         <String>
         <Number>
         <Symbol>
         <ProperList>
         <ImproperList>
         <Vector>
         <Quoted>
         <QuasiQuoted>
         <Unquoted>
         <UnquoteAndSpliced>
         )))

                                        ;*************************************************************************************************

(load "pattern-matcher.scm")

#| .:: tools rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define *void-object* (void))

(define *reserved-words*
  '(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!))

(define *error-text* "ERROR")

(define *error-continuation*
  (lambda () *error-text*))

(define reserved-word?
  (lambda (x)
    (member x *reserved-words*)))

(define not-reserved-word?
  (lambda (x)
    (not (member x *reserved-words*))))

(define var?
  (lambda (x)
    (and (symbol? x)
         (not-reserved-word? x))))

(define simple-const?
  (let ((preds (list boolean? char? number? string?)))
    (lambda (e)
      (ormap (lambda (p?) (p? e)) preds))))

(define listify
  (lambda (x)
    (cond ((null? x) '())
          ((pair? x) x)
          (else `(,x)))))

(define list-is-duplicative?
  (lambda (s)
    (cond ((null? s) #f)
          ((member (car s) (cdr s)) #t)
          (else (list-is-duplicative? (cdr s))))))

(define beginify
  (lambda (s)
    (cond ((null? s) *void-object*)
          ((null? (cdr s)) (car s))
          (else `(begin ,@s)))))

#| .:: basic rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <void-rule>
  (pattern-rule
   (void)
   (lambda () `(const ,*void-object*))))

(define <const-rule>
  (pattern-rule
   (? 'c simple-const?)
   (lambda (c) `(const ,c))))

(define <quote-rule>
  (pattern-rule
   `(quote ,(? 'c))
   (lambda (c) `(const ,c))))

(define <var-rule>
  (pattern-rule
   (? 'var var?)
   (lambda (var) `(var ,var))))

#| .:: assignment rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <assignment-rule>
  (pattern-rule
   `(set! ,(? 'var var?) ,(? 'val))
   (lambda (var val)
     `(set ,(parse var) ,(parse val)))))

#| .:: application rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <application-rule>
  (pattern-rule
   `(,(? 'foo not-reserved-word?) . ,(? 'args))
   (lambda (foo . args)
     `(applic ,(parse foo) (,@(map parse (car args)))))))


#| .:: if rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <if2-rule>
  (pattern-rule
   `(if ,(? 'test) ,(? 'dit))
   (lambda (test dit)
     `(if3 ,(parse test) ,(parse dit) (const ,*void-object*)))))

(define <if3-rule>
  (pattern-rule
   `(if ,(? 'test) ,(? 'dit) ,(? 'dif))
   (lambda (test dit dif)
     `(if3 ,(parse test) ,(parse dit) ,(parse dif)))))

#| .:: disjunction rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <disj-rule-no-args>
  (pattern-rule
   `(or)
   (lambda () `(const ,#f))))

(define <disj-rule-single-arg>
  (pattern-rule
   `(or ,(? 'expr))
   (lambda (expr) (parse expr) )))

(define <disj-rule-several-args>
  (pattern-rule
   `(or ,(? 'expr) . ,(? 'rest-exprs))
   (lambda (expr . rest-exprs)
     (let ((rest-exprs-unwrapped (car rest-exprs)))
       `(or (,(parse expr) ,@(map parse rest-exprs-unwrapped)))))))

(define <disj-rule>
  (compose-patterns
   <disj-rule-no-args>
   <disj-rule-single-arg>
   <disj-rule-several-args>
   ))

#| .:: and rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <and-rule-no-args>
  (pattern-rule
   `(and)
   (lambda () `(const ,#t))))

(define <and-rule-with-args>
  (pattern-rule
   `(and ,(? 'expr) . ,(? 'rest-exprs))
   (lambda (expr . rest-exprs)
     (letrec ((rest-exprs-unwrapped (car rest-exprs))
              (and->if (lambda (lst)
                         (if (null? (cdr lst))
                             (car lst)
                             (list 'if
                                   (car lst)
                                   (and->if (cdr lst))
                                   #f)))))
       (parse (and->if `(,expr ,@(car rest-exprs))))))))

(define <and-rule>
  (compose-patterns
   <and-rule-no-args>
   <and-rule-with-args>))

#| .:: cond rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <cond-rule>
  (pattern-rule
   `(cond ,(? 'expr) . ,(? 'exprs))
   (lambda (head tail)
     (letrec ((cond->if (lambda (lst)
                          (if (null? lst)
                              (void)
                              (if (equal? 'else (caar lst))
                                  (beginify (cdar lst))
                                  `(if ,(caar lst)
                                       ,(beginify (cdar lst))
                                       ,(cond->if (cdr lst))))))))
       (parse (cond->if `(,head ,@tail)))))))

#| .:: define rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define <define-rule>
  (pattern-rule
   `(define ,(? 'var) ,(? 'val) . ,(? 'val-rest))
   (lambda (var val . val-rest)
     (if (null? val-rest)
         `(def ,(parse var) ,(parse val))
         `(def ,(parse var) ,(parse (beginify (cons val (car val-rest)))))))))

(define merge-bodies
  (lambda (body rest-body)
    (if (null? rest-body)
        body
        (beginify (cons body (car rest-body))))))

(define <define-mit-rule-var>
  (pattern-rule
   `(define (,(? 'object) . ,(? 'var-arg)) ,(? 'body) . ,(? 'rest-body))
   (lambda (object var-arg body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       `(def ,(parse object) ,(parse `(lambda ,var-arg ,body)))
       ))))

(define <define-mit-rule-simple-opt>
  (pattern-rule
   `(define (,(? 'object) ,(? 'args)) ,(? 'body))
   (lambda (object args body . rest-body)
     `(def ,(parse object) ,(parse `(lambda ,args ,body))))))

(define <define-mit-rule>
  (compose-patterns
   <define-mit-rule-var>
   <define-mit-rule-simple-opt>
   ))


#| .:: begin rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define get-tag car)
(define get-data cdr)

(define <begin-rule-empty>
  (pattern-rule
   `(begin)
   (lambda () `(const ,*void-object*))))

(define <begin-rule-single-statement>
  (pattern-rule
   `(begin ,(? 'body))
   (lambda (body) (parse body))))

(define flatten-list
  (lambda (s)
    (cond ((null? s) '())
          ((list? (car s)) (append (car s) (flatten-list (cdr s))))
          (else (cons (car s) (flatten-list (cdr s)))))))

(define <begin-rule-several-statements>
  (let ((parse-unwrap
         (lambda (e)
           (let ((e-tagged (parse e)))
             (if (equal? 'seq (get-tag e-tagged)) (cadr e-tagged) (list e-tagged))))))
    (pattern-rule
     `(begin ,(? 'first-statement) . ,(? 'rest-statements))
     (lambda (first-statement . rest-statements)
       (let ((body (cons first-statement (car rest-statements))))
         `(seq ,(flatten-list (map parse-unwrap body))))))))

(define <seq-rule-explicit>
  (compose-patterns
   <begin-rule-empty>
   <begin-rule-single-statement>
   <begin-rule-several-statements>
   ))

#| .:: lambda rules ::. ______________________________________________________________________________________________________________________________________________________|#

(define identify-lambda
  (lambda (args ret-simple ret-opt ret-var)
    (cond ((null? args) (ret-simple '()))
          ((symbol? args) (ret-var args))
          (else (identify-lambda
                 (cdr args)
                 (lambda (s) (ret-simple `(,(car args) ,@s))) ;simple
                 (lambda (s . opt) (ret-opt `(,(car args) ,@s) (car opt))) ; opt
                 (lambda (var) (ret-opt `(,(car args)) `(,var)))) ; var
           ))))

(define args-not-duplicative?
  (lambda (args)
    (not (and (list? args) (list-is-duplicative? args)))))

(define <lambda-rule>
  (pattern-rule
   `(lambda ,(? 'args args-not-duplicative?) ,(? 'body) . ,(? 'rest-body))
   (lambda (args body . rest-body)
     (let ((rest-body (car rest-body)))

       (let ((body (if (null? rest-body)
                       body
                       (beginify (cons body rest-body)))))

         (let ((parsed-body (parse body)))

           (identify-lambda
            args
            (lambda (s) `(lambda-simple ,s ,parsed-body)) ; simple
            (lambda (s opt) `(lambda-opt ,s ,@opt ,parsed-body)) ; opt
            (lambda (var) `(lambda-var ,var ,parsed-body)) ; var
            )))))))

#| .:: let rule ::. ______________________________________________________________________________________________________________________________________________________|#

(define <let-no-args-rule>
  (pattern-rule
   `(let () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ,body)))))))

(define <let-rule>
  (lambda (e fail-cont)
    ((pattern-rule
      `(let (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
      (lambda (args-head rest body . rest-body)
        (let ((args (if (null? rest)
                        `(,args-head)
                        `(,args-head ,@rest)))
              (body (if (null? rest-body)
                        `(,body)
                        `(,body ,@(car rest-body)))))
          (if (list-is-duplicative? (map car args))
              (fail-cont)
              (parse `((lambda ,(map car args) ,@body) ,@(map cadr args))))))) e fail-cont)))

(define <let*-no-args-rule>
  (pattern-rule
   `(let* () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ,body)))))))

(define <let*-rule>
  (pattern-rule
   `(let* (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
   (lambda (args-head rest head-body . rest-body)
     (letrec ((args (if (null? rest)
                        `(,args-head)
                        `(,args-head ,@rest)))
              (body (if (null? rest-body)
                        `(,head-body)
                        `(,head-body ,@(car rest-body))))
              (let*->encapsulated-lambdas (lambda (args body)
                                            (if (null? args)
                                                body
                                                `(((lambda (,(caar args))
                                                     ,@(let*->encapsulated-lambdas (cdr args) body))
                                                   ,@(cdar args)))))))
       (parse (car (let*->encapsulated-lambdas args body)))))))

(define <letrec-no-args-rule>
  (pattern-rule
   `(letrec () ,(? 'body) . ,(? 'rest))
   (lambda (body . rest-body)
     (let ((body (merge-bodies body rest-body)))
       (parse `((lambda () ((lambda () ,body)))))))))

(define <letrec-rule>
  (lambda (e fail-cont)
    ((pattern-rule
      `(letrec (,(? 'args) . ,(? 'rest)) ,(? 'body) . ,(? 'rest))
      (lambda (args-head rest head-body . rest-body)
        (letrec ((args (if (null? rest)
                           `(,args-head)
                           `(,args-head ,@rest)))
                 (body (if (null? rest-body)
                           `(,head-body)
                           `(,head-body ,@(car rest-body))))
                 (args->set (lambda (lst)
                              (if (null? (cdr lst))
                                  `((set! ,(caar lst) ,@(cdar lst)))
                                  `((set! ,(caar lst) ,@(cdar lst)) ,@(args->set (cdr lst)))))))
          (if (list-is-duplicative? (map car args))
              (fail-cont)
              (parse `((lambda ,(map car args) ,@(append (args->set args) `(((lambda () ,@body))))) ,@(map (lambda (x) #f) args))))))) e fail-cont)))

(define <let-rules>
  (compose-patterns
   <let-no-args-rule>
   <let-rule>
   <let*-no-args-rule>
   <let*-rule>
   <letrec-no-args-rule>
   <letrec-rule>
   ))


#| .:: qq rule ::. ______________________________________________________________________________________________________________________________________________________|#

;;; qq.scm
;;; A naive, one-level quasiquote implementation + optimizations
;;;
;;; Programmer: Mayer Goldberg, 2016



;;;

(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
           (eq? (car e) tag)
           (pair? (cdr e))
           (null? (cddr e))))))

(define quote? (^quote? 'quote))
(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))

(define const?
  (let ((simple-sexprs-predicates
         (list boolean? char? number? string?)))
    (lambda (e)
      (or (ormap (lambda (p?) (p? e))
           simple-sexprs-predicates)
          (quote? e)))))

(define quotify
  (lambda (e)
    (if (or (null? e)
            (pair? e)
            (symbol? e)
            (vector? e))
        `',e
        e)))

(define unquotify
  (lambda (e)
    (if (quote? e)
        (cadr e)
        e)))

(define const-pair?
  (lambda (e)
    (and (quote? e)
         (pair? (cadr e)))))

(define expand-qq
  (letrec ((expand-qq
            (lambda (e)
              (cond ((unquote? e) (cadr e))
                    ((unquote-splicing? e)
                     (error 'expand-qq
                            "unquote-splicing here makes no sense!"))
                    ((pair? e)
                     (let ((a (car e))
                           (b (cdr e)))
                       (cond ((unquote-splicing? a)
                              `(append ,(cadr a) ,(expand-qq b)))
                             ((unquote-splicing? b)
                              `(cons ,(expand-qq a) ,(cadr b)))
                             (else `(cons ,(expand-qq a) ,(expand-qq b))))))
                    ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
                    ((or (null? e) (symbol? e)) `',e)
                    (else e))))
           (optimize-qq-expansion (lambda (e) (optimizer e (lambda () e))))
           (optimizer
            (compose-patterns
             (pattern-rule
              `(append ,(? 'e) '())
              (lambda (e) (optimize-qq-expansion e)))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify `(,@(unquotify c1) ,(unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(append ,(? 'c1 const-pair?) ,(? 'c2 const-pair?))
              (lambda (c1 c2)
                (let ((c (quotify (append (unquotify c1) (unquotify c2)))))
                  c)))
             (pattern-rule
              `(append ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  `(append ,e1 ,e2))))
             (pattern-rule
              `(cons ,(? 'c1 const?) (cons ,(? 'c2 const?) ,(? 'e)))
              (lambda (c1 c2 e)
                (let ((c (quotify (list (unquotify c1) (unquotify c2))))
                      (e (optimize-qq-expansion e)))
                  (optimize-qq-expansion `(append ,c ,e)))))
             (pattern-rule
              `(cons ,(? 'e1) ,(? 'e2))
              (lambda (e1 e2)
                (let ((e1 (optimize-qq-expansion e1))
                      (e2 (optimize-qq-expansion e2)))
                  (if (and (const? e1) (const? e2))
                      (quotify (cons (unquotify e1) (unquotify e2)))
                      `(cons ,e1 ,e2))))))))
    (lambda (e)
      (optimize-qq-expansion
       (expand-qq e)))))
#|
(define <qq-rule>
  (pattern-rule
   `(quasiquote ,(? 'd))
   (lambda (d) d)))|#

(define <qq-rule>
  (pattern-rule
   (? 'qq (lambda (x) (list? x)) (lambda (x)
                                   (and (equal? 'quasiquote (get-tag x))
                                        (not (equal? '() (get-data x))))))
   (lambda (c) (parse (expand-qq (cadr c))))))

#| .:: PARSING INTERFACE ::. ______________________________________________________________________________________________________________________________________________________|#

(define tag-parse
  (let ((run
         (compose-patterns
          <qq-rule>

          <void-rule>
          <const-rule>
          <quote-rule>
          <var-rule>

          <if2-rule>
          <if3-rule>

          <define-mit-rule>
          <define-rule>

          <disj-rule>

          <lambda-rule>

          <seq-rule-explicit>

          <assignment-rule>
          <application-rule>

          <and-rule-no-args>
          <and-rule>
          <cond-rule>

          <let-rules>
          )
         ))
    (lambda (sexpr)
      (run sexpr *error-continuation*))))

(define parse tag-parse)

;;
;; *************************************************************************************************************************************
;; _________applic-nil_____________________________________________
;; ________________________________________________________________ 

(define remove-applic-lambda-nil
  (let ((run (compose-patterns
              (pattern-rule
               `(var ,(? 'var))
               (lambda (var) `(var ,var)))

              (pattern-rule
               `(const ,(? 'const))
               (lambda (const) `(const ,const)))

              (pattern-rule
               `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
               (lambda (test dit dif) `(if3 ,(remove-applic-lambda-nil test) ,(remove-applic-lambda-nil dit) ,(remove-applic-lambda-nil dif))))

              (pattern-rule
               `(def ,(? 'var-name) ,(? 'val))
               (lambda (var-name val) `(def ,var-name ,(remove-applic-lambda-nil val))))

              (let ((lambda-nil-body-get
                     (pattern-rule
                      `(lambda-simple () ,(? 'body))
                      (lambda (body) body))))
                (pattern-rule ;; here
                 `(applic ,(? 'lambda-nil (lambda (e) (lambda-nil-body-get e (lambda () #f)))) ,(? 'args null?))
                 (lambda (lambda-nil args) (remove-applic-lambda-nil (lambda-nil-body-get lambda-nil (lambda () #f))))))

              (pattern-rule
               `(lambda-simple ,(? 'args list?) ,(? 'body))
               (lambda (args body)
                 `(lambda-simple ,args ,(remove-applic-lambda-nil body))))

              (pattern-rule
               `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
               (lambda (args opt-arg body) `(lambda-opt ,args ,opt-arg ,(remove-applic-lambda-nil body))))

              (pattern-rule
               `(lambda-var ,(? 'arg) ,(? 'body))
               (lambda (arg body) `(lambda-var ,arg ,(remove-applic-lambda-nil body))))

              (pattern-rule
               `(applic ,(? 'func) ,(? 'exprs list?))
               (lambda (func exprs) `(applic ,(remove-applic-lambda-nil func) ,(map remove-applic-lambda-nil exprs))))

              (pattern-rule
               `(or ,(? 'args list?))
               (lambda (args) `(or ,(map remove-applic-lambda-nil args))))

              (pattern-rule
               `(set ,(? 'var) ,(? 'val))
               (lambda (var val) `(set ,(remove-applic-lambda-nil var) ,(remove-applic-lambda-nil val))))

              (pattern-rule
               `(seq ,(? 'exprs list?))
               (lambda (exprs) `(seq ,(map remove-applic-lambda-nil exprs))))

              (pattern-rule
               `(box ,(? 'var))
               (lambda (var) `(box ,(remove-applic-lambda-nil var))))

              (pattern-rule
               `(box-get ,(? 'var))
               (lambda (var) `(box-get ,(remove-applic-lambda-nil var))))

              (pattern-rule
               `(box-set ,(? 'var) ,(? 'val))
               (lambda (var val) `(box-set ,(remove-applic-lambda-nil var) ,(remove-applic-lambda-nil val))))

              )))
    (lambda (e)
      (run e (lambda () (error 'remove-applic-lambda-nil (format "I can't recognize this: ~s" e)))))))

;; _________eliminate-nested-def___________________________________
;; ________________________________________________________________

#| from Mayer's class |#
(define <<exract-def>>
  (lambda (pes ret-ds-es)
    (if (null? pes) (ret-ds-es '() '())
        (<<exract-def>>
         (cdr pes)
         (lambda (ds es)
           (cond 	((eq? (caar pes) 'def) (ret-ds-es (cons (car pes) ds) es))
                     ((eq? (caar pes) 'seq)
                      (<<exract-def>> (cadar pes)
                                      (lambda (ds1 es1)
                                        (ret-ds-es (append ds1 ds)
                                         (append es1 es)))))
                     (else (ret-ds-es ds (cons (car pes) es)))))))))

(define ^letrec-statement
  (lambda (args body ^lambda)
    (<<exract-def>> `(,(eliminate-nested-defines body))
                    (lambda (ds es)
                      (if (null? ds)

                          (if (equal? (length es) 1)
                              (^lambda args (car es))
                              (^lambda args `(seq ,es)))

                          ;;                                 def-name
                          (let ((params (map (lambda (def) (cadadr def)) ds))
                                (<dummy-values> (map (lambda (def) `(const #f)) ds))
                                (body `(seq ,(append (map (lambda (def) `(set ,(cadr def) ,(caddr def))) ds) es))))
                            (^lambda args `(applic (lambda-simple ,params ,body) ,<dummy-values>))))
                      ))))

#| almost identical to the template template |#
(define eliminate-nested-defines
  (let ((^lambda-simple (lambda (args body) `(lambda-simple ,args ,body)))
        (^lambda-opt (lambda (args-opt body) `(lambda-opt ,(car args-opt) ,(cdr args-opt) ,body)))
        (^lambda-var (lambda (arg body) `(lambda-var ,arg ,body))))
    (let ((run (compose-patterns
                (pattern-rule
                 `(var ,(? 'var))
                 (lambda (var) `(var ,var)))

                (pattern-rule
                 `(const ,(? 'const))
                 (lambda (const) `(const ,const)))

                (pattern-rule
                 `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
                 (lambda (test dit dif) `(if3 ,(eliminate-nested-defines test) ,(eliminate-nested-defines dit) ,(eliminate-nested-defines dif))))

                (pattern-rule
                 `(def ,(? 'var-name) ,(? 'val))
                 (lambda (var-name val) `(def ,var-name ,(eliminate-nested-defines val))))

                (pattern-rule
                 `(lambda-simple ,(? 'args list?) ,(? 'body))
                 (lambda (args body)
                   (^letrec-statement args body ^lambda-simple)))

                (pattern-rule
                 `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
                 (lambda (args opt-arg body)
                   (^letrec-statement (cons args opt-arg) body ^lambda-opt)))

                (pattern-rule
                 `(lambda-var ,(? 'arg) ,(? 'body))
                 (lambda (arg body)
                   (^letrec-statement arg body ^lambda-var)))

                (pattern-rule
                 `(applic ,(? 'func) ,(? 'exprs list?))
                 (lambda (func exprs) `(applic ,(eliminate-nested-defines func) ,(map eliminate-nested-defines exprs))))

                (pattern-rule
                 `(or ,(? 'args list?))
                 (lambda (args) `(or ,(map eliminate-nested-defines args))))

                (pattern-rule
                 `(set ,(? 'var) ,(? 'val))
                 (lambda (var val) `(set ,(eliminate-nested-defines var) ,(eliminate-nested-defines val))))

                (pattern-rule
                 `(seq ,(? 'exprs list?))
                 (lambda (exprs) `(seq ,(map eliminate-nested-defines exprs))))

                (pattern-rule
                 `(box ,(? 'var))
                 (lambda (var) `(box ,(eliminate-nested-defines var))))

                (pattern-rule
                 `(box-get ,(? 'var))
                 (lambda (var) `(box-get ,(eliminate-nested-defines var))))

                (pattern-rule
                 `(box-set ,(? 'var) ,(? 'val))
                 (lambda (var val) `(box-set ,(eliminate-nested-defines var) ,(eliminate-nested-defines val))))

                )))
      (lambda (e)
        (run e (lambda () (error 'eliminate-nested-defines (format "I can't recognize this: ~s" e))))))))

;; _________pe->lex-pe_____________________________________________
;; ________________________________________________________________

(define get-var-minor-index
  (letrec ((get-var-minor-index-i
            (lambda (v vars i)
              (cond ((null? vars) -1)
                    ((equal? (car vars) v) i)
                    (else (get-var-minor-index-i v (cdr vars) (+ i 1)))))))
    (lambda (v vars) (get-var-minor-index-i v vars 0))))

(define get-var-major-index
  (letrec ((get-var-major-index-i (lambda (v vars i)
                                    (cond ((null? vars) -1)
                                          ((member v (car vars)) i)
                                          (else (get-var-major-index-i v (cdr vars) (+ i 1)))))))
    (lambda (v vars) (get-var-major-index-i v vars 0))))

(define get-element-at-i
  (lambda (s i)
    (cond ((< i 0) (error 'get-element-at-i (format "negative index: s=~s i=~s" s i)))
          ((null? s) (error 'get-element-at-i (format "index out of bounds: s=~s i=~s" s i)))
          ((equal? 0 i) (car s))
          (else (get-element-at-i (cdr s) (- i 1))))))

(define pe->lex-pe
  (let ((run-outer
         (lambda (cont)
           (letrec ((run
                     (lambda (e bvars pvars)
                       ((compose-patterns

                         (pattern-rule
                          `(var ,(? 'v))
                          (lambda (v)
                            (cond ((member v pvars) `(pvar ,v ,(get-var-minor-index v pvars)))
                                  ((let ((belonging-list
                                          (map (lambda (s) (member v s)) bvars)
                                          ))
                                     (ormap (lambda (x) x) belonging-list))
                                   (let* ((major (get-var-major-index v bvars))
                                          (env (get-element-at-i bvars major))
                                          (minor (get-var-minor-index v env)))
                                     `(bvar ,v ,major ,minor)))
                                  (else `(fvar ,v)))))

                         (pattern-rule
                          `(const ,(? 'const))
                          (lambda (const) `(const ,const)))

                         (pattern-rule
                          `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
                          (lambda (test dit dif) `(if3 ,(run test bvars pvars) ,(run dit bvars pvars) ,(run dif bvars pvars))))

                         (pattern-rule
                          `(def ,(? 'var-name) ,(? 'val))
                          (lambda (var-name val) `(def ,(run var-name bvars pvars) ,(run val bvars pvars))))

                         (pattern-rule
                          `(lambda-simple ,(? 'args list?) ,(? 'body))
                          (lambda (args body)
                            (let ((bvars `(,pvars ,@bvars)) (pvars args))
                              `(lambda-simple ,args ,(run body bvars pvars)))))

                         (pattern-rule
                          `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
                          (lambda (args opt-arg body)
                            (let ((bvars `(,pvars ,@bvars)) (pvars `(,@args ,opt-arg)))
                              `(lambda-opt ,args ,opt-arg ,(run body bvars pvars)))))

                         (pattern-rule
                          `(lambda-var ,(? 'arg) ,(? 'body))
                          (lambda (arg body)
                            (let ((bvars `(,pvars ,@bvars)) (pvars `(,arg)))
                              `(lambda-var ,arg ,(run body bvars pvars)))))

                         (pattern-rule
                          `(applic ,(? 'func) ,(? 'exprs list?))
                          (lambda (func exprs) `(applic ,(run func bvars pvars) ,(map (lambda (e) (run e bvars pvars)) exprs))))

                         (pattern-rule
                          `(or ,(? 'args list?))
                          (lambda (args) `(or ,(map (lambda (e) (run e bvars pvars)) args))))

                         (pattern-rule
                          `(set ,(? 'var) ,(? 'val))
                          (lambda (var val) `(set ,(run var bvars pvars) ,(run val bvars pvars))))

                         (pattern-rule
                          `(seq ,(? 'exprs list?))
                          (lambda (exprs) `(seq ,(map (lambda (e) (run e bvars pvars)) exprs))))

                         (pattern-rule
                          `(box ,(? 'var))
                          (lambda (var) `(box ,(run var bvars pvars))))

                         (pattern-rule
                          `(box-get ,(? 'var))
                          (lambda (var) `(box-get ,(run var bvars pvars))))

                         (pattern-rule
                          `(box-set ,(? 'var) ,(? 'val))
                          (lambda (var val) `(box-set ,(run var bvars pvars) ,(run val bvars pvars))))) e cont))))
             run))))
    (lambda (e)
      ((run-outer (lambda () (error 'pe->lex-pe (format "I can't recognize this: ~s" e)))) e '() '()))))

;; _________annotate-tc____________________________________________
;; ________________________________________________________________

(define special-map
  (lambda (f1 f2 s)
    (cond ((null? s) s)
          ((null? (cdr s)) (cons (f2 (car s)) '()))
          (else (cons (f1 (car s)) (special-map f1 f2 (cdr s)))))))

(define is-seq?
  (lambda (e)
    ((pattern-rule
      `(seq ,(? 'exprs list?))
      (lambda (exprs) #t)) e (lambda () #f))))

(define is-not-seq? (lambda (e) (not (is-seq? e))))

(define annotate-tc
  (letrec ((run-outer
            (lambda (cont)
              (letrec ((run-inner
                        (lambda (should-annotate?)
                          (lambda (e)
                            ((compose-patterns
                              (pattern-rule
                               `(var ,(? 'var))
                               (lambda (var) `(var ,var)))

                              (pattern-rule
                               `(fvar ,(? 'var))
                               (lambda (var) `(fvar ,var)))

                              (pattern-rule
                               `(pvar ,(? 'var) ,(? 'minor))
                               (lambda (var minor) `(pvar ,var ,minor)))

                              (pattern-rule
                               `(bvar ,(? 'var) ,(? 'major) ,(? 'minor))
                               (lambda (var major minor) `(bvar ,var ,major ,minor)))

                              (pattern-rule
                               `(const ,(? 'const))
                               (lambda (const) `(const ,const)))

                              (pattern-rule
                               `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
                               (lambda (test dit dif) `(if3 ,((run-inner #f) test) ,((run-inner should-annotate?) dit) ,((run-inner should-annotate?) dif))))

                              (pattern-rule
                               `(def ,(? 'var-name) ,(? 'val))
                               (lambda (var-name val) `(def ,var-name ,((run-inner #f) val))))

                              (pattern-rule
                               `(lambda-simple ,(? 'args list?) ,(? 'body))
                               (lambda (args body)
                                 `(lambda-simple ,args ,((run-inner #t) body))))

                              (pattern-rule
                               `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
                               (lambda (args opt-arg body) `(lambda-opt ,args ,opt-arg ,((run-inner #t) body))))

                              (pattern-rule
                               `(lambda-var ,(? 'arg) ,(? 'body))
                               (lambda (arg body) `(lambda-var ,arg ,((run-inner #t) body))))

                              (pattern-rule
                               `(applic ,(? 'func) ,(? 'exprs list?))
                               (lambda (func exprs)
                                 (if should-annotate?
                                     `(tc-applic ,((run-inner #f) func) ,(map (run-inner #f) exprs))
                                     `(applic ,((run-inner #f) func) ,(map (run-inner #f) exprs)))))

                              (pattern-rule
                               `(or ,(? 'args list?))
                               (lambda (args) `(or ,(special-map (run-inner #f) (run-inner should-annotate?) args))))

                              (pattern-rule
                               `(set ,(? 'var) ,(? 'val))
                               (lambda (var val) `(set ,((run-inner #f) var) ,((run-inner #f) val))))

                              (pattern-rule
                               `(seq ,(? 'exprs list?))
                               (lambda (exprs) `(seq ,(special-map (run-inner #f) (run-inner should-annotate?) exprs))))

                              (pattern-rule
                               `(box ,(? 'var))
                               (lambda (var) `(box ,((run-inner #f) var))))

                              (pattern-rule
                               `(box-get ,(? 'var))
                               (lambda (var) `(box-get ,((run-inner #f) var))))

                              (pattern-rule
                               `(box-set ,(? 'var) ,(? 'val))
                               (lambda (var val) `(box-set ,((run-inner #f) var) ,((run-inner #f) val))))) e cont)))))
                run-inner))))
    (lambda (e)
      (((run-outer (lambda () (error 'pe->lex-pe (format "I can't recognize this: ~s" e)))) #f) e))))


;; _________box-set________________________________________________
;; ________________________________________________________________

;;;                   SET GET BOUND
(define empty-usage `(,#f ,#f ,#f))
(define usage-or
  (lambda (usage1 usage2)
    (let ((set-flag (or (car usage1) (car usage2)))
          (get-flag (or (cadr usage1) (cadr usage2)))
          (bound-flag (or (caddr usage1) (caddr usage2))))
      `(,set-flag ,get-flag ,bound-flag))))

(define usage-add-set
  (lambda (usage)
    (usage-or usage `(,#t #f #f))))

(define usage-add-get
  (lambda (usage)
    (usage-or usage `(,#f #t #f))))

(define usage-add-bound
  (lambda (usage)
    (usage-or usage `(,#f #f #t))))

(define accumulate-usage
  (lambda (x y) (usage-or x y)))



(define check-var-usage
  (let ((run-outer
         (lambda (<v> cont)
           (letrec ((run
                     (lambda (is-bound? e)
                       ((compose-patterns
                         (pattern-rule
                          `(var ,(? 'var))
                          (lambda (var)
                            (if (equal? <v> var)
                                `(,#f ,#t ,is-bound?)
                                empty-usage)))

                         (pattern-rule
                          `(const ,(? 'const))
                          (lambda (const) empty-usage))

                         (pattern-rule
                          `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
                          (lambda (test dit dif)
                            (let ((res-test (run is-bound? test))
                                  (res-dif (run is-bound? dif))
                                  (res-dit (run is-bound? dit)))
                              (accumulate-usage res-test (accumulate-usage res-dif res-dit)))))

                         (pattern-rule
                          `(def ,(? 'var-name) ,(? 'val))
                          (lambda (var-name val)
                            (run is-bound? val)))

                         (pattern-rule
                          `(lambda-simple ,(? 'args list?) ,(? 'body))
                          (lambda (args body)
                            (if (member <v> args)
                                empty-usage
                                (run #t body))))

                         (pattern-rule
                          `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
                          (lambda (args opt-arg body)
                            (if (or (member <v> args) (equal? <v> opt-arg))
                                empty-usage
                                (run #t body))))

                         (pattern-rule
                          `(lambda-var ,(? 'arg) ,(? 'body))
                          (lambda (arg body)
                            (if (equal? <v> arg)
                                empty-usage
                                (run #t body))))

                         (pattern-rule
                          `(applic ,(? 'func) ,(? 'exprs list?))
                          (lambda (func exprs)
                            (let ((results `(,(run is-bound? func) ,@(map (lambda (x) (run is-bound? x)) exprs))))
                                        ;(let ((results (map (lambda (x) (validate-candidate <v> x)) results)))
                              (fold-left accumulate-usage empty-usage results))))

                         (pattern-rule
                          `(or ,(? 'args list?))
                          (lambda (args)
                            (let ((results (map (lambda (x) (run is-bound? x)) args)))
                                        ;(let ((results (map (lambda (x) (validate-candidate <v> x)) results)))
                              (fold-left accumulate-usage empty-usage results))))

                         (pattern-rule
                          `(set ,(? 'var) ,(? 'val))
                          (lambda (var val)
                            (let ((set-usage (if (equal? (cadr var) <v>) `(,#t ,#f ,is-bound?) empty-usage)))
                              (usage-or set-usage (run is-bound? val)))))

                         (pattern-rule
                          `(seq ,(? 'exprs list?))
                          (lambda (exprs)
                            (let ((results (map (lambda (x) (run is-bound? x)) exprs)))
                                        ;(let ((results (map (lambda (x) (validate-candidate <v> x)) results)))
                              (fold-left accumulate-usage empty-usage results))))

                         (pattern-rule
                          `(box ,(? 'var))
                          (lambda (var)
                            empty-usage))

                         (pattern-rule
                          `(box-get ,(? 'var))
                          (lambda (var)
                            empty-usage))

                         (pattern-rule
                          `(box-set ,(? 'var) ,(? 'val))
                          (lambda (var val)
                            (run is-bound? val)))

                         ) e cont))))
             run))))

    (lambda (<v> e)
      ((run-outer <v> (lambda () (error 'check-var-usage (format "I can't recognize this: ~s" e)))) #f e))))

(define to-box? (lambda (<v> body) (andmap (lambda (b) b) (check-var-usage <v> body))))

(define apply-boxing-to-var
  (lambda (<v>)
    (let ((fail-cont (lambda () (error 'apply-boxing-to-var (format "I can't recognize this: ~s" e)))))
      (letrec ((run
                (lambda (e)
                  ((compose-patterns

                    (pattern-rule
                     `(var ,(? 'var))
                     (lambda (var)
                       (if (equal? <v> var)
                           `(box-get (var ,var))
                           `(var ,var))))

                    (pattern-rule
                     `(const ,(? 'const))
                     (lambda (const) `(const ,const)))

                    (pattern-rule
                     `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
                     (lambda (test dit dif) `(if3 ,(run test) ,(run dit) ,(run dif))))

                    (pattern-rule
                     `(def ,(? 'var-name) ,(? 'val))
                     (lambda (var-name val) `(def ,var-name ,(run val))))

                    (pattern-rule
                     `(lambda-simple ,(? 'args list?) ,(? 'body))
                     (lambda (args body)
                       (if (member <v> args)
                           `(lambda-simple ,args ,body)
                           `(lambda-simple ,args ,(run body)))))

                    (pattern-rule
                     `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
                     (lambda (args opt-arg body)
                       (if (or (member <v> args) (equal? <v> opt-arg))
                           `(lambda-opt ,args ,opt-arg ,body)
                           `(lambda-opt ,args ,opt-arg ,(run body)))))

                    (pattern-rule
                     `(lambda-var ,(? 'arg) ,(? 'body))
                     (lambda (arg body)
                       (if (equal? <v> arg)
                           `(lambda-var ,arg ,body)
                           `(lambda-var ,arg ,(run body)))))

                    (pattern-rule
                     `(applic ,(? 'func) ,(? 'exprs list?))
                     (lambda (func exprs) `(applic ,(run func) ,(map run exprs))))

                    (pattern-rule
                     `(or ,(? 'args list?))
                     (lambda (args) `(or ,(map run args))))

                    (pattern-rule
                     `(set ,(? 'var) ,(? 'val))
                     (lambda (var val)
                       (let ((val (run val)))
                         (if (equal? <v> (cadr var))
                             `(box-set ,var ,val)
                             `(set ,var ,val))))) ; ,(run var) ?

                    (pattern-rule
                     `(seq ,(? 'exprs list?))
                     (lambda (exprs) `(seq ,(map run exprs))))

                    (pattern-rule
                     `(box ,(? 'var))
                     (lambda (var) `(box ,(run var))))

                    (pattern-rule
                     `(box-get ,(? 'var))
                     (lambda (var) `(box-get ,(run var))))

                    (pattern-rule
                     `(box-set ,(? 'var) ,(? 'val))
                     (lambda (var val) `(box-set ,(run var) ,(run val))))) e fail-cont))))
        (lambda (e)
          (run e))))))

(define box-set
  (let ((run (compose-patterns
              (pattern-rule
               `(var ,(? 'var))
               (lambda (var) `(var ,var)))

              (pattern-rule
               `(const ,(? 'const))
               (lambda (const) `(const ,const)))

              (pattern-rule
               `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
               (lambda (test dit dif) `(if3 ,(box-set test) ,(box-set dit) ,(box-set dif))))

              (pattern-rule
               `(def ,(? 'var-name) ,(? 'val))
               (lambda (var-name val) `(def ,var-name ,(box-set val))))

              (let ((^<args-usage-statuses> (lambda (args body) (map (lambda (var) (cons var (to-box? var body))) args)))
                    (^<body-boxed>
                     (lambda (body <args-usage-statuses>) (fold-left
                                                           (lambda (acc-body arg-status)
                                                             (if (cdr arg-status)
                                                                 ((apply-boxing-to-var (car arg-status)) acc-body)
                                                                 acc-body))
                                                           body
                                                           <args-usage-statuses>)))
                    (^<setters-seq>
                     (lambda (<args-usage-statuses>) (fold-left
                                                      (lambda (acc setter) (if (null? setter) acc `(,@acc ,setter)))
                                                      '()
                                                      (map (lambda (arg-status)
                                                             (let ((arg (car arg-status)) (status (cdr arg-status)))
                                                               (if status `(set (var ,arg) (box (var ,arg))) '())))
                                                           <args-usage-statuses>)))))
                (compose-patterns

                 (pattern-rule
                  `(lambda-simple ,(? 'args list?) ,(? 'body))
                  (lambda (args body)
                    (let* ((<args-usage-statuses> (^<args-usage-statuses> args body))
                           (body (^<body-boxed> body <args-usage-statuses>))
                           (setters-seq (^<setters-seq> <args-usage-statuses>)))
                      (if (or (null? setters-seq) (null? (car setters-seq)))
                          `(lambda-simple ,args ,(box-set body))
                          (if (equal? (car body) 'seq)
                              `(lambda-simple ,args (seq ,(append setters-seq (flatten-list (cdr (box-set body))))))
                              `(lambda-simple ,args (seq ,(append setters-seq `(,(box-set body))))))))))

                 (pattern-rule
                  `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
                  (lambda (args opt-arg body)
                    (let* ((<args-usage-statuses> (^<args-usage-statuses> `(opt-arg ,@args) body))
                           (body (^<body-boxed> body <args-usage-statuses>))
                           (setters-seq (^<setters-seq> <args-usage-statuses>)))
                      (if (or (null? setters-seq) (null? (car setters-seq)))
                          `(lambda-opt ,args ,opt-arg ,(box-set body))
                          (if (equal? (car body) 'seq)
                              `(lambda-opt ,args ,opt-arg (seq ,(append setters-seq (flatten-list (cdr (box-set body))))))
                              `(lambda-opt ,args ,opt-arg (seq ,(append setters-seq `(,(box-set body))))))))))

                 (pattern-rule
                  `(lambda-var ,(? 'arg) ,(? 'body))
                  (lambda (arg body)
                    (let* ((<args-usage-statuses> (^<args-usage-statuses> `(,arg) body))
                           (body (^<body-boxed> body <args-usage-statuses>))
                           (setters-seq (^<setters-seq> <args-usage-statuses>)))
                      (if (or (null? setters-seq) (null? (car setters-seq)))
                          `(lambda-var ,arg ,(box-set body))
                          (if (equal? (car body) 'seq)
                              `(lambda-var ,arg (seq ,(append setters-seq (flatten-list (cdr (box-set body))))))
                              `(lambda-var ,arg (seq ,(append setters-seq `(,(box-set body))))))))))))

              (pattern-rule
               `(applic ,(? 'func) ,(? 'exprs list?))
               (lambda (func exprs) `(applic ,(box-set func) ,(map box-set exprs))))

              (pattern-rule
               `(or ,(? 'args list?))
               (lambda (args) `(or ,(map box-set args))))

              (pattern-rule
               `(set ,(? 'var) ,(? 'val))
               (lambda (var val) `(set ,(box-set var) ,(box-set val))))

              (pattern-rule
               `(seq ,(? 'exprs list?))
               (lambda (exprs) `(seq ,(map box-set exprs))))

              (pattern-rule
               `(box ,(? 'var))
               (lambda (var) `(box ,(box-set var))))

              (pattern-rule
               `(box-get ,(? 'var))
               (lambda (var) `(box-get ,(box-set var))))

              (pattern-rule
               `(box-set ,(? 'var) ,(? 'val))
               (lambda (var val) `(box-set ,(box-set var) ,(box-set val))))
              )))
    (lambda (e)
      (run e (lambda () (error 'box-set (format "I can't recognize this: ~s" e)))))))


;;
;; *************************************************************************************************************************************
;; _________tools and constants____________________________________
;; ________________________________________________________________ 
(define id (lambda (x) x))

(define file->string
  (lambda (in-file)
    (let ((in-port (open-input-file in-file)))
      (letrec ((run
                (lambda ()
                  (let ((ch (read-char in-port)))
                    (if (eof-object? ch)
                        (begin
                          (close-input-port in-port)
                          '())
                        (cons ch (run)))))))
        (list->string
         (run))))))

(define set-subtraction
  (lambda (s1 s2)
    (fold-left (lambda (acc el) (append acc (if (member el s2) '() `(,el)))) '() s1)))
(define set-union
  (lambda (s1 s2)
    (append s1 (subtraction s2 s1))))
(define list->set
  (lambda (s)
    (fold-left
     (lambda (acc el)
       (if (member el acc)
           acc
           `(,@acc ,el)))
     '()
     s)))

(define label-generator
  (lambda (label) ;; label must be string! i.e. "my_label"
    (let ((counter -1))
      (lambda ()
        (set! counter (+ counter 1))
        (string-append label (number->string counter))))))

(define ^counter
  (lambda ()
    (let ((counter -1))
      (lambda () (set! counter (+ 1 counter)) counter))))

(define vector-get-element-index
  (let ((get-element-index-v-el-i
         (lambda (vector el)
           (letrec ((get-element-index-i
                     (lambda (i)
                       (if (equal? (vector-ref vector i) el) i (get-element-index-i (+ i 1))))))
             get-element-index-i))))
    (lambda (vector el)
      ((get-element-index-v-el-i vector el) 0))))

(define vector-fold-left
  (lambda (f acc vector)
    (fold-left f acc (vector->list vector))))

(define vector-map
  (letrec ((iterator
            (lambda (vector-in f i length vector-out)
              (if (< i length)
                  (begin (vector-set! vector-out i (f (vector-ref vector-in i))) (iterator vector-in f (+ i 1) length vector-out))
                  vector-out))))
    (lambda (f vector)
      (let ((length (vector-length vector)))
        (iterator vector f 0 length (make-vector length))))))

#|(define string-append-list
  (lambda (list)
    (fold-left string-append "" list)))|#


(load "cisc-lib.scm")

;; _________code-gen_______________________________________________
;; ________________________________________________________________ 

(define stack-fix
  (lambda (num-of-args)
    (let ((accumulator R6)
          (actual-num-of-args R5)
          (list-stack-index (number->string (+ 2 num-of-args)))
          (num-of-args (number->string num-of-args)))
      (nl-string-append
       (>mov actual-num-of-args (>fparg 1))
       (>mov accumulator sob-nil)
       (>for-loop actual-num-of-args
                  num-of-args
                  >dec
                  >jle
                  (>mov-res accumulator (>cons accumulator (>fparg-nan (base+displ loop-counter "1")))))
       (>mov (>fparg-nan list-stack-index) accumulator)))))

(define lambda-code-gen
  (lambda (stack-fix body-label exit-label num-of-params body major code-gen)

    (nl-string-append
     (>comment (format "lambda code-gen, body: ~s" body))

     (>mov-res R2
               (>malloc (number->string (+ 1 major)))) ; R2 <- new env (empty)
     (>mov R1 (>fparg 0))                              ; R1 <- old env

; copy old enviroment to new enviroment
     (>comment (format "copy old enviroment to new enviroment:"))
     ;; for ( i = 0; i < major; i++) { body... }
     (>for-loop "0"
                (number->string major)
                >inc
                >jge
                (>mov (>indd R2 (string-append loop-counter " + 1"))
                      (>indd R1 loop-counter)))

                                        ; allocate space for args in the enviroment
     (>comment (format "allocate space for args in the enviroment:"))
     (>mov R3 (>fparg 1))
     (>mov-res (>indd R2 "0") (>malloc R3))

                                        ; copy arguments to new enviroment
     (>comment (format "copy arguments to new enviroment:"))
     (>mov R0 (>indd R2 "0"))
     (>for-loop "0"
                R3
                >inc
                >jge
                (>mov (>indd R0 loop-counter)
                      (>fparg-nan (string-append loop-counter " + 1"))))

                                        ; create lambda_sob object
     (>comment (format "create lambda_sob object:"))
     (>mov-res R0 (>malloc "3"))
     (>mov (>indd R0 "0") t_closure)               ; t_closure
     (>mov (>indd R0 "1") R2)                      ; env
     (>mov (>indd R0 "2") (>get-label body-label)) ; code-pointer
     (>jmp exit-label)


     (>make-label body-label)
     (>push fp)
     (>mov fp sp)


     (>comment (format "stack-fix:"))
     stack-fix ;; <------- stack fix for lambda-opt and lambda-var
     (>comment (format "end of stack-fix"))

                                        ; argument number check
                                        ;(>cmp (>fparg 1) (>imm num-of-params))
                                        ;(>jne "L_error_lambda_args_count")



     (code-gen (+ 1 major) body)
     (>mov sp fp)
     (>pop fp)
     (>ret)
     (>make-label exit-label)
     (>comment (format "end of lambda."))
     )))

;; TODO:
(define code-gen
  (let ((if3-else (label-generator "if3_else_"))
        (if3-exit (label-generator "if3_exit_"))
        (or-exit (label-generator "or_exit_"))
        (lambda-body (label-generator "closure_body_"))
        (lambda-exit (label-generator "closure_exit_")))

    (let ((^run
           (lambda (fvars indexed-consts)
             (lambda (cont)
               (letrec ((code-gen
                         (lambda (major e)
                           ((compose-patterns
                             
                             (pattern-rule
                              `(const ,(? 'const))
                              (lambda (const)
                                (let ((index (get-const-offset indexed-consts const)))
                                  (string-append (>comment (format "const ~s" const))
                                                 nl
                                                 (>mov r0 (>imm (base+displ CONST_TABLE_BASE_ADDR (number->string index))))
                                                 ))))

                                        ; TODO:
                             (pattern-rule
                              `(fvar ,(? 'v))
                              (lambda (v) ""))

                             (pattern-rule
                              `(pvar ,(? 'v) ,(? 'minor))
                              (lambda (v minor) (nl-string-append (>comment (format "pvar ~s ~s" v minor))
                                                                  (>mov r0 (>fparg-displ 2 minor)))))

                             (pattern-rule
                              `(bvar ,(? 'v) ,(? 'major) ,(? 'minor))
                              (lambda (v major minor)
                                (nl-string-append
                                 (>comment (format "bvar ~s ~s ~s" v major minor))
                                 (>mov r0 (>fparg 0))
                                 (>mov r0 (>indd r0 major))
                                 (>mov r0 (>indd r0 minor)))))

                             (pattern-rule
                              `(if3 ,(? 'test) ,(? 'dit) ,(? 'dif))
                              (lambda (test dit dif)
                                (let ((else_label (if3-else))
                                      (exit_label (if3-exit)))
                                  (nl-string-append (>comment (format "if3 ~s ~s ~s" test dit dif))
                                                    ""
                                                    (code-gen major test)
                                                    (>cmp r0 (>imm sob-false))
                                                    (>jmp exit_label)
                                                    (code-gen major dit)
                                                    (>jmp exit_label)
                                                    (>make-label else_label)
                                                    (code-gen major dif)
                                                    (>make-label exit_label)
                                                    ))))

                                        ; TODO:
                             (pattern-rule
                              `(def ,(? 'var-name) ,(? 'val))
                              (lambda (var-name val) ""))

                                        ; TODO: test
                             (pattern-rule
                              `(lambda-simple ,(? 'args list?) ,(? 'body))
                              (lambda (args body)
                                (let ((num-of-params (number->string (length args)))
                                      (body-label (lambda-body))
                                      (exit-label (lambda-exit)))
                                  (lambda-code-gen ""
                                                   body-label
                                                   exit-label
                                                   num-of-params
                                                   body
                                                   major
                                                   code-gen))))


                                        ; TODO:
                             (pattern-rule
                              `(lambda-opt ,(? 'args list?) ,(? 'opt-arg) ,(? 'body))
                              (lambda (args opt-arg body)
                                (let ((num-of-params (length args))
                                      (body-label (lambda-body))
                                      (exit-label (lambda-exit)))
                                  (lambda-code-gen (stack-fix num-of-params)
                                                   body-label
                                                   exit-label
                                                   num-of-params
                                                   body
                                                   major
                                                   code-gen))))


                                        ; TODO:
                             (pattern-rule
                              `(lambda-var ,(? 'arg) ,(? 'body))
                              (lambda (arg body)
                                (let ((num-of-params 0)
                                      (body-label (lambda-body))
                                      (exit-label (lambda-exit)))
                                  (lambda-code-gen (stack-fix num-of-params)
                                                   body-label
                                                   exit-label
                                                   num-of-params
                                                   body
                                                   major
                                                   code-gen))))

                             (pattern-rule ;; TODO: test; error check?
                              `(applic ,(? 'func) ,(? 'exprs list?))
                              (lambda (func exprs)
                                (let ((num-of-args (number->string (length exprs)))
                                      (exprs (reverse exprs)))
                                  (nl-string-append (>comment (format "applic ~s ~s" func (reverse exprs)))
                                                    ;(>push "0") ;; for var and opt support
                                                    (string-append-list
                                                     (map (lambda (e) (nl-string-append (code-gen major e)
                                                                                        (>push R0)))
                                                          exprs))
                                                    (>push num-of-args)
                                                    (code-gen major func)

                                                    #| TYPE CHECK:
                                                    (>cmp (>indd R0 0) (>imm t_closure))
                                                    (>jne "L_error_cannot_apply_non_clos")
                                                    |#

                                                    (>push (>indd R0 "1"))
                                                    (>calla (>indd R0 "2"))
                                                    (>drop "1")

                                                    (>pop R1)
                                                    (>drop R1)
                                                    ;(>drop "1") ;; for the (>push "0") earlier
                                                    ))))

                                        ; TODO:
                             (pattern-rule
                              `(tc-applic ,(? 'func) ,(? 'exprs list?))
                              (lambda (func exprs) ""))

                             (pattern-rule
                              `(or ,(? 'args list?))
                              (lambda (args)
                                (let ((exit-label (or-exit)))
                                  (string-append (>comment (format "or ~s" args))
                                                 nl
                                                 (string-append-list (special-map (lambda (e) (nl-string-append (code-gen major e)
                                                                                                                (>cmp r0 (>imm sob-false))
                                                                                                                (>jne exit-label)))
                                                                                  (lambda (e) (>nl (code-gen major e)))
                                                                                  args))
                                                 exit-label ":"))))

                                        ; TODO: test
                             (pattern-rule
                              `(set ,(? 'var) ,(? 'val))
                              (lambda (var val)
                                (let ((r0->fparg ((pattern-rule
                                                   `(pvar ,(? 'v) ,(? 'minor))
                                                   (lambda (v minor) (>mov (>fparg-displ 2 minor) r0))) var (lambda () (error 'code-gen (format "set: unrecognized pvar: ~s" var))))))
                                  (string-append (>comment (format "set ~s ~s" var val))
                                                 nl
                                                 r0->fparg
                                                 (>mov r0 (>imm sob-void))
                                                 nl))))

                             (pattern-rule
                              `(seq ,(? 'exprs list?))
                              (lambda (exprs)
                                (string-append-list (map (lambda (expr) (>nl (code-gen major expr))) exprs)))) ;; TODO test

; TODO:
                             (pattern-rule
                              `(box ,(? 'var))
                              (lambda (var) ""))

                                        ; TODO: test
                             (pattern-rule
                              `(box-get ,(? 'var))
                              (lambda (var)
                                (string-append (>comment (format "box-get ~s" var))
                                               nl
                                               (code-gen major var)
                                               (>mov r0 (>ind r0)))))

                                        ; TODO:
                             (pattern-rule
                              `(box-set ,(? 'var) ,(? 'val))
                              (lambda (var val) ""))

                             ) e cont))))
                 code-gen)))))

      (lambda (e fvars indexed-consts)
        (((^run fvars indexed-consts) (lambda () (error 'code-gen (format "I can't recognize this: ~s" e)))) 0 e)))))





;; _________construct-tables_______________________________________
;; ________________________________________________________________
(load "tdd-tools.scm")

(define disassemble-const
  (lambda (c)
    (cond ((null? c) c)
          ((not (list? c)) `(,c))
          (else (let ((first (car c))
                      (rest (cdr c)))
                  `(,@(disassemble-const rest) ,@(disassemble-const first) ,c))))))

(define construct-tables
  (letrec ((^construct-sequence-tables
            (lambda (construct-tables-inner)
              (letrec ((construct-sequence-tables
                        (lambda (es fvars consts cont)
                          (if (null? es)
                              (cont fvars consts)
                              (construct-tables-inner (car es)
                               fvars
                               consts
                               (lambda (fvars consts)
                                 (construct-sequence-tables
                                  (cdr es)
                                  fvars
                                  consts
                                  cont)))))))
                construct-sequence-tables))))
    (letrec ((construct-tables-inner
              (lambda (e fvars consts cont)
                (letrec ((construct-sequence-tables (^construct-sequence-tables construct-tables-inner)))
                  (if (null? e)
                      (cont fvars consts)
                      (if (not (list? e))
                          (error 'construct-tables-inner (format "e is not a list: ~s" e))
                          (let ((p-name (car e)))
                            (cond ((equal? 'const p-name)
                                   (cont fvars `(,@consts ,@(sort
                                                             (lambda (e1 e2) (cond ((and (list? e1)
                                                                                         (list? e2))
                                                                                    (< (length e1) (length e2)))
                                                                                   ((list? e1) #f)
                                                                                   ((list? e2) #t)
                                                                                   (> e1 e2)))
                                                             (disassemble-const (cadr e))))))

                                  ((equal? 'fvar p-name) (cont `(,@fvars ,(cadr e)) consts))
                            
                                  ((or (equal? 'pvar p-name)
                                       (equal? 'bvar p-name)
                                       (equal? 'box p-name)
                                       (equal? 'box-get p-name))
                                   (cont fvars consts))

                                  ((equal? 'if3 p-name)
                                   (let ((<test> (cadr e))
                                         (<dit> (caddr e))
                                         (<dif> (cadddr e)))
                                     (construct-sequence-tables `(,<test> ,<dit> ,<dif>) fvars consts cont)))

                                  ((equal? 'def p-name) (construct-tables-inner (cadr e) fvars consts cont))

                                  ((equal? 'lambda-simple p-name) (construct-tables-inner (caddr e) fvars consts cont))
                                  ((equal? 'lambda-opt p-name) (construct-tables-inner (cadddr e) fvars consts cont))
                                  ((equal? 'lambda-var p-name) (construct-tables-inner (caddr e) fvars consts cont))

                                  ((or (equal? 'applic p-name)
                                       (equal? 'tc-applic p-name))
                                   (let ((<func> (cadr e))
                                         (<args> (caddr e)))
                                     (construct-sequence-tables `(,<func> ,@<args>) fvars consts cont)))

                                  ((or (equal? 'seq p-name) (equal? 'or p-name)) (construct-sequence-tables (cadr e) fvars consts cont))

                                  ((equal? 'set p-name)
                                   (let ((<var> (cadr e))
                                         (<val> (caddr e)))
                                     (construct-sequence-tables `(,<var> ,<val>) fvars consts cont)))

                                  ((equal? 'box-set p-name) (construct-tables-inner (caddr e) fvars consts cont))

                                  (else (error 'construct-tables-inner (format "unrecognized pattern: ~s" e)))))))))))

      (let ((<initial-fvars-table> '())
            (<initial-const-table> `(,(void) () ,#t ,#f)))

        (lambda (e) (construct-tables-inner e <initial-fvars-table> <initial-const-table> (lambda (fvars consts) `(,fvars ,consts))))))))


;; _________compile-scheme-file____________________________________
;; ________________________________________________________________ 

(define list->sexpr
  (lambda (list fail-cont)
    (<sexpr> list
             (lambda (e s)
               (if (null? s) `(,e) `(,e ,@(list->sexpr s fail-cont))))
             fail-cont)))

(define string->sexpr
  (lambda (string fail-cont)
    (list->sexpr (string->list string) fail-cont)))

(define get-tables
  (lambda (pes)
    (fold-left
     (lambda (acc e)
       (let ((tables (construct-tables e)))
         `(,(list->set (append (car acc) (car tables)))
           ,(list->set (append (cadr acc) (cadr tables))))))
     `(() ())
     pes)))

(define get-const-offset
              (let ((get-offset car) (get-const cdr))
                (lambda (indexed-table const)
                  (cond ((null? indexed-table) "ERROR_CONST_NOT_FOUND_IN_TABLE (encode-const-table)")
                        ((eq? (get-const (car indexed-table)) const)
                         (begin 
                           ;(display-colored-BIG (format "const ~s is at ~s" const (get-offset (car indexed-table))))
                         (get-offset (car indexed-table))))
                        (else (get-const-offset (cdr indexed-table) const))))))

(define create-const-table-indexes
  (lambda(table)
    (let ((offset 0))
      (vector-map (lambda (const)
                    (let ((old-offset offset))
                      (set! offset (+ offset (cond ((equal? const (void)) 1)
                                                   ((null? const)         1)
                                                   ((boolean? const)      2)
                                                   ((char? const)         2)
                                                   ((integer? const)      2)
                                                   ((pair? const)         3)
                                                   ((string? const) (+ 2 (string-length const)))
                                                   ((symbol? const) '???)
                                                   ((vector? const) (+ 2 (vector-length const)))
                                                   ((procedure? const)    3)
                                                   (else (error 'encode-const-table_offset: (format "cant decide type of argument: ~s" const))))))
                      (cons old-offset const)))
                  table))))

(define encode-const-table
  (letrec ((map (lambda (f x) (if (null? x) x (cons (f (car x)) (map f (cdr x)))))))
  (lambda (base-addr table indexed-table)
    (letrec ((counter (^counter))
             (encode
              (lambda (val)
                (>nl (>mov (>indd base-addr (number->string (counter))) val)))))
      (string-append-list (map (lambda (const)
                                 (cond ((equal? const (void)) (encode t_void))

                                       ((null? const) (encode t_nil))

                                       ((boolean? const) (nl-string-append (encode (>imm (number->string (if const 1 0))))
                                                                           (encode t_bool)))

                                       ((char? const) (nl-string-append (encode (string-append "'" (list->string `(,const)) "'"))
                                                                        (encode t_char)))

                                       ((integer? const)
                                        (nl-string-append (encode (>imm (number->string const)))
                                                          (encode t_integer)))

                                       ((pair? const) (let ((car-index (number->string (get-const-offset indexed-table (car const))))
                                                            (cdr-index (number->string (get-const-offset indexed-table (cdr const)))))
                                                        (nl-string-append (encode (>imm (number->string cdr-index)))
                                                                          (encode (>imm (number->string car-index)))
                                                                          (encode t_pair))))

                                       ((string? const)
                                        (nl-string-append (string-append-list
                                                           (map (lambda (char)
                                                                  (>nl (encode (list->string (list #\' char #\')))))
                                                                (string->list const)))
                                                          (encode (>imm (number->string (string-length const))))
                                                          (encode t_string)))

                                       ((symbol? const) "")
                                       ((vector? const) "")
                                       ((procedure? const) "")
                                       (else (error 'encode-const-table: (format "cant decide type of argument: ~s" const)))))
                               (vector->list table)))))))






;;;; generates code for the constants: sob-void sob-nil sob-true sob-false
;; 
(define generate-constants-macro
    (lambda (indexed-consts)
      (nl-string-append (>define sob-void  (>imm (base+displ CONST_TABLE_BASE_ADDR (number->string (get-const-offset indexed-consts (void))))))
                        (>define sob-nil   (>imm (base+displ CONST_TABLE_BASE_ADDR (number->string (get-const-offset indexed-consts '())))))   
                        (>define sob-true  (>imm (base+displ CONST_TABLE_BASE_ADDR (number->string (get-const-offset indexed-consts #t)))))    
                        (>define sob-false (>imm (base+displ CONST_TABLE_BASE_ADDR (number->string (get-const-offset indexed-consts #f)))))))) 


;;;; generates print lines
;; 
(define make-print
  (let ((prologue "\n") (epilogue (nl-string-append ; "INFO"
                                                    ; "SHOW(\"SP\", SP);"
                                                    (>push r0)
                                                    (>call "WRITE_SOB")
                                                    (>drop "1")
                                                    (>call "NEWLINE"))))
    (lambda (code)
      (if (equal? code "") code (string-append prologue code epilogue)))))



;;;; final interface for compiling files from Scheme to C 
;;
;; 
(define prologue "
/* change to 0 for no debug info to be printed: */
#define DO_SHOW 1

#include \"arch/debug_macros.h.c\"

#include <stdio.h>
#include <stdlib.h>
#include \"arch/cisc.h\"

int main() {
START_MACHINE;
JUMP(CONTINUE);
#include \"arch/char.lib\"
#include \"arch/io.lib\"
#include \"arch/math.lib\"
#include \"arch/string.lib\"
#include \"arch/system.lib\"
#include \"arch/scheme.lib\"
CONTINUE:
")

(define epilogue "
STOP_MACHINE;
return 0; 
}
")

(define compile-scheme-file
  (lambda (source dest)
    (let* ((code-text (file->string source))
           (sexprs (string->sexpr code-text (lambda (w) (error 'string->sexpr (format "input is not a legal symbolic expression: ~s" w)))))
           (pes
            (map (lambda (sexpr)
                   (annotate-tc
                    (pe->lex-pe
                     (box-set
                      (remove-applic-lambda-nil
                       (eliminate-nested-defines
                        (parse sexpr)))))))
                 sexprs))

           (tables (get-tables pes))
           (fvars (list->vector (car tables)))
           (consts (list->vector (cadr tables)))
           (inedxed-const-table (vector->list (create-const-table-indexes consts)))
           
           
           (generated-code (fold-left string-append "" (map (lambda (pes) (make-print (code-gen pes fvars inedxed-const-table))) pes)))
           (generated-code (string-append (generate-constants-macro inedxed-const-table) generated-code))

           (generated-code (string-append (>nl (>define CONST_TABLE_BASE_ADDR "1000"))
                                          (encode-const-table CONST_TABLE_BASE_ADDR consts inedxed-const-table)
                                          generated-code))
           
#|           (generated-code (string-append (generate-table-code consts
                                                               "1000" ; actual const table address
                                                               CONST_TABLE_BASE_ADDR
                                                               (^make-sob consts))
                                          generated-code))
|#
           #|(generated-code (string-append (generate-table-code fvars 
                                                               (base+displ CONST_TABLE_BASE_ADDR (number->string (vector-length consts))) 
                                                               FVARS_TABLE_BASE_ADDR
                                                               id) 
                                          generated-code))|#

           (generated-code (string-append prologue generated-code))
           (generated-code (string-append generated-code epilogue)))

      (let ((output-port (open-output-file dest)))
        (display generated-code output-port)
        (close-output-port output-port)))))



































