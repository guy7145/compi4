(define register-generator (label-generator "R"))
(define R0 (register-generator))
(define r0 R0)
(define R1 (register-generator))
(define r1 R1)
(define R2 (register-generator))
(define r2 R2)
(define R3 (register-generator))
(define r3 R3)
(define R4 (register-generator))
(define r4 R4)
(define R5 (register-generator))
(define r5 R5)
(define R6 (register-generator))
(define r6 R6)
(define R7 (register-generator))
(define r7 R7)

(define CONST_TABLE_BASE_ADDR "consts_addr")
(define FVARS_TABLE_BASE_ADDR "fvars_addr")

(define sob-false "SOB_FALSE")
(define sob-true "SOB_TRUE")
(define sob-void "SOB_VOID")
(define sob-nil "SOB_NIL")


(define nl "\n")
(define >nl
  (lambda (x)
    (string-append x nl)))
(define >comma
  (lambda s
    (if (null? s)
        s
        (fold-left (lambda (acc el) (if (null? el) acc) (string-append acc " , " el)) (car s) (cdr s)))))
(define >paren
  (lambda (x)
    (string-append "(" x ")")))
(define >func
  (lambda (f x)
    (string-append f (>paren x) ";")))
(define >push
  (lambda (R)
    (>func "PUSH" R)))
(define >call
  (lambda (x)
    (>func "CALL" x)))
(define drop1 (>func "DROP" "1"))
(define >drop1 (lambda (x) (string-append x drop1)))
(define >imm (lambda (x) (string-append "IMM" (>paren x))))
(define >ind (lambda (x) (string-append "IND" (>paren x))))
(define >indd (lambda (arr dis) (string-append "INDD" (>paren (>comma arr dis)))))
(define >mov
  (lambda (dest src)
    (string-append "MOV" (>paren (>comma dest src)))))
(define >compare
  (lambda (x y)
    (>func "CMP" (>comma x y))))
(define >cmp >compare)
(define >jmp
  (lambda (label)
    (>func "JUMP" label)))
(define >jeq
  (lambda (label)
    (>func "JUMP_EQ" label)))
(define >jne
  (lambda (label)
    (>func "JUMP_NE" label)))
(define >label
  (lambda (l)
    (string-append l ":")))
(define nl-string-append
  (lambda s
    (fold-left (lambda (acc el) (string-append acc el nl)) "" s)))
(define base+displ
  (lambda (base displacement)
    (string-append base " + " displacement)))
(define >define
  (lambda (name value)
    (string-append "#define " name " " value)))
(define >fparg
  (lambda (x disp)
    (string-append "‫‪FPARG" (>paren (base+displ (number->string x) (number->string disp))))))

















