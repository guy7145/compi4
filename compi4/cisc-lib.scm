(define nl-string-append
  (lambda s
    (fold-left (lambda (acc el) (string-append acc el nl)) "" s)))

(define string-append-list
  (lambda (list)
    (fold-left string-append "" list)))

(define nl-string-append-list
  (lambda (list)
    (fold-left nl-string-append "" list)))
#|
;; for debugging purposes ONLY!!!
(define string-append
  (lambda x
    (display-green x)
    (string-append-list x)))
|#
#|
(define ocar car)
(define car (lambda (x) (display-colored-BIG x) (ocar x)))
|#

(define DEBUG-PRINT
  (lambda (obj)
    (nl-string-append (>push obj)
                      (>call "WRITE_SOB")
                      drop1
                      (>call "NEWLINE"))))

;; registers
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
(define R8 (register-generator))
(define r8 R8)
(define R9 (register-generator))
(define r9 R9)
(define R10 (register-generator))
(define r10 R10)
(define R11 (register-generator))
(define r11 R11)
(define R12 (register-generator))
(define r12 R12)
(define R13 (register-generator))
(define r13 R13)
(define R14 (register-generator))
(define r14 R14)
(define R15 (register-generator))
(define r15 R15)

(define fp "FP")
(define FP fp)
(define sp "SP")
(define SP sp)

;; constants
;; 
(define CONST_TABLE_ACTUAL_ADDRESS "1000")
(define CONST_TABLE_BASE_ADDR "consts_addr")
(define FVARS_TABLE_BASE_ADDR "fvars_addr")
(define SYMBOL_TABLE_BASE_ADDR "sym_tbl_addr")
(define SYMBOL_TABLE_LENGTH_COUNTER_ADDR "sym_tbl_length")
(define SYMBOL_TABLE_LENGTH_COUNTER_ACTUAL_ADDR "999")

(define sob-false "SOB_FALSE")
(define sob-true "SOB_TRUE")
(define sob-void "SOB_VOID")
(define sob-nil "SOB_NIL")


;; types
;; 
(define t_void "T_VOID")
(define t_nil "T_NIL")
(define t_bool "T_BOOL")
(define t_char "T_CHAR")
(define t_integer "T_INTEGER")
(define t_string "T_STRING")
(define t_symbol "T_SYMBOL")
(define t_pair "T_PAIR")
(define t_vector "T_VECTOR")
(define t_closure "T_CLOSURE")
(define t_rational "T_RATIONAL")

;; backend stuff
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


;; stack management
(define >push
  (lambda (R)
    (>func "PUSH" R)))
(define >pop
  (lambda (R)
    (>func "POP" R)))
(define >drop (lambda (x) (>func "DROP" x)))
(define drop1 (>drop "1"))


;; functions
(define >call
  (lambda (x)
    (>func "CALL" x)))
(define >calla
  (lambda (x)
    (>func "CALLA" x)))
(define >ret (lambda () "RETURN"))

;; addressing modes
(define >imm (lambda (x) (string-append "IMM" (>paren x))))
(define >ind (lambda (x) (string-append "IND" (>paren x))))
(define >indd (lambda (arr dis) (string-append "INDD" (>paren (>comma arr dis)))))


;; mov
(define >mov
  (lambda (dest src)
    (if (equal? dest src)
        nl
        (string-append "MOV" (>paren (>comma dest src)) ";"))))
(define >mov-res ;; moves to 'dest' the result of function application 'app'
  (lambda (dest app)
    (nl-string-append app
                      (>mov dest R0))))


;; arithmentics
(define >add
  (lambda (dest src)
    (string-append "ADD" (>paren (>comma dest src)) ";")))
(define >sub
  (lambda (dest src)
    (string-append "SUB" (>paren (>comma dest src)) ";")))
(define >mul
  (lambda (dest src)
    (string-append "MUL" (>paren (>comma dest src)) ";")))
(define >div
  (lambda (dest src)
    (string-append "DIV" (>paren (>comma dest src)) ";")))
(define >rem
  (lambda (dest src)
    (string-append "REM" (>paren (>comma dest src)) ";")))


;; compare
(define >compare
  (lambda (x y)
    (>func "CMP" (>comma x y))))
(define >cmp >compare)


;; jumps
(define >jmp
  (lambda (label)
    (>func "JUMP" label)))
(define >jmp-a
  (lambda (label)
    (>func "JUMPA" label)))
(define >jeq
  (lambda (label)
    (>func "JUMP_EQ" label)))
(define >jne
  (lambda (label)
    (>func "JUMP_NE" label)))
(define >jge
  (lambda (label)
    (>func "JUMP_GE" label)))
(define >jle
  (lambda (label)
    (>func "JUMP_LE" label)))
(define >jlt
  (lambda (label)
    (>func "JUMP_LT" label)))
(define >jgt
  (lambda (label)
    (>func "JUMP_GT" label)))

;; labels
(define >make-label
  (lambda (l)
    (string-append l ":")))
(define >get-label
  (lambda (l)
    (string-append "LABEL" (>paren l))))


(define base+displ
  (lambda (base displacement)
    (string-append base " + " displacement)))

(define base-displ
  (lambda (base displacement)
    (string-append base " - " displacement)))

(define >define
  (lambda (name value)
    (string-append "#define " name " " value)))


(define >fparg
  (lambda (x)
    (string-append "FPARG" (>paren (number->string x)))))
(define >fparg-nan
  (lambda (x)
    (string-append "FPARG" (>paren x))))
(define >fparg-displ
  (lambda (x disp)
    (string-append "FPARG" (>paren (base+displ (number->string x) (number->string disp))))))
(define >starg
  (lambda (x)
    (string-append "STARG" (>paren x))))
(define >stack
  (lambda (x)
    (string-append "STACK" (>paren x))))


(define >malloc
  (lambda (size)
    (nl-string-append (>push size)
                      (>call "MALLOC")
                      (>drop "1"))))
(define >cons
  (lambda (a b)
    (nl-string-append (>push a)
                      (>push b)
                      (>call "MAKE_SOB_PAIR")
                      (>drop "2"))))

(define >inc (lambda (x) (>func "INCR" x)))
(define >dec (lambda (x) (>func "DECR" x)))
(define >inc-twice (lambda (x) (nl-string-append (>inc x) 
                                                 (>inc x))))


(define loop-counter R7)
(define LOOP-EXIT-LABEL (label-generator "loop_exit_"))
(define LOOP-HEAD-LABEL (label-generator "loop_head_"))
(define >for-loop
  (lambda (start end ++ cond-jump . body)
    (let ((loop-head-label (LOOP-HEAD-LABEL))
          (loop-exit-label (LOOP-EXIT-LABEL))
          (body (nl-string-append-list body)))
      (nl-string-append (>mov loop-counter start)
                        (>make-label loop-head-label)
                        (>cmp loop-counter end)
                        (cond-jump loop-exit-label)
                        body
                        (++ loop-counter)
                        (>jmp loop-head-label)
                        (>make-label loop-exit-label)))))

(define >comment
  (lambda (x)
    (string-append "/* " x " */")))




;; CISC FORMS
;; 
(define >>scheme-function
  (lambda body (nl-string-append (>push fp)
                                 (>mov fp sp)

                                 (nl-string-append-list body)

                                 (>mov sp fp)
                                 (>pop fp)
                                 (>ret))))

(define >>arg
  (lambda (i)
    (>fparg-nan (base+displ "2" i))))










