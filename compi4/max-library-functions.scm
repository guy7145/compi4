(define not-encoder
  (lambda ()
    (>>scheme-function
     (>mov R0 sob-true)
     (>mov R1 (>>arg "0"))
     (>cmp R1 sob-false)
     (>jeq "pred_not_end")
     (>mov R0 sob-false)
     (>make-label "pred_not_end")
     )))

(define eq?-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 sob-true)
     (>mov R1 (>>arg "0"))
     (>mov R2 (>>arg "1"))
     (>cmp (>ind R1) (>ind R2)) ; compare types
     (>jne "pred_eq_false")

     (>cmp (>ind R1) t_integer)
     (>jeq "pred_eq_int")
     (>cmp (>ind R1) t_rational)
     (>jeq "pred_eq_rational")
     (>cmp (>ind R1) t_char)
     (>jeq "pred_eq_char")
     (>cmp (>ind R1) t_symbol)
     (>jeq "pred_eq_symbol")
     (>jmp "pred_eq_other")

     (>make-label "pred_eq_int")
     (>make-label "pred_eq_char")
     (>make-label "pred_eq_symbol")
     (>cmp (>indd R1 "1") (>indd R2 "1"))
     (>jne "pred_eq_false")
     (>jmp "pred_eq_end")

     (>make-label "pred_eq_rational")
     (>cmp (>indd R1 "1") (>indd R2 "1"))
     (>jne "pred_eq_false")
     (>cmp (>indd R1 "2") (>indd R2 "2"))
     (>jne "pred_eq_false")
     (>jmp "pred_eq_end")

     (>make-label "pred_eq_other")
     (>mov R1 (>>arg "0"))
     (>mov R2 (>>arg "1"))
     (>cmp R1 R2)
     (>jeq "pred_eq_end")

     (>make-label "pred_eq_false")
     (>mov R0 sob-false)
     (>make-label "pred_eq_end")
     )))

(define denomenator-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 (>>arg "0"))

     (>cmp (>ind R0) "T_RATIONAL")
     (>jeq "denominator_rational")
     (>mov R0 (>imm "1"))
     (>jmp "denominator_end")

     (>make-label "denominator_rational")
     (>mov R0 (>indd R0 "2"))

     (>make-label "denominator_end")
     (>push R0)
     (>call "MAKE_SOB_INTEGER")
     (>drop "1")
     )))

(define numerator-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 (>>arg "0"))
     (>mov R0 (>indd R0 "1"))
     (>push R0)
     (>call "MAKE_SOB_INTEGER")
     (>drop "1")
     )))

(define remainder-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 (>>arg "0"))
     (>mov R0 (>indd R0 "1"))
     (>mov R1 (>>arg "1"))
     (>mov R1 (>indd R1 "1"))
     (>rem R0 R1)
     (>push R0)
     (>call "MAKE_SOB_INTEGER")
     (>drop "1")
     )))

(define apply-encoder
  (let ((ret r11)
        (env-f r12) ; f's enviroment
        (n r13) ; number of arguments
        (f r14) ; f's obj
        (body-label-f r14) ; f's body label
        (lst r10)
        (car r9)
        (cdr r8)
        (car-offset "1")
        (cdr-offset "2")
        (length r15)
        (sp-backup r6)
        (my-loop-counter r5)
        (l-push-list-elements-loop "l_apply_push_list_elements_loop")
        (l-finished-pushing-list-elements "l_apply_finished_pushing_list_elements")
        (l-stacking-arguments-loop "l_apply_stacking_arguments_loop")
        (l-finished-stacking-arguments "l_apply_finished_stacking_arguments")
        (l-move-frame-to-base-loop "l_apply_move_frame_to_base_loop")
        (l-finished-moving-frame-to-base "l_apply_finished_moving_frame_to_base"))
    
    (lambda()
      (nl-string-append ;"INFO"
                        (>mov fp sp) ; we will use macro (>>arg ...) as if f argument doesn't exist and (>>arg "0") [fparg(2)] is the first not-f non-list argument

                        (>pop ret)  ; ret
                        (>drop "1") ; apply's env
                        (>pop n)    ; n (num of args)
                        (>pop f)    ; func

                        
                        
                        (>mov env-f (>indd f "1"))
                        (>mov body-label-f (>indd f "2"))

                        (>mov sp-backup sp)
                        (>sub n "2") ; sub f and lst; n <-- n-2
                        (>sub sp n); point at lst
                        (>pop lst)


                        (>add sp "1") ; so we won't overwrite arguments while 
                        (>add sp n)   ; retrieving list's elements 

                        ;; push list elements abovethe rest of the arguments
                        (>mov length "0")
                        
                        (>make-label l-push-list-elements-loop)
                        (>cmp (>ind lst) t_pair) ; (improper lists?)
                        (>jne l-finished-pushing-list-elements)
                        
                        (>inc length)
                        (>mov car (>indd lst car-offset))
                        (>mov lst (>indd lst cdr-offset))
                        (>push car)
                        ;(format "SHOW(\"car:\", INDD(R9, 1));")
                        (>jmp l-push-list-elements-loop)
                        
                        ;(format "SHOW(\"length:\", R15);")

                        (>make-label l-finished-pushing-list-elements)

                        ;; push rest of the arguments atop the list elements 
                        (>mov my-loop-counter n)
                        
                        (>make-label l-stacking-arguments-loop)
                        (>cmp my-loop-counter "0")
                        (>jlt l-finished-stacking-arguments)
                        (>dec my-loop-counter)
                        (>push (>>arg my-loop-counter))
                        (>jmp l-stacking-arguments-loop)
                        (>make-label l-finished-stacking-arguments)
                        
                        ;; move sp-backup to the start of the frame:
                        (>sub sp-backup n)
                        (>sub sp-backup "1")
                        
                        ;;
                        (>add sp-backup n)
                        (>add sp-backup length)
                        (>mov fp sp-backup)
                        (>add fp "4")
                        
                        ;; move all arguments to the base of the frame (both normal arguments and list elements)
                        (>mov my-loop-counter n)
                        (>add my-loop-counter length)
                        
                        (>make-label l-move-frame-to-base-loop)
                        (>cmp my-loop-counter "0")
                        (>jeq l-finished-moving-frame-to-base)
                        (>dec my-loop-counter)
                        (>mov (>>arg my-loop-counter) (>starg my-loop-counter))
                        (>jmp l-move-frame-to-base-loop)
                        (>make-label l-finished-moving-frame-to-base)
                        
                        ;; recalculate n (add number of list elements)
                        (>add n length)
                        
                        (>mov sp sp-backup)
                        
                        ;; push ret, env and n
                        (>push n)
                        (>push env-f)
                        (>push ret)
                        
                        (>jmp-a body-label-f)
                        )
      )))













(define add-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 (>imm "0"))
     (>mov R2 (>fparg 1))
     (>mov R3 (>imm "1"))
     (>jmp "op_add_end")
     (>for-loop "2"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>mov R4 (>imm "1"))

                (>cmp (>ind R1) "T_RATIONAL")
                (>jne "op_add_integer")
                (>mov R4 (>indd R1 "2"))

                (>make-label "op_add_integer")
                (>mov R1 (>indd R1 "1"))
                (>mul R1 R3)
                (>mul R3 R4)
                (>mul R0 R4)
                (>add R0 R1))
     (>mov R5 R0)
     (>rem R5 R3)
     (>cmp R5 (>imm "0"))
     (>jeq "op_add_res_int")
     (>push R3)
     (>push R0)
     (>call "MAKE_SOB_RATIONAL")
     (>drop "2")
     (>jmp "op_add_end")

     (>make-label "op_add_res_int")
     (>div R0 R3)
     (>push R0)
     (>call "MAKE_SOB_INTEGER")
     (>drop "1")
     (>make-label "op_add_end")
     )))

(define sub-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 (>fparg-nan (>imm "2")))
     (>mov R0 (>indd R0 "1"))
     (>mov R3 (>imm "1"))
     (>cmp (>ind (>fparg-nan (>imm "2"))) "T_RATIONAL")
     (>jne "SUB_OPERATOR_FIRST_IS_INTEGER")
     (>mov R3 (>fparg-nan (>imm "2")))
     (>mov R3 (>indd R3 "2"))
     (>make-label "SUB_OPERATOR_FIRST_IS_INTEGER")
     (>mov R2 (>fparg 1))
     (>cmp R2 (>imm "1"))
     (>jne "SUB_OPERATOR_MULTI_ARG")
     (>mul R0 (>imm "-1"))
     (>make-label "SUB_OPERATOR_MULTI_ARG")
     (>for-loop "3"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>mov R4 (>imm "1"))

                (>cmp (>ind R1) "T_RATIONAL")
                (>jne "op_sub_integer")
                (>mov R4 (>indd R1 "2"))

                (>make-label "op_sub_integer")
                (>mov R1 (>indd R1 "1"))
                (>mul R1 R3)
                (>mul R3 R4)
                (>mul R0 R4)
                (>sub R0 R1))
     (>mov R5 R0)
     (>rem R5 R3)
     (>cmp R5 (>imm "0"))
     (>jeq "op_sub_res_int")
     (>push R3)
     (>push R0)
     (>call "MAKE_SOB_RATIONAL")
     (>drop "2")
     (>jmp "op_sub_end")

     (>make-label "op_sub_res_int")
     (>div R0 R3)
     (>push R0)
     (>call "MAKE_SOB_INTEGER")
     (>drop "1")
     (>make-label "op_sub_end")
     )))

(define mul-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 (>imm "1"))
     (>mov R2 (>fparg 1))
     (>mov R3 (>imm "1"))
     (>for-loop "2"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>mov R4 (>imm "1"))

                (>cmp (>ind R1) "T_RATIONAL")
                (>jne "op_mul_integer")
                (>mov R4 (>indd R1 "2"))

                (>make-label "op_mul_integer")
                (>mov R1 (>indd R1 "1"))
                (>mul R3 R4)
                (>mul R0 R1))
     (>mov R5 R0)
     (>rem R5 R3)
     (>cmp R5 (>imm "0"))
     (>jeq "op_mul_res_int")
     (>push R3)
     (>push R0)
     (>call "MAKE_SOB_RATIONAL")
     (>drop "2")
     (>jmp "op_mul_end")

     (>make-label "op_mul_res_int")
     (>div R0 R3)
     (>push R0)
     (>call "MAKE_SOB_INTEGER")
     (>drop "1")
     (>make-label "op_mul_end")
     )))

(define div-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 (>fparg-nan (>imm "2")))
     (>mov R0 (>indd R0 "1"))
     (>mov R3 (>imm "1"))
     (>cmp (>ind (>fparg-nan (>imm "2"))) "T_RATIONAL")
     (>jne "DIV_OPERATOR_FIRST_IS_INTEGER")
     (>mov R3 (>fparg-nan (>imm "2")))
     (>mov R3 (>indd R3 "2"))
     (>make-label "DIV_OPERATOR_FIRST_IS_INTEGER")
     (>mov R2 (>fparg 1))
     (>cmp R2 (>imm "1"))
     (>jne "DIV_OPERATOR_MULTI_ARG")
     (>mov R4 R0)
     (>mov R0 R3)
     (>mov R3 R4)
     (>make-label "DIV_OPERATOR_MULTI_ARG")
     (>for-loop "3"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>mov R4 (>imm "1"))

                (>cmp (>ind R1) "T_RATIONAL")
                (>jne "op_div_integer")
                (>mov R4 (>indd R1 "2"))

                (>make-label "op_div_integer")
                (>mov R1 (>indd R1 "1"))
                (>mul R3 R1)
                (>mul R0 R4))
     (>mov R5 R0)
     (>rem R5 R3)
     (>cmp R5 (>imm "0"))
     (>jeq "op_div_res_int")
     (>push R3)
     (>push R0)
     (>call "MAKE_SOB_RATIONAL")
     (>drop "2")
     (>jmp "op_div_end")

     (>make-label "op_div_res_int")
     (>div R0 R3)
     (>push R0)
     (>call "MAKE_SOB_INTEGER")
     (>drop "1")
     (>make-label "op_div_end")
     )))

(define less-than-encoder
  (lambda ()
    (>>scheme-function
     (>mov R5 sob-true)
     (>mov R0 (>fparg-nan (>imm "2")))
     (>mov R0 (>indd R0 "1"))
     (>mov R3 (>imm "1"))
     (>cmp (>ind (>fparg-nan (>imm "2"))) "T_RATIONAL")
     (>jne "LT_OPERATOR_FIRST_IS_INTEGER")
     (>mov R3 (>fparg-nan (>imm "2")))
     (>mov R3 (>indd R3 "2"))
     (>make-label "LT_OPERATOR_FIRST_IS_INTEGER")
     (>mov R2 (>fparg 1))
     (>for-loop "3"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>mov R4 (>imm "1"))

                (>cmp (>ind R1) "T_RATIONAL")
                (>jne "op_lt_integer")
                (>mov R4 (>indd R1 "2"))

                (>make-label "op_lt_integer")
                (>mov R1 (>indd R1 "1"))
                (>mov R6 R0)
                (>mov R8 R1)
                (>mul R6 R4)
                (>mul R8 R3)
                (>cmp R6 R8)
                (>jlt "op_lt_ok")
                (>mov R5 sob-false)
                (>jmp "op_lt_end")

                (>make-label "op_lt_ok")
                (>mov R0 R1)
                (>mov R3 R4))
     (>make-label "op_lt_end")
     (>mov R0 R5)
     )))

(define greater-than-encoder
  (lambda ()
    (>>scheme-function
     (>mov R5 sob-true)
     (>mov R0 (>fparg-nan (>imm "2")))
     (>mov R0 (>indd R0 "1"))
     (>mov R3 (>imm "1"))
     (>cmp (>ind (>fparg-nan (>imm "2"))) "T_RATIONAL")
     (>jne "GT_OPERATOR_FIRST_IS_INTEGER")
     (>mov R3 (>fparg-nan (>imm "2")))
     (>mov R3 (>indd R3 "2"))
     (>make-label "GT_OPERATOR_FIRST_IS_INTEGER")
     (>mov R2 (>fparg 1))
     (>for-loop "3"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>mov R4 (>imm "1"))

                (>cmp (>ind R1) "T_RATIONAL")
                (>jne "op_gt_integer")
                (>mov R4 (>indd R1 "2"))

                (>make-label "op_gt_integer")
                (>mov R1 (>indd R1 "1"))
                (>mov R6 R0)
                (>mov R8 R1)
                (>mul R6 R4)
                (>mul R8 R3)
                (>cmp R6 R8)
                (>jgt "op_gt_ok")
                (>mov R5 sob-false)
                (>jmp "op_gt_end")

                (>make-label "op_gt_ok")
                (>mov R0 R1)
                (>mov R3 R4))
     (>make-label "op_gt_end")
     (>mov R0 R5)
     )))

(define equals-encoder
  (lambda ()
    (>>scheme-function
     (>mov R5 sob-true)
     (>mov R0 (>fparg-nan (>imm "2")))
     (>mov R0 (>indd R0 "1"))
     (>mov R3 (>imm "1"))
     (>cmp (>ind (>fparg-nan (>imm "2"))) "T_RATIONAL")
     (>jne "EQ_OPERATOR_FIRST_IS_INTEGER")
     (>mov R3 (>fparg-nan (>imm "2")))
     (>mov R3 (>indd R3 "2"))
     (>make-label "EQ_OPERATOR_FIRST_IS_INTEGER")
     (>mov R2 (>fparg 1))
     (>for-loop "3"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>mov R4 (>imm "1"))

                (>cmp (>ind R1) "T_RATIONAL")
                (>jne "op_eq_integer")
                (>mov R4 (>indd R1 "2"))

                (>make-label "op_eq_integer")
                (>mov R1 (>indd R1 "1"))
                (>mov R6 R0)
                (>mov R8 R1)
                (>mul R6 R4)
                (>mul R8 R3)
                (>cmp R6 R8)
                (>jeq "op_eq_ok")
                (>mov R5 sob-false)
                (>jmp "op_eq_end")

                (>make-label "op_eq_ok")
                (>mov R0 R1)
                (>mov R3 R4))
     (>make-label "op_eq_end")
     (>mov R0 R5)
     )))

(define make-vector-encoder
  (let
      ((skip-default-value-assignment "make_vector_skip_default_value_assignment")
       (end-label "make_vector_end_label"))
    (lambda ()
      (>>scheme-function
       (>mov r3 (>fparg 1))
       (>cmp r3 (>imm "1"))
       (>jne skip-default-value-assignment)

       (>push "0")
       (>call "MAKE_SOB_INTEGER")
       (>drop "1")
       (>mov r2 r0)
       (>jmp end-label)
       (>make-label skip-default-value-assignment)

       (>mov r2 (>>arg "1")) ; sob


       (>make-label end-label)
       (>mov r1 (>>arg "0")) ; sob_int
       (>mov r1 (>indd r1 "1"))
       (>add r1 (>imm "2"))

       (>mov-res r0 (>malloc r1))
       (>mov (>indd r0 "0") t_vector)
       (>sub r1 (>imm "2"))
       (>mov (>indd r0 "1") r1)
       (>for-loop
        r1
        "0"
        >dec
        >jlt
        (>mov (>indd r0 (base+displ loop-counter "2")) r2)
        )
       ))
    ))



(define make-string-encoder
  (let
      ((skip-default-value-assignment "make_string_skip_default_value_assignment")
       (end-label "make_string_end_label"))
    (lambda ()
      (>>scheme-function
       (>mov r3 (>fparg 1))
       (>cmp r3 (>imm "1"))
       (>jne skip-default-value-assignment)

       (>mov r2 (>imm "0"))
       (>jmp end-label)
       (>make-label skip-default-value-assignment)

       (>mov r2 (>>arg "1")) ; sob
       (>mov r2 (>indd r2 "1"))

       (>make-label end-label)
       (>mov r1 (>>arg "0")) ; sob_int
       (>mov r1 (>indd r1 "1"))
       (>add r1 (>imm "2"))

       (>mov-res r0 (>malloc r1))
       (>mov (>indd r0 "0") t_string)
       (>sub r1 (>imm "2"))
       (>mov (>indd r0 "1") r1)
       (>for-loop
        r1
        "0"
        >dec
        >jlt
        (>mov (>indd r0 (base+displ loop-counter "2")) r2)
        )
       ))
    ))

(define vector-encoder
  (lambda()
    (>>scheme-function
     (>mov R2 (>fparg 1))
     (>for-loop "2"
                (base+displ R2 "2")
                >inc
                >jge
                (>mov R1 (>fparg-nan loop-counter))
                (>push R1))
     (>push R2)
     (>call "MAKE_SOB_VECTOR")
     (>pop R1)
     (>drop R1)
     )))

(define rational?-encoder
  (lambda()
    (>>scheme-function
     (>mov R0 sob-true)
     (>mov R1 (>>arg "0"))
     (>mov R1 (>ind R1))
     (>cmp R1 t_rational)
     (>jeq "pred_eq_end")
     (>cmp R1 t_integer)
     (>jeq "pred_eq_end")
     (>mov R0 sob-false)
     (>make-label "pred_rational_end")
     )))

(define max-library-functions-encoders
  `((not . ,not-encoder)
    (eq? . ,eq?-encoder)
    (denominator . ,denomenator-encoder)
    (numerator . ,numerator-encoder)
    (remainder . ,remainder-encoder)
    (apply . ,apply-encoder)
    (+ . ,add-encoder)
    (- . ,sub-encoder)
    (* . ,mul-encoder)
    (/ . ,div-encoder)
    (< . ,less-than-encoder)
    (> . ,greater-than-encoder)
    (= . ,equals-encoder)
    (make-vector . ,make-vector-encoder)
    (make-string . ,make-string-encoder)
    (vector . ,vector-encoder)
    (rational? . ,rational?-encoder)
    ))
