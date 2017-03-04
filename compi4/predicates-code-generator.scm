(define DUMMY_ENV "DUMMY_ENVIROMENT")
(define DUMMY_ENV_ACTUAL_ADDRESS "0")
(define generate-predicates
  (let* ((^label-lib-func (label-generator "LIB_FUNC_"))
         (pred-names-types-and-labels (list `(boolean? ,t_bool ,(^label-lib-func)) ; 'zero? 'rational? 'number?
                                            `(char? ,t_char ,(^label-lib-func))
                                            `(integer? ,t_integer ,(^label-lib-func))
                                            `(null? ,t_nil ,(^label-lib-func))
                                            `(pair? ,t_pair ,(^label-lib-func))
                                            `(procedure? ,t_closure ,(^label-lib-func))
                                            `(string? ,t_string ,(^label-lib-func))
                                            `(symbol? ,t_symbol ,(^label-lib-func))
                                            `(vector? ,t_vector ,(^label-lib-func))))
         (get-name car)
         (get-type cadr)
         (get-label caddr)
         (^label-exit (label-generator "L_LIBARY_PREDICATE_EXIT_"))
         (^label-eq (label-generator "L_LIBARY_PREDICATE_TRUE_"))
         (predicate-code-gen
          (lambda (type func-label)
            (let ((label-exit (^label-exit))
                  (label-eq (^label-eq)))
              (nl-string-append (>make-label func-label)
                                (>push fp)
                                (>mov fp sp)

                                (>mov R0 (>fparg 2))     ; mov r0, argument                         
                                (>cmp (>ind R0) type)    ; 
                                (>jeq label-eq)
                                (>mov R0 sob-false)
                                (>jmp label-exit)
                                (>make-label label-eq)   ; equal:
                                (>mov R0 sob-true)

                                (>make-label label-exit) ; exit:
                                (>mov sp fp)
                                (>pop fp)
                                (>ret)
                                )))))

    (lambda (fvar-tbl)
      (map (lambda (name-type-label)
             (let ((name (get-name name-type-label))
                   (type (get-type name-type-label))
                   (func-label (get-label name-type-label))
                   (offset-in-tbl (search-fvar-index-by-name fvar-tbl name)))

               (nl-string-append (>comment  (format "library-function: ~s" name))
                                 (predicat-code-gen type)
                                 (>mov (>indd FVARS_TABLE_BASE_ADDR (number->string offset-in-tbl)) R0)
                                 (>mov R0 sob-void))))
           pred-names-types-and-labels))))














