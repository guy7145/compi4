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
			(>cmp R1 R2)
			(>jeq "pred_eq_end")
			(>mov R0 sob-false)
			(>make-label "pred_eq_end")
		)))

(define denomenator-encoder
	(lambda()
		(>>scheme-function
			(>mov R0 (>>arg "0"))
			(>mov R0 (>indd R0 "2"))
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

(define remainder-encoder ; TODO: fix
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
	(lambda() 
		(>>scheme-function
			(>mov R0 (>>arg "0"))			; func
			(>mov R1 (>>arg "1"))			; args list
			(>mov R2 (>imm "0"))			; args counter
			(>make-label "APPLY_LOOP_START")
			(>cmp R1 sob-nil)			; whie list != null
			(>jeq "APPLY_LOOP_END")
			(>push (>indd R1 "1"))			; push car
			(>add R2 "1")				; counter++
			(>mov R1 (>indd R1 "2"))		; list++
			(>jmp "APPLY_LOOP_START")
			(>make-label "APPLY_LOOP_END")
			(>push R2)				; push counter
			(>push (>indd R1 "1"))			; push env
			(>calla (>indd R0 "2"))			; call func
			(>pop R1) 				; env
			(>pop R1) 				; counter
			(>drop R1)
		)))

(define bin-add-encoder
	(lambda()
		(>>scheme-function
			(>mov R0 (>>arg "0"))
			(>mov R0 (>indd R0 "1"))
			(>mov R1 (>>arg "1"))
			(>mov R1 (>indd R1 "1"))
			(>add R0 R1)
			(>push R0)
			(>call "MAKE_SOB_INTEGER")
		)))

(define bin-sub-encoder
	(lambda()
		(>>scheme-function
			(>mov R0 (>>arg "0"))
			(>mov R0 (>indd R0 "1"))
			(>mov R1 (>>arg "1"))
			(>mov R1 (>indd R1 "1"))
			(>sub R0 R1)
			(>push R0)
			(>call "MAKE_SOB_INTEGER")
		)))

(define bin-mul-encoder
	(lambda()
		(>>scheme-function
			(>mov R0 (>>arg "0"))
			(>mov R0 (>indd R0 "1"))
			(>mov R1 (>>arg "1"))
			(>mov R1 (>indd R1 "1"))
			(>mul R0 R1)
			(>push R0)
			(>call "MAKE_SOB_INTEGER")
		)))

(define bin-div-encoder
	(lambda()
		(>>scheme-function
			(>mov R0 (>>arg "0"))
			(>mov R0 (>indd R0 "1"))
			(>mov R1 (>>arg "1"))
			(>mov R1 (>indd R1 "1"))
			(>div R0 R1)
			(>push R0)
			(>call "MAKE_SOB_INTEGER")
		)))

(define max-library-functions-encoders
	`((not . ,not-encoder)
	  (eq? . ,eq?-encoder)
	  (denominator . ,denomenator-encoder)
	  (numerator . ,numerator-encoder)
	  (remainder . ,remainder-encoder)
	  (apply . ,apply-encoder)
	  (bin+ . ,bin-add-encoder)
	  (- . ,bin-sub-encoder)
	  (* . ,bin-mul-encoder)
	  (/ . ,bin-div-encoder)
	))
