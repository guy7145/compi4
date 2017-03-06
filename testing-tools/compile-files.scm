(load "tdd-tools.scm")
(load "compiler.scm")

(define compile-and-make-comparison-script
  (let ((compare-script-file "automated-compare-tests.scm")
        (filenames (list "lambda-simple-and-applic" "eq?" "vectors-and-lists" "test03" )) ; "test06" "cse-example" "lambda-var" "lambda-opt" "lambda-opt-var-stack-fix" "rationals" "numbers-and-booleans" "if-test" "or-tests" "lib-functions" "max-lib-tests" "scheme-written-lib-functions" "simple-example"
        (prologue "
(load \"tdd-tools.scm\")
#|
(define odisplay display)
(define display	(lambda (x) 
		(if 	(pair? x) 
			(begin 	(display #\\( ) 
				(display (car x)) 
				(display #\\.) 
				(display (cdr x)) 
				(display #\\) )
			)
			(odisplay x)
		)
(display-colored-BIG 'Automated-compare-tests:)
))
|#
")
        (epilogue "
(LightCyan)(display-normal 'done.)(NC)
")
        (generate-compare-command-line
         (let ((part1 "(LightCyan)(display-title '")
               (part2 ")(NC)(COMPARE-FILE-CONTENTS ")
               (part3 ")(newline)"))
           (lambda (filename)
             (string-append part1 filename part2 "\"" filename ".out2" "\" \"" filename ".out1" "\"" part3)))))

    (lambda ()
      (map (lambda (filename)
             (Green)(display (string-append filename ".scm"))(display " -> ")(display (string-append filename ".c"))(newline)(NC)
             (compile-scheme-file (string-append filename ".scm")
                                  (string-append filename ".c")))
           filenames)
      (if (file-exists? compare-script-file) (delete-file compare-script-file))
      (let ((output-port (open-output-file compare-script-file)))
        (display prologue output-port)
        (display (fold-left (lambda (acc el) (string-append acc (generate-compare-command-line el))) "" filenames) output-port)
        (display epilogue output-port)
        (close-output-port output-port)
        (LightGreen)(display-normal '(automatic tests file created successfully))(NC)))))

(compile-and-make-comparison-script)
