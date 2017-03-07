(load "tdd-tools.scm")
(load "compiler.scm") 

(define compile-and-make-comparison-script
  (let ((compare-script-file "automated-compare-tests.scm")
        (filenames (list "or-tests" "eq?" "vectors-and-lists" "test03" "variable-scopes" "scheme-written-lib-functions" "lib-functions" "append" "tc-applic" "some-test-cases" "rationals" "numbers-and-booleans" "if-test" "lambda-opt-var-stack-fix" "a22" "test06" "apply" )) ; "cse-example" "lambda-var" "lambda-opt" "max-lib-tests" "simple-example" "lambda-simple-and-applic" "torture-test-for-compiler-02"
        (prologue "
(load \"tdd-tools.scm\")
")
(display-colored-BIG 'Automated-compare-tests:)
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
