;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - ASS3 - Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "compiler.scm")
(define test-func (lambda (x)
                    (annotate-tc
                     (pe->lex-pe
                      (box-set
                       (remove-applic-lambda-nil
                        (eliminate-nested-defines (parse x))))))))

(load "hw3.so")

(define tests-counter 0)
(define failed-tests-counter 0)

;;;; Configuration
(define show-passed-tests #t)
(define show-summary #t)

(define show-difference
  (lambda (actual expected)
    (if (or (null? actual) (null? expected)) ""
        (if (equal? (car actual) (car expected))
            (begin (display (format "\033[1;32m~s\033[0m" (car actual)))
              (show-difference (cdr actual) (cdr expected)))
            (begin (display (format "\033[1;31m~s\033[0m" (car actual)))
              (show-difference (cdr actual) (cdr expected)))))
    ))

(define assert
  (lambda (input)
    (set! tests-counter (+ 1 tests-counter))
    (let ((actual-output (test-func input))
          (expected-output (full-cycle input)))
      (cond ((equal? actual-output expected-output)
             (if show-passed-tests
                 (begin (display (format "~s) ~s\n" tests-counter input))
                   (display (format "\033[1;32m Success! ☺ \033[0m \n\n"))))
             #t)
            (else
             (set! failed-tests-counter (+ 1 failed-tests-counter))
             (display (format "~s) ~s\n" tests-counter input))
             (display (format "\033[1;31mFailed! ☹\033[0m\n\n\033[1;34mExpected:\n ~s\033[0m\n\n\033[1;29mActual:\n ~s\033[0m\n\n" expected-output actual-output))
             #f))
      )))

(define runTests
  (lambda (tests-name lst)
    (newline)
    (display tests-name)
    (display ":")
    (newline)
    (display "==============================================")
    (newline)
    (let ((results (map assert lst)))
      (newline)
      (cond ((andmap (lambda (exp) (equal? exp #t)) results)
             (display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)
            (else
             (display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
    ))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      (if show-summary
          (begin
            (display (format "Summary\n=============================\n\033[1;32mPassed: ~s of ~s tests ☺\033[0m\n" (- tests-counter failed-tests-counter) tests-counter))
            (if (> failed-tests-counter 0)
                (display (format "\033[1;31mFailed: ~s of ~s tests ☹\033[0m\n\n" failed-tests-counter tests-counter)))))
      (cond ((andmap (lambda (exp) (equal? exp #t)) results)
             (display "\033[1;32m!!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
            (else (display "\033[1;31m#####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
      (newline))
    ))

(define MyTests
  (list '()
        ))
(runAllTests
 (list
  (cons "MyTests" MyTests)
  ))