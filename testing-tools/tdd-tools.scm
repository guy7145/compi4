;; styling tools
;;
(define display-normal (lambda (msg) (display (format "~s\n" msg))))
(define display-title (lambda (msg) (display (format "~s:\n" msg))))
(define display-green (lambda (msg) (display (format "\033[1;32m~s\033[0m \n" msg))))
(define display-red (lambda (msg) (display (format "\033[1;31m~s\033[0m \n" msg))))
(define display-colored (lambda (msg) (display (format "\033[1;33m~s\033[0m \n" msg))))
(define display-colored-2 (lambda (msg) (display (format "\033[1;30m~s\033[0m \n" msg))))
(define display-colored-title (lambda (msg) (display (format "\033[1;33m~s:\033[0m \n" msg))))
(define display-colored-BIG (lambda (msg) (display (format "\033[1;36m~s\033[0m \n" msg))))
(define display-colored-BIG-title (lambda (msg) (display (format "\033[1;36m~s:\033[0m \n" msg))))

                                        ; bright colors:
(define DarkGray (lambda() (display "\033[1;30m")))
(define LightRed (lambda() (display "\033[1;31m")))
(define LightGreen (lambda() (display "\033[1;32m")))
(define Yellow (lambda() (display "\033[1;33m")))
(define LightBlue (lambda() (display "\033[1;34m")))
(define LightPurple (lambda() (display "\033[1;35m")))
(define LightCyan (lambda() (display "\033[1;36m")))
(define White (lambda() (display "\033[1;37m")))

                                        ; dark colors:
(define Black (lambda() (display "\033[0;30m")))
(define Red (lambda() (display "\033[0;31m")))
(define Green (lambda() (display "\033[0;32m")))
(define Brown (lambda() (display "\033[0;33m"))) ; /Orange?
(define Blue (lambda() (display "\033[0;34m")))
(define Purple (lambda() (display "\033[0;35m")))
(define Cyan (lambda() (display "\033[0;36m")))
(define LightGray (lambda() (display "\033[0;37m")))

(define NC (lambda() (display "\033[0m"))) ; No Color

;; testing tools
;;
(define default-succ-cont (lambda () (display-green 'success!)))
(define default-fail-cont (lambda () (display-red 'failed!)))
(define default-maybe-cont (lambda () (display-colored 'maybe!)))

(define match-test
  (lambda (result expected success fail maybe)
    (cond ( ; success
           (equal? result expected) (success))
          ( ; fail
           (or (and (list? result) (not-list? expected))
               (and (not-list? result) (list? expected)))
           (fail))
          ( ; maybe
           (and (not-list? result) (not-list? expected))
           (maybe result expected)
           )
          (else ; next iteration
           (let ((succ-cont (lambda () (match-test (cdr result) (cdr expected) success fail maybe)))
                 (maybe-cont (lambda (g1 g2)
                               (display-colored-2 `(,g1 as ,g2))
                               (match-test (cdr result)
                                           (cdr expected)
                                           maybe
                                           fail
                                           maybe))))
             (match-test
              (car result)
              (car expected)
              succ-cont
              fail
              maybe-cont))))))

(define ASSERT-MATCH
  (lambda (result expected)
    (match-test result
                expected
                default-succ-cont
                default-fail-cont
                default-maybe-cont)))

(define ASSERT-EQUAL
  (lambda (result expected)
    (cond ((equal? result expected) (begin (display-green 'passed!) #t))
          (else (begin
                  (display-red 'failed!)
                  (display-colored 'expected:)
                  (display expected)
                  (newline)
                  (display-colored 'actual:)
                  (display result)
                  (newline)
                  #f)))
    ))

(define ASSERT-EQUAL-ex
  (lambda (input parse f1 f2)
    (if (not (ASSERT-EQUAL (f1 input) (f2 input)))
        (begin (display-colored input) (display-colored-2 (parse input)) #f)
        #t)))

(define get-first-line
  (lambda (lst)
    (if (null? lst)
        '()
        (let ((current (car lst))
              (rest (cdr lst)))
          (if (eq? #\newline current)
              '()
              (cons current (get-first-line rest)))))))

(define get-rest-lines
  (lambda (lst)
    (if (null? lst) 
        lst
        (let ((current (car lst))
              (rest (cdr lst)))
          (if (eq? #\newline current)
              rest
              (get-first-line rest))))))

(define COMPARE-FILE-CONTENTS
  (let ((file->list
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
               (run))))))
#|    
    (letrec ((assertion-loop
              (lambda (file1 file2)
                (let ((obj1 (list->string (get-first-line file1)))
                      (obj2 (list->string (get-first-line file2))))
                  (if (not (and (eq? obj1 "") (eq? obj2 "")))
                      (begin
                        (ASSERT-EQUAL obj1 obj2)
                        (assertion-loop (get-rest-lines file1) (get-rest-lines file2))))))))
      |#
      (lambda (file1 file2)
        (let ((file1 (list->string (file->list file1))) (file2 (list->string (file->list file2))))
          (ASSERT-EQUAL file1 file2)))))


