;; append shouldn't work for improper lists (aka '(1 2 3 . 4), (cons 1 2), '("ab" . "c"), etc...)

(append '(1 2) '(1 2))
;(append '() '())
;(append '() '(1 2 3 4 5 6 #\a #\b #\c #\d))
;(append '(a s c d) '())

;(append '() '(1 2 3 4) '(1 2 3 4 5 6) '((123 123) 123) '(4))

