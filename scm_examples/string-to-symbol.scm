'y
(define y (string->symbol "y")) ; bucket exists
(define s (string->symbol "s")) ; bucket doesn't exist
s
y
(eq? s y)

(eq? (string->symbol (symbol->string 'earth)) 'earth)
(eq? (string->symbol "earth") 'earth)
(eq? (string->symbol "earth") 'moon)
(eq? (string->symbol "earth") 'earth?)
(eq? (string->symbol "earth??") 'earth?)
