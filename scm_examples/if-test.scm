(if #t #t #f)

(if #t
    (if (if #f #f #f)
        (if #t
            (if (if (if #f #t #f)
                    #t
                    #f)
                #t
                #t)
            (if #f #f #t))
        (if #t #t #t))
    #t)

(if #t
    (if (if #f #f #f)
        (if #t
            (if (if (if #f #t #f)
                    #t
                    #f)
                #t
                #t)
            (if #f #f #t))
        (if #t #t #t))
    #f)