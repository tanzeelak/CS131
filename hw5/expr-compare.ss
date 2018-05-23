(define (compare-list x y)
  (cond
   [(equal? (car x) (car y))
    (display "first y \n")
    (compare-list (cdr x) (cdr y))
   ]
   [else
    (display "else \n")
    `(if % ,(car x) ,(car y))
;    (first y)
    ]
   )
  )

(define (expr-compare x y)
  (cond
   [(equal? x y)
    x
    ]
   [(and (boolean? x)(boolean? y))
    (if x '% '(not %))
    ]
   
   [(and (list? x)(list? y))
    (compare-list x y)
    ]
   )
  )
