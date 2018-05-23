(define (compare-list x y)
  (cond
   ;need cases for when one or the other list is empty
   [(or (equal? x '()) (equal? y '()) ) ;base case: two empty lists so we return one too
    (display `(empty: x = ,x y = ,y))
    (display "\n")
    '()
    ]
   [(equal? (car x) (car y)) ;head elements are equal so we recurse
    (display `(head elements are equal: car x = ,(car x) AND car y = ,(car y)))
    (display "\n")
    (cons (car x) (compare-list (cdr x) (cdr y)) )
    ]
   [(or (list? (car x) ) (list? (car y) ) )  ;if list of lists
    (display `(list of lists: car x = ,(car x) AND car y = ,(car y)))
    (display "\n")
    (cons (expr-compare (car x) (car y)) (compare-list (cdr x) (cdr y) ))
    ]
   [else
    (display `(head elements not equal: car x =  ,(car x) AND car y = ,(car y)))
    (display "\n")
    (cons `(if % ,(car x) ,(car y)) (compare-list (cdr x) (cdr y)) )
    ]
   )
  )

(define (expr-compare x y)
  (cond
   [(equal? x y) ;base case: x and y are sole equal elements
    (display `(x and y are equal: ,x))
    (display "\n")
    x
    ]
   [(and (boolean? x)(boolean? y)) ;x and y are booleans to take care of
    (display "x and y are boolsz")
    (if x '% '(not %))
    ]
   [(and (list? x)(list? y)) ;x and y are both lists so we compare
    (display `(compare lists: ,x AND ,y))
    (display "\n")
    (compare-list x y)
    ]
   [(not (and (list? x) (list? y) ) ) ;x or y is a list but not both so we return the if case
    (display "one is not a list \n")
    `(if % ,x ,y)
    ]
   )
  )
