(define (compare-list x y)
  (cond
   ;need cases for when one or the other list is empty
   [(or (equal? x '()) (equal? y '()) ) ;base case: two empty lists so we return one too
    (display `(empty: x = ,x y = ,y))
    (display "\n")
    '()
    ]
   [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) ;dont recurse if quoted
    `(if % ,x ,y)
    ]
   [(or (equal? (car x) 'if) (equal? (car y) 'if))
    `(if % ,x ,y)
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
    (display "x and y are boolsz \n")
    (if x '% '(not %))
    ]
   [(not (and (list? x) (list? y) ) ) ;x or y is a list but not both so we return the if case
    (display "one is not a list \n")
    `(if % ,x ,y)
    ]
   [(and (list? x)(list? y)) ;x and y are both lists so we compare
    (display `(compare lists: ,x AND ,y))
    (display "\n")
    (compare-list x y)
    ]
   )
  )

(define-syntax-rule (assert output expected)
  (when (not (equal? output expected))
	(display "Assertion `") (display #'c) (displayln "' failed.")
	(display "  Result: ") (displayln output)
	    (display "Expected: ") (displayln expected)))


;(define (tests) 
(assert (expr-compare 12 12) 12)
(assert (expr-compare 12 20) `(if % 12 20))
(assert (expr-compare #t #t) '#t)
(assert (expr-compare #f #f) '#f)
(assert (expr-compare #t #f) '%)
(assert (expr-compare #f #t) '(not %))
(assert (expr-compare 'a '(cons a b)) '(if % a (cons a b)))
(assert (expr-compare '(cons a b) '(cons a b)) '(cons a b))
(assert (expr-compare '(cons a b) '(cons a c)) '(cons a (if % b c)))
;)
