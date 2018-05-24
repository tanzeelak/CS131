(define (bind-vals a b) 
  (string->symbol
   (string-append
    (symbol->string a )
    "!"
           (symbol->string b ) ) )
  )

(define (pair-input-and-binded a b)
  (list
   a
   (bind-vals a b)
   )
  )


(define (create-bindings l1 l2)
  (cond
   [(and (equal? l1 '()) (equal? l2 '()) )
    (display "im empty\n")
    '()
    ] 
   [(not(equal? (car (car l1)) (car (car l2)) ) )
    (display "about to bind\n") ;about to bind
    (cons
     (pair-input-and-binded (car (car l1)) (car (car l2)))
     (create-bindings (cdr l1) (cdr l2) )
     )
    ]
   [else
    (display "else case\n")
    (create-bindings (cdr l1) (cdr l2) )
   ]
   )
  )

(define (match-eval x y)
  (display "match-eval")
  (cond 
  [(equal? x y) 
   (display `(x and y are equal: ,x))
   (display "\n")
   x
   ]
  [else
   (list `(if % ,(car x) ,(car y)) )
   ]
  )
  )

(define (match-eval2 x y)
  (display "match-eval")
  (cond
   [(equal? x y)
    (display `(x and y are equal: ,x))
    (display "\n")
    x
    ]
   [else
    `(if % ,x ,y)
    ]
   )
  )

(define (match-var a b)
  (display `(a = ,a))
  (display "\n")
  (display `(b = ,b))
  (display "\n")
  (cond
   [(not (equal? a b ) )
    (string->symbol
     (string-append
      (symbol->string a )
      "!"
             (symbol->string b ) ) )
    ]
   [else
    a
    ]
   )
  )

(define (def-pair def1 def2)
  (display `(def1 = ,def1))
  (display "\n")
  (display `(def2 = ,def2))
  (display "\n")
  (cons
   (match-var (car def1) (car def2))
   (match-eval (cdr def1) (cdr def2))
   )
  )

(define (def-inside l1 l2)
  (display `(def1 = ,l1))
  (display "\n")
  (display `(def2 = ,l2))
  (display "\n")
  (cond 
   [(and (equal? l1 '()) (equal? l2 '()) )
    `()
    ]
   [else
   (cons 
    (def-pair (car l1) (car l2))
    (def-inside (cdr l1) (cdr l2))
    )
	
    ]
   )
  )

;https://stackoverflow.com/questions/15479490/member-function-in-racket
(define (member? item seq)
  (sequence-ormap (lambda (x)
		    (equal? item x))
		  seq))

(define (is-input-in-mapping a mappings)
  (display `(a is : ,a))
  (display "\n")
  (display `(mappings is : ,mappings))
  (display "\n")
  (cond
   [(equal? mappings '())
    (display "finished looking through mappings\n")
    '()
    ]
   [(member? a (car mappings))
    (display "is a member\n")
 ;   (car
     (cdr (car mappings))
;     )
    ]
   [else
    (display "looking through next mapping\n")
    (is-input-in-mapping a (cdr mappings) )
    ]
   )
  )


(define (iterate-thru-body l1 l2  mappings)
  (display "i've entered interate-thru-body\n")
  (display `(l1: ,l1 ) )
  (display "\n")
  (display `(l2: ,l2 ) )
  (display "\n")
  (cond
   [(or (equal? l1 '()) (equal? l2 '()))
    (display "finished iterating through bodies\n")
    '()
    ]
   [else
    (display "still iterating thru body\n")
    (cond
     [(equal? (assoc (car l1) mappings) #f) ;we didn't find it in the map
      (display "we didn't find it in the map\n")
      (display `(l1 here: ,(car l1)))
      (display "\n")
      (display `(l2 here: ,(car l2)))
      (display "\n")
      (cons 
       (match-eval2 (car l1) (car l2))
       (iterate-thru-body (cdr l1) (cdr l2) mappings) 
       )
      ]
     [else ;we found it in the map
      (display "we foudn it in the map \n")
      (cons 
       (assoc (car l1) mappings)
       (iterate-thru-body (cdr l1) (cdr l2) mappings)
       )
      ]
     
     )
    ]
  )
  )

(define (func-body l1 l2 mappings)
  (display "ive entered func body\n")
  (display `(l1 is actually: ,l1))
  (display "\n")
  (display `(l2 is actually: ,l2))
  (display "\n")

  (cond
   [(not (list? l1))
    (display "it's not a list\n")
    (cond
     [(equal? (assoc l1 mappings) #f )
      (display "we didn't find it")
      (match-eval l1 l2)
      ]
     [else ;we found it in the map 
      (display "we found it in the map\n")
      (car (cdr (assoc l1 mappings) ))
      ]
     )
    ]
   [
    (display "is a list\n")
    (iterate-thru-body l1 l2 mappings)
   ]
  )
  )

(define (let-inside l1 l2)
  (display `(look at my bindings: ,(create-bindings (car l1) (car l2))) )
  (display "\n")
  (list
   `let
   (def-inside (car l1) (car l2))
   (func-body
    (car (cdr l1) )
    (car (cdr l2) )
    (create-bindings (car l1) (car l2))
    )
    )
  )

;https://stackoverflow.com/questions/16720941/custom-function-for-length-of-a-list-in-scheme
(define (list-length lst)
  (cond ((null? lst) 0)
	(else (+ 1 (list-length (cdr lst))))))

(define (compare-list x y)
  (cond
   [(or (equal? x '()) (equal? y '()) ) ;base case: two empty lists so we return one too
    (display `(empty: x = ,x y = ,y))
    (display "\n")
    '()
    ]
   [(not (equal? (list-length x) (list-length y) )  ) ;if list lengths are diff stop
    `(if % ,x ,y)
    ]
   [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) ;dont recurse if quoted
    `(if % ,x ,y)
    ]
   [(or (and (equal? (car x) 'if) (not (equal? (car y) 'if) )) ;if one of the cases has an if statement but not the other, dont recurse
	(and (equal? (car y) 'if) (not (equal? (car x) 'if) ))
	)
    `(if % ,x ,y)
    ]
   [(and (boolean? (car x) )(boolean? (car y)) ) ;x and y are booleans to take care of
    (display "bools \n")
    (cons
     (if (car x) '% '(not %))
     (compare-list (cdr x) (cdr y))
     )
    ]
   [(equal? (car x) (car y)) ;head elements are equal so we recurse
    (display `(head elements are equal: car x = ,(car x) AND car y = ,(car y)))
    (display "\n")
    (cond
     [(equal? (car x) 'let)
      (display "LET CASE HERE \n")
      (let-inside (cdr x) (cdr y))
	      
      ]
     [else
      (cons (car x) (compare-list (cdr x) (cdr y)) )
      ]
     )
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
(assert (expr-compare '(cons (cons a b) (cons b c))
		      '(cons (cons a c) (cons a c)))
	'(cons (cons a (if % b c)) (cons (if % b a) c)))
(assert (expr-compare '(cons a b) '(list a b)) '((if % cons list) a b))
(assert (expr-compare '(list) '(list a)) '(if % (list) (list a)))
(assert (expr-compare ''(a b) ''(a c)) '(if % '(a b) '(a c)))
(assert (expr-compare '(quote (a b)) '(quote (a c))) '(if % '(a b) '(a c)))
(assert (expr-compare '(quoth (a b)) '(quoth (a c))) '(quoth (a (if % b c))))
(assert (expr-compare '(if x y z) '(if x z z)) '(if x (if % y z) z))
(assert (expr-compare '(if x y z) '(g x y z)) '(if % (if x y z) (g x y z)))
(assert (expr-compare '(let ((a 1)) (f a)) '(let ((a 2)) (g a))) '(let ((a (if % 1 2))) ((if % f g) a)))
(assert (expr-compare '(let ((a c)) a) '(let ((b d)) b)) '(let ((a!b (if % c d))) a!b))
(assert (expr-compare ''(let ((a c)) a) ''(let ((b d)) b)) '(if % '(let ((a c)) a) '(let ((b d)) b)))

					;)
