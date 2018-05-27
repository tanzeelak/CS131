(define (bind-vals a b) 
  (string->symbol
   (string-append
    (symbol->string a )
    "!"
           (symbol->string b ) ) )
  )

(define (pair-input-and-binded doIRev a b)
  (list
   (cond
    [(equal? doIRev #f) a ]
    [else b]    
    )
   (bind-vals a b)
   )
  )

(define (create-bindings doIRev l1 l2 pmap)
  (display "CREATE BINDINGS\n")
  (display l1)
  (display l2)
  (display "\n")
  (cond
   [(and (equal? l1 '()) (equal? l2 '()) )
    (display "im empty\n")
    pmap
    ]
   [(not(equal? (car (car l1)) (car (car l2)) ) )
    (display "about to bind\n")
    (cons
     (pair-input-and-binded doIRev (car (car l1)) (car (car l2)))
     (create-bindings doIRev (cdr l1) (cdr l2) pmap)
     )
    ]
   [else
    (display "else case\n")
    (display (cdr (car l1)))
    (display (cdr (car l2)))
    (display "\n")
    (create-bindings doIRev (cdr l1) (cdr l2) pmap)
   ]
   )
  )

(define (create-bindings2 doIRev l1 l2 pmap)
  (display "CREATE BINDINGS2\n")
  (display l1)
  (display l2)
  (display "\n")
  (cond
   [(and (equal? l1 '()) (equal? l2 '()) )
    (display "im empty\n")
     pmap
    ]
   [(not(equal? (car l1) (car l2) ) )
    (display "about to bind\n")
    (cons
     (pair-input-and-binded doIRev (car l1) (car l2))
     (create-bindings2 doIRev (cdr l1) (cdr l2) pmap)
     )
    ]
   [else
    (display "else case\n")
    (create-bindings2 doIRev (cdr l1) (cdr l2) pmap)
    ]
   )
  )

(define (handle-list-calls x y mappings1 mappings2)
  (display "handle-list-calls\n")
  (display `(x = ,x))
  (display "\n")
  (display `(y = ,y))
  (display "\n")
  (cond
   [(or (equal? x '() ) (equal? y '() ) )
    '()
    ]
   [(or (list? x) (list? y))
    (cond
     [(or (list? (car x)) (list? (car y)) )
      (display "list of lists\n")
      (handle-list-calls (car x) (car y) mappings1 mappings2)
      ]
     [else
      (display "this is the final list\n")
      (compare-list x y mappings1 mappings2)
      ]
     )
    ]
   )
  )

(define (match-eval x y mappings1 mappings2)
  (display "match-eval\n")
  (display `(mappings1 = ,mappings1 mappings2 = ,mappings2))
  (display "\n")
  (display `(x = ,(car x) y = ,(car y)) )
  (cond 
  [(equal? (car x) (car y)) x]
  [(or (list? (car x)) (list? (car y)))
   (display "\n One OF THESE IS ALIST\n")
   (handle-list-calls (car x) (car y) mappings1 mappings2)
   ]
  [else
   (list `(if % ,(car x) ,(car y)) )
   ]
  )
  )

(define (match-eval2 x y mappings1 mappings2)
  (display "match-eval2\n")
  (display `(x = ,x y = ,y))
  (display "\n")
  (cond
   [(equal? x y) x]
   [(or (list? x) (list? y)) ;comment this out if it fails in the final
    (display "\n YO LAMBDA HAS  LIST BOI IN THE PAIR\n")
    (handle-list-calls x y mappings1 mappings2)
    ]
   [else
    `(if % ,x ,y)
    ]
   )
  )

(define (match-var a b)
  (display "match-var\n")
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
   [else a ]
   )
  )


(define (match-outer-var a b mappings1 mappings2)
  (display "match-outer-var\n")
  (display `(a = ,a))
  (display "\n")
  (display `(b = ,b))
  (display "\n")
  (cond
   [(or (equal? (assoc a mappings1) #f) (equal? (assoc b mappings2) #f))
   (display "at least one was not found in the map\n")
    (cond
     [(and (equal? (assoc a mappings1) #f) (equal? (assoc b mappings2) #f))
      (display "neither val was found in teh map so we just bind\n")
      (match-var a b)
      ]
    )
    ]
   [(or (not (equal? (assoc a mappings1) #f)) (not (equal? (assoc b mappings2) #f)))
    (display "both were found in the map\n")
    (display `(assoc a mappings1 = ,(assoc a mappings1) ))
    (display "\n")
    (display `(assoc b mappings2 = ,(assoc b mappings2) ))
   (display "\n")
    (cond
     [(equal? (cdr (assoc a mappings1)) (cdr (assoc b mappings2)) )
      (display "they are mapped to the same val so we return that val\n")
     (display (cdr (assoc a mappings1)))
     (car (cdr (assoc a mappings1)))
     ]
     [else
      (display "they are mapped to different vals so we % the vals")
      (match-eval (cdr (assoc a mappings1)) (cdr (assoc b mappings2)) mappings1 mappings2)
      ]
     )
    ]
  )
  )

(define (def-pair islambda def1 def2 mappings1 mappings2)
  (display "DEFPAIR\n")
  (display `(def1 = ,(cdr def1)))
  (display "\n")
  (display `(def2 = ,(cdr def2)))
  (display "\n")
  (cons
   (match-outer-var (car def1) (car def2) mappings1 mappings2)
;   (match-var (car def1) (car def2))
   (match-eval (cdr def1) (cdr def2) mappings1 mappings2)
   )
  )

(define (def-pair2  def1 def2 mappings1 mappings2)
  (display "DEF-PAIR2\n")
  (display `(mappings1 = ,mappings1))
  (display "\n")
  (display `(mappings2 = ,mappings2))
  (display "\n")
  (display `(def1 = ,def1))
  (display "\n")
  (display `(def2 = ,def2))
  (display "\n")
  (cond
   [(equal? def1 '())
    (display "it is empty\n")
    `()
    ]
   [else
    (display "else in def pair\n")
    (cons
     (match-outer-var (car def1) (car def2) mappings1 mappings2)
 ;    (match-var (car def1) (car def2) )
     (def-pair2 (cdr def1) (cdr def2) mappings1 mappings2)
     )
    ]
   )
  )

(define (def-inside l1 l2 mappings1 mappings2)
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
    (def-pair #f (car l1) (car l2) mappings1 mappings2)
    (def-inside (cdr l1) (cdr l2) mappings1 mappings2)
    )
    ]
   )
  )

(define (iterate-thru-body l1 l2  mappings1 mappings2)
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
     [(equal? (assoc (car l1) mappings1) #f) 
      (display "we didn't find it in the map\n")
      (display `(l1 here: ,(car l1)))
      (display "\n")
      (display `(l2 here: ,(car l2)))
      (display "\n")
      (cons 
       (match-eval2 (car l1) (car l2) mappings1 mappings2)
       (iterate-thru-body (cdr l1) (cdr l2) mappings1 mappings2) 
       )
      ]
     [else 
      (display "we foudn it in the map \n")
      (cons
       (car (cdr (assoc (car l1) mappings1) ) )
       (iterate-thru-body (cdr l1) (cdr l2) mappings1 mappings2)
       )
      ]
     )
    ]
  )
  )

(define (func-body l1 l2 mappings1 mappings2)
  (display "ive entered func body\n")
  (display `(l1 is actually: ,l1))
  (display "\n")
  (display `(l2 is actually: ,l2))
  (display "\n")
  (display `(mappings1: ,mappings1))
  (display "\n")
  (display `(mappings2: ,mappings2))
  (display "\n")
  (cond
   [(not (list? l1))
    (display "it's not a list\n")
    (cond
     [(equal? (assoc l1 mappings1) #f )
      (display "we didn't find it\n")
      (match-eval l1 l2)
      ]
     [(equal? (cdr (assoc l1 mappings1)) (cdr (assoc l2 mappings2)))
      (display "we found it in the map and they match\n")
      (car (cdr (assoc l1 mappings1) ))
      ]
     [(not (equal? (cdr (assoc l1 mappings1)) (cdr (assoc l2 mappings2)) ) )
      (display "LOL NEED TO IMPLEMENT")
      (display "we found them in the maps but they have different bindings")
      ]
     )
    ]
   [
    (display "is a list\n")
    (iterate-thru-body l1 l2 mappings1 mappings2)
   ]
  )
  )

(define (func-body2 l1 l2 mappings1 mappings2)
  (display "ive entered func body\n")
  (display `(l1 is actually: ,l1))
  (display "\n")
  (display `(l2 is actually: ,l2))
  (display "\n")
  (display `(mappings1: ,mappings1))
  (display "\n")
  (display `(mappings2: ,mappings2))
  (display "\n")
  (cond
   [(or (equal? l1 '()) (equal? l2 '()) )
    (display "\n we've reached an empty lsit\n")
    '()
    ]
   [(or (equal? (car l1) 'let) (equal? (car l1) 'lambda) )
    (display "\n WE FOUND A FUKCIN LET INSIDE THE BODY\n")
    (compare-list l1 l2 mappings1 mappings2)
    ]
   [(or (equal? (assoc (car l1) mappings1) #f) (equal? (assoc (car l2) mappings2) #f))
    (display "\n one was not found in the map\n")
    (display `( car l1 = ,(car l1) car l2 = ,(car l2)))
    (display "\n")
    (display `(carl l1 mapping =  ,(assoc (car l1) mappings1) car l2 mapping =  ,(assoc (car l2) mappings2) ))
    (display "\n")
    (cond
     [(and (equal? (assoc (car l1) mappings1) #f) (equal? (assoc (car l2) mappings2) #f))
      (display "\n neither was found in the map\n")
      (cons 
       (match-eval2 (car l1) (car l2) mappings1 mappings2)
       (func-body2 (cdr l1) (cdr l2) mappings1 mappings2)
       )
      ]
     [(not (assoc (car l1) mappings1))
      (display "\nthe first was not found in the map\n")
      (cons
       (match-eval2 (car l1) (car (cdr (assoc (car l2) mappings2))) mappings1 mappings2)
       (func-body2 (cdr l1) (cdr l2) mappings1 mappings2)
       )
      ]
     [(not (assoc (car l2) mappings2))
      (display "the second was not found in the map\n")
      (cons
       (match-eval2 (car (cdr (assoc (car l1) mappings1))) (car l2) mappings1 mappings2)
       (func-body2 (cdr l1) (cdr l2) mappings1 mappings2)
       )
      ]
     )
    ]
   [(and (assoc (car l1) mappings1) (assoc (car l2) mappings2))
    (display "both were in the map\n")
    (cond
     [(equal? (cdr (assoc (car l1) mappings1)) (cdr (assoc (car l2) mappings2)))
      (display "they are mapped to the same value, so we return that value\n")
      (display (cdr (assoc (car l1) mappings1)))
      (display "\n")
      (cons 
       (car (cdr (assoc (car l1) mappings1)) )
       (func-body2 (cdr l1) (cdr l2) mappings1 mappings2)
       )
      ]
     [(not (equal? (cdr (assoc (car l1) mappings1)) (cdr (assoc (car l2) mappings2))))
      (display "they are not mapped to the same value so we gotta do percento on that mapping\n")
      (display (cdr (assoc (car l1) mappings1)))
      (display "\n")
      (display (cdr (assoc (car l2) mappings2)))
      (display "\n")
      (cons
       (match-eval2 (cdr (assoc (car l1) mappings1)) (cdr (assoc (car l2) mappings2)) mappings1 mappings2)
       (func-body2 (cdr l1) (cdr l2) mappings1 mappings2)
       )
      ]
     )
    ]
   )
  )

(define (let-inside l1 l2 pmap1 pmap2)
  (display `(PMAP1 = ,pmap1 PMAP2 = ,pmap2))
  (display `(LOOK AT my bindings: ,(create-bindings #f (car l1) (car l2) pmap1) ) )
  (display `(LOOK AT my bindings: ,(create-bindings #t (car l1) (car l2) pmap2) ) )
  (display "\n")
  (display `(funcbod1 is: ,l1) )
  (display "\n")
  (display `(funcbod2 is: ,l2) )
  (display "\n FUNC BODS ABOVE ME \n")
  ;; may need to change
  (list
   `let
   (def-inside
     (car l1)
     (car l2)
     (create-bindings #f (car l1) (car l2) pmap1)
     (create-bindings #t (car l1) (car l2) pmap2)
     )
   (func-body
    (car (cdr l1) )
    (car (cdr l2) )
    (create-bindings #f (car l1) (car l2) pmap1)
    (create-bindings #t (car l1) (car l2) pmap2)
    )
    )
  )

(define (lambda-inside l1 l2 pmap1 pmap2)
  (display `(PMAP1 = ,pmap1 PMAP2 = ,pmap2))
  (display `(LOOK AT my bindings: ,(create-bindings2 #f (car l1) (car l2) pmap1)  ) )
  (display `(LOOK AT my bindings: ,(create-bindings2 #t (car l1) (car l2) pmap2)  ) )
  (display "\n")
  (display `(def1 is: ,l1) )
  (display "\n")
  (display `(def2 is: ,l2) )
  (display "\n FUNC BODS ABOVE ME \n")
  ;; LOL I NEVER APPLY THE PREV BIDNIGNS TO DEF-PAIR
  (list
   `lambda
   (def-pair2
     (car l1)
     (car l2)
     (create-bindings2 #f (car l1) (car l2) pmap1)
     (create-bindings2 #t (car l1) (car l2) pmap2)
     )
   (func-body2
    (car (cdr l1))
    (car (cdr l2))
    (create-bindings2 #f (car l1) (car l2) pmap1)
    (create-bindings2 #t (car l1) (car l2) pmap2)
    )
   )
  )

;https://stackoverflow.com/questions/16720941/custom-function-for-length-of-a-list-in-scheme
(define (list-length lst)
  (cond ((null? lst) 0)
	(else (+ 1 (list-length (cdr lst))))))

(define (compare-list x y pmap1 pmap2)
  (cond
   [(or (equal? x '()) (equal? y '()) ) 
    (display `(empty: x = ,x y = ,y))
    (display "\n")
    '()
    ]
   [(not (equal? (list-length x) (list-length y) )  ) 
    `(if % ,x ,y)
    ]
   [(or (equal? (car x) 'quote) (equal? (car y) 'quote)) 
    `(if % ,x ,y)
    ]
   [(or (and (equal? (car x) 'if) (not (equal? (car y) 'if) )) 
	(and (equal? (car y) 'if) (not (equal? (car x) 'if) ))	)
    `(if % ,x ,y)
    ]
   [(and (boolean? (car x) )(boolean? (car y)) )
    (display "bools \n")
    (cons
     (if (car x) '% '(not %))
     (compare-list (cdr x) (cdr y) pmap1 pmap2)
     )
    ]
   [(equal? (car x) (car y))
    (display `(head elements are equal: car x = ,(car x) AND car y = ,(car y)))
    (display "\n")
    (cond
     [(equal? (car x) 'let)
      (display "LET CASE HERE \n")
      (let-inside (cdr x) (cdr y) pmap1 pmap2)
      ]
     [(equal? (car x) 'lambda)
      (display "LAMBDA CASE HERE\n")
      (lambda-inside (cdr x) (cdr y) pmap1 pmap2)
      ]
     [else
      (cons (car x) (compare-list (cdr x) (cdr y) pmap1 pmap2) )
      ]
     )
    ]
   [(or (list? (car x) ) (list? (car y) ) ) 
;    (display `(list of lists: car x = ,(car x) AND car y = ,(car y)))
;    (display "\n")
    (cons (expr-compare (car x) (car y)) (compare-list (cdr x) (cdr y) pmap1 pmap2))
    ]
   [else
    (display `(head elements not equal: car x =  ,(car x) AND car y = ,(car y)))
    (display "\n")
    (cons `(if % ,(car x) ,(car y)) (compare-list (cdr x) (cdr y) pmap1 pmap2))
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
   [(not (and (list? x) (list? y) ) ) 
    `(if % ,x ,y)
    ]
   [(and (list? x)(list? y)) 
    (compare-list x y '() '())
    ]
   )
  )

(define-syntax-rule (assert output expected)
  (when (not (equal? output expected))
	(display "Assertion `") (display #'c) (displayln "' failed.")
	(display "  Result: ") (displayln output)
	    (display "Expected: ") (displayln expected)))

; basically does the reverse.
; checks the equality of x and when expr-compare is forced to evaluate to true whenever a % is seen
; checks the equality of y and when expr-compare is forced to evaluate to false whenever a % is seen
; both of these must be equal to result in true
(define (test-expr-compare x y)
  (and (equal? (eval x) (eval (list 'let '((% #t)) (expr-compare x y)) ) )
       (equal? (eval y) (eval (list 'let '((% #f)) (expr-compare x y)) ) )
       )
  )

; to test: (expr-compare test-expr-x test-expr-y) 
(define test-expr-x
  '(
    ((lambda (a b) (f a b)) 1 2)
    #f
    a
    (quote (a b c))
    (+ 3 (let ((a 1) (b 2)) (list a b)))
    )
  )
(define test-expr-y
  '(
    ((lambda (a c) (f c a)) 1 2)
    #t
    (cons c hehe)
    (quote (a b c))
    (+ 2 (let ((a 1) (c 2)) (list a c)))
    )
    )


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
(assert (expr-compare '(+ #f (let ((a 1) (b 2)) (f a b)))
		      '(+ #t (let ((a 1) (c 2)) (f a c)))) '(+
							     (not %)
							     (let ((a 1) (b!c 2)) (f a b!c))))
(assert (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2)) '((lambda (a) ((if % f g) a)) (if % 1 2)))
(assert (expr-compare '((lambda (a b) (f a b)) 1 2)
		      '((lambda (a b) (f b a)) 1 2))
	'((lambda (a b) (f (if % a b) (if % b a))) 1 2))
(assert (expr-compare '((lambda (a b) (f a b)) 1 2)
		      '((lambda (a c) (f c a)) 1 2))
	'((lambda (a b!c) (f (if % a b!c) (if % b!c a)))
	       1 2))


'(let
     (( a (lambda (b a) (b a)) ))
   (eq? a
	(
	 (lambda (a b)
	   (let ((a b) (b a)) (a b))
		 )
	 a
	 (lambda (a) a)
	 )
	)
   )
