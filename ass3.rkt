#lang typed/racket

(require typed/rackunit)

;I was able to make all my test-cases pass so Full project implemented

;JYSS4
;Arith language top-definitons
(define-type ExprC (U numC binop leq0? idC FuncallC))
(struct numC ([n : Real])#:transparent)
(struct binop ([operator : Sexp] [l : ExprC] [r : ExprC])#:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct FuncallC ([fname : Symbol] [paramVals : (Listof ExprC)])#:transparent)

(struct FundefC ([fname : Symbol] [param : Symbol] [body : ExprC])#:transparent)

;function that maps binop symbol to arithetic operation
(define (arithmeticOperator [s : Sexp] [l : Real] [r : Real]) : Real
  (match s
    ['+ (+ l r)]
    ['* (* l r)]
    ['- (- l r)]
    ['/ (cond
          [(not (zero? r)) (/ l r)]
          [else (error 'JYSS "division by 0")])])
  )

;substition function for all expressions with values
(define (subst [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(FuncallC f a) (FuncallC f (list (subst what for (first a))))]
    [(binop op l r) (binop op (subst what for l) (subst what for r))]
    [(leq0? a b c) (leq0? (subst what for a) (subst what for b) (subst what for c))])
)

;test-case
(check-equal? (subst (numC 3) 'x (binop '+ (idC 'x) (idC 'y))) (binop '+ (numC 3) (idC 'y)))
(check-equal? (subst (numC 3) 'x (FuncallC 'f (list (idC 'x)))) (FuncallC 'f (list (numC 3))))
(check-equal? (subst (numC 3) 'x (FuncallC 'f (list (idC 'y)))) (FuncallC 'f (list (idC 'y))))
(check-equal? (subst (numC 3) 'x (numC 6)) (numC 6))

;gets the fundefiniton I want to substite with for my funcalls body
(define (get-fundef [n : Symbol] [fds : (Listof FundefC)]) : FundefC
  (cond
    [(empty? fds)
     (error 'JYSS "reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FundefC-fname (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))

;test-case
(check-equal? (get-fundef 'B (list (FundefC 'A 'x (numC 5)) (FundefC 'B 'y (numC 6)))) (FundefC 'B 'y (numC 6)))
(check-exn (regexp (regexp-quote "reference to undefined function"))
           (lambda () (get-fundef 'C (list (FundefC 'A 'x (numC 5)) (FundefC 'B 'y (numC 6))))))

;helper function for interpreting funcalls (eager property of interpreting args first)
(define (interpret-funcalls [fname : Symbol] [paramV : ExprC] [lstfunc : (Listof FundefC)]) : Real
  (define fn (get-fundef fname lstfunc))
  (interp (subst (numC (interp paramV lstfunc)) (FundefC-param fn)  (FundefC-body fn)) lstfunc))

;helper function to create listof of exprC from args
(define (my-params [s : (Listof Any)]) : (Listof ExprC)
  (cast s (Listof ExprC)))

;the evaluator/interpreter which uses listof function definitions to evaluate expressions
(define (interp [exp : ExprC] [lstfuncd : (Listof FundefC)]) : Real
  (match exp
      [(numC n) n]
      [(idC s) (error 'JYSS "undefined variable error ~e" s)]
      [(leq0? a b c) (if (<= (interp a lstfuncd) 0) (interp b lstfuncd) (interp c lstfuncd))]
      [(binop o l r) (arithmeticOperator o (interp l lstfuncd) (interp r lstfuncd))]
      [(FuncallC (? symbol? f) (list args ...))
       (cond
          [(equal? 1 (length (my-params args)))(interpret-funcalls f (first (my-params args)) lstfuncd)]
          [else (error 'JYSS "bad things happened more than 1 arg")])]
    ))

;test-case
(check-equal? (interp  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                                                               (binop '- (binop '* (numC 2) (numC 3)) (numC 3))) '()) 4)
(check-equal? (interp (leq0? (numC 4) (numC 4) (numC 800)) '()) 800)
(check-equal? (interp (leq0? (numC -1) (numC 7) (numC -1)) '()) 7)
(check-equal? (interp (FuncallC 'f (list (numC 3))) (list (FundefC 'f 'x (binop '+ (idC 'x) (numC 1))))) 4)
(check-exn (regexp (regexp-quote "division by 0"))
           (lambda () (interp (binop '/ (numC 5) (numC 0)) '())))
(check-exn (regexp (regexp-quote "undefined variable error"))
           (lambda () (interp (idC 'y) '())))

;Finds and interprets the function named main with a param of value 0 from the function definitions
(define (interp-fns [lstfuncd : (Listof FundefC)]) : Real

  (match (get-fundef 'main lstfuncd)
    [(FundefC 'main 'init exp) (interp (FuncallC 'main (list (numC 0))) lstfuncd)])
  )

;test-case
(check-equal? (interp-fns (list (FundefC 'main 'init (binop '+ (numC 4) (numC 4))))) 8)

;helper function to check if symbol is a valid id
(define (valid-ID? [s : Symbol]) : Boolean
  (if (member s '(+ - * / leq0? fn)) #f #t)
  )

;helper function to determine the type that an sexp should return
(define (my-type [s : Sexp]) : Symbol
  (match s
    [(? symbol? s) 'id]
    [(? real? r) 'num]
    [(? list? l)
     (cond
       [(equal? (first l) 'leq0?) 'leq0?]
       [(member (first l) '(+ - * /)) 'binop]
       [else 'function])
     ]
    [other (error 'JYSS "invalid Sexpression")])
  )

;test-case
(check-equal? (my-type '(leq0? x y g)) 'leq0?)
(check-equal? (my-type '(f x y g)) 'function)
(check-equal? (my-type 'u) 'id)
(check-equal? (my-type '2) 'num)
(check-equal? (my-type '(+ e 5 f)) 'binop)

;parse concrete JYSS syntax to expressions for interpreter to understand
(define (parse [s : Sexp]) : ExprC
    (match (my-type s)
 ;     [(? symbol? s) (if (valid-ID? s) (idC s) (error 'JYSS "cannot use a reserved symbol must use valid ID"))]
 ;    [(? real? s) (numC s)]
 ;     [(list (? symbol? a) (? symbol? b) ...) (cond
 ;                               [(equal? (length (cast b (Listof Sexp))) 0) (FuncallC a (list))]
 ;                               [(> (length (cast b (Listof Sexp))) 0) (FuncallC a (map parse (cast b (Listof Sexp))))]
 ;                               [else (error 'JYSS "not parsing correctly")])]
 ;     [(? list? l)
 ;      (match l
 ;        [(list op b c) (case op
 ;                        [(+ - * /) (binop op (parse b) (parse c))]
 ;                        [else (error 'JYSS "symbol not found")])]
 ;        [(list 'leq0? b c d) (leq0? (parse b) (parse c) (parse d))]
 ;        [other (error 'JYSS "not a parsible expression")])
      
      ['id (match s
             [(? symbol? a) (if (valid-ID? a) (idC a) (error 'JYSS "cannot use a reserved symbol must use valid ID ~e" s))])]
      ['num (match s
              [(? real? r) (numC r)])]
      ['function (match s
                   [(list (? symbol? a) b ...) (cond
                                                             [(equal? (length (cast b (Listof Sexp))) 0) (FuncallC a (list))]
                                                             [(> (length (cast b (Listof Sexp))) 0) (FuncallC a (map parse (cast b (Listof Sexp))))]
                                                             [else (error 'JYSS "not parsing correctly ~e" s)])]
                   [other (error 'JYSS "invalid function call syntax ~e" s)])]
      ['binop (match s
                [(list op l r)  (case op
                                  [(+ - * /) (binop op (parse l) (parse r))]
                                  [else (error 'JYSS "symbol not found ~e" s)])]
                [other (error 'JYSS "invalid binop syntax ~e" s)])]
      ['leq0? (match s
                [(list 'leq0? a b c) (leq0? (parse a) (parse b) (parse c))])]
      ))

;test-case
(check-equal? (parse '{+ {* 1 {/ 2 2}} {- (* 2 3) 3}})  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                                                               (binop '- (binop '* (numC 2) (numC 3)) (numC 3))))
(check-equal? (parse '{leq0? 4 5 5}) (leq0? (numC 4) (numC 5) (numC 5)))
(check-equal? (parse '{fn x}) (FuncallC 'fn (list (idC 'x))))
(check-equal? (parse '{fn x y}) (FuncallC 'fn (list (idC 'x) (idC 'y))))
(check-equal? (parse '{^ 3 4}) (FuncallC '^ (list (numC 3) (numC 4))))
(check-exn (regexp (regexp-quote "invalid binop syntax"))
           (lambda () (parse '{/ 3 4 5})))
(check-exn (regexp (regexp-quote "invalid function call syntax"))
           (lambda () (parse '{7 3 4 5})))
(check-exn (regexp (regexp-quote "cannot use a reserved symbol must use valid ID"))
           (lambda () (parse '(+ + 3))))

;parsing concrete JYSS syntax to function definitions for interpreter
(define (parse-fundef [s : Sexp]) : FundefC
     (match s
       [(list 'fn (list fun-name arg-name) exp) (match (second s)
                       [(list (? symbol? a) (? symbol? b))
                        (if (and (valid-ID? a) (valid-ID? b))
                            (FundefC a b (parse (third s)))
                            (error 'JYSS "not valid ID"))])]
       [other (error 'JYSS "invalid function definition syntax")])
  )

;test-case
(check-equal? (parse-fundef '{fn {addone x} {+ 4 1}}) (FundefC 'addone 'x (binop '+ (numC 4) (numC 1))))
(check-exn (regexp (regexp-quote "invalid function definition syntax"))
           (lambda () (parse-fundef '(house))))
(check-exn (regexp (regexp-quote "invalid function definition syntax"))
           (lambda () (parse-fundef 6)))
(check-exn (regexp (regexp-quote "not valid ID"))
           (lambda () (parse-fundef '(fn (+ x) 13))))



;helper function to create a list of fnames from all the function definitions we can use to check for dups
(define (all-fnames [lst : (Listof FundefC)]) : (Listof Symbol)
  (match lst
    ['() '()]
    [(cons f r) (cons (FundefC-fname f) (all-fnames r))])
  )

;test-case
(check-equal? (all-fnames (list (FundefC 'f 'x (numC 4)) (FundefC 'z 'e (numC 4)))) '(f z))

;parsing JYSS syntax to a list of function definitions 
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons f r) (cond
                 [(list? f) (cons (parse-fundef f) (parse-prog r))]
                 [else (error 'JYSS "no valid fundef found")])]
    )
 )

;test-case
(check-equal? (parse-prog '{{fn {f x} {+ x 14}} {fn {main init} {+ x 14}}})
              (list (FundefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                    (FundefC 'main 'init (binop '+ (idC 'x) (numC 14)))))
(check-exn (regexp (regexp-quote "no valid fundef found"))
           (lambda () (parse-prog '{something})))

;Combines parsing and interpretation be able to evaluate JYSS3
(define (top-interp [s : Sexp]) : Real
  (cond
   [(equal? #f (check-duplicates (all-fnames (parse-prog s)))) (interp-fns (parse-prog s))]
   [else (error 'JYSS "we have duplicate function definitions")]
   ))

;test-case
;(check-equal? (top-interp '{{fn {f x} {+ x 2}} {fn {g x} {+ x 2}} {fn {main init} {f {g 11}}}}) 15)
(check-equal? (top-interp (quote ((fn (main init) (leq0? (* 3 1) 3 (+ 2 9)))))) 11)
(check-exn (regexp (regexp-quote "we have duplicate function definitions"))
           (lambda () (top-interp '{{fn {f x} {+ x 2}} {fn {f x} {+ x 2}} {fn {main init} {f {g 11}}}})))


