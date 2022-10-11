#lang typed/racket

(require typed/rackunit)

;TODO - JYSS3
;Will also need to check that multiple functions do not have the same name in list of function definitions

;Arith language top-definitons
(define-type ExprC (U numC binop leq0? idC FuncallC))
(struct numC ([n : Real])#:transparent)
(struct binop ([operator : Sexp] [l : ExprC] [r : ExprC])#:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct FuncallC ([fname : Symbol] [paramVal : ExprC])#:transparent)

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
    [(FuncallC f a) (FuncallC f (subst what for a))]
    [(binop op l r) (binop op (subst what for l) (subst what for r))])
)

;test-case
(check-equal? (subst (numC 3) 'x (binop '+ (idC 'x) (idC 'y))) (binop '+ (numC 3) (idC 'y)))
(check-equal? (subst (numC 3) 'x (FuncallC 'f (idC 'x))) (FuncallC 'f (numC 3)))
(check-equal? (subst (numC 3) 'x (FuncallC 'f (idC 'y))) (FuncallC 'f (idC 'y)))
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

;evaluator/interpreter
(define (interp [exp : ExprC] [lstfuncd : (Listof FundefC)]) : Real
  (match exp
      [(numC n) n]
      [(idC s) (error 'JYSS "undefined variable error ~e" s)]
      [(leq0? a b c) (if (<= (interp a lstfuncd) 0) (interp b lstfuncd) (interp c lstfuncd))]
      [(binop o l r) (arithmeticOperator o (interp l lstfuncd) (interp r lstfuncd))]
      [(FuncallC f paramValue)(interpret-funcalls f paramValue lstfuncd)]
    ))

;test-case
(check-equal? (interp  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                                                               (binop '- (binop '* (numC 2) (numC 3)) (numC 3))) '()) 4)
(check-equal? (interp (leq0? (numC 4) (numC 4) (numC 800)) '()) 800)
(check-equal? (interp (leq0? (numC -1) (numC 7) (numC -1)) '()) 7)
(check-equal? (interp (FuncallC 'f (numC 3)) (list (FundefC 'f 'x (binop '+ (idC 'x) (numC 1))))) 4)
(check-exn (regexp (regexp-quote "division by 0"))
           (lambda () (interp (binop '/ (numC 5) (numC 0)) '())))
(check-exn (regexp (regexp-quote "undefined variable error"))
           (lambda () (interp (idC 'y) '())))

;Finds and interprets the function named main with a param of value 0 from the function definitions
(define (interp-fns [lstfuncd : (Listof FundefC)]) : Real

  (match (get-fundef 'main lstfuncd)
    [(FundefC 'main 'init exp) (interp (FuncallC 'main (numC 0)) lstfuncd)])
  )

;test-case
(check-equal? (interp-fns (list (FundefC 'main 'init (binop '+ (numC 4) (numC 4))))) 8)

;parse concrete JYSS syntax to expressions for interpreter to understand
(define (parse [s : Sexp]) : ExprC
    (match s
      [(? symbol? s) (if (member s '(+ - * /)) (error 'JYSS "an operand cannot be any of above") (idC s))]
      [(? real? s) (numC s)]
      [(list (? symbol? a) b) (FuncallC a (parse b))]
      [(? list? l)
       (match l
         [(list op b c) (case op
                         [(+ - * /) (binop op (parse b) (parse c))]
                         [else (error 'JYSS "symbol not found")])]
         [(list 'leq0? b c d) (leq0? (parse b) (parse c) (parse d))]
         [other (error 'JYSS "not a parsible expression")])
               ]
      ))

;test-case
(check-equal? (parse '{+ {* 1 {/ 2 2}} {- (* 2 3) 3}})  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                                                               (binop '- (binop '* (numC 2) (numC 3)) (numC 3))))
(check-equal? (parse '{leq0? 4 5 5}) (leq0? (numC 4) (numC 5) (numC 5)))
(check-equal? (parse '{fn x}) (FuncallC 'fn (idC 'x)))
(check-exn (regexp (regexp-quote "not a parsible expression"))
           (lambda () (parse '{/ 3 4 5})))
(check-exn (regexp (regexp-quote "symbol not found"))
           (lambda () (parse '{^ 3 4})))
(check-exn (regexp (regexp-quote "an operand cannot be any of above"))
           (lambda () (parse '(+ + 3))))

;helper function to check if symbol is a valid id
(define (valid-ID? [s : Symbol]) : Boolean
  (if (member s '(+ - * /)) #f #t)
  )

;parsing concrete JYSS syntax to function definitions for interpreter
(define (parse-fundef [s : Sexp]) : FundefC
     (match s
       [(list 'fn (list (? symbol? items) ...) c) (match (second s)
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
    ))

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
(check-equal? (top-interp '{{fn {f x} {+ x 2}} {fn {g x} {+ x 2}} {fn {main init} {f {g 11}}}}) 15)
(check-exn (regexp (regexp-quote "we have duplicate function definitions"))
           (lambda () (top-interp '{{fn {f x} {+ x 2}} {fn {f x} {+ x 2}} {fn {f init} {f {g 11}}}})))

