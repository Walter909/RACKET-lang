#lang typed/racket

(require typed/rackunit)

;TODO - JYSS3
;Add / and - to binops able to be interpreted
;Will need to support using idC
;Will need to check length of funcalls lists
;Will need to parse with {} instead of ()
;Will need to implement other functions 
;Will need to add function substitution logic with list of fundefs
;Will need to handle what happens with first 'main function call

;Arith language top-definitons
(define-type ExprC (U numC binop leq0? idC FuncallC))
(struct numC ([n : Real])#:transparent)
(struct binop ([operator : Sexp] [l : ExprC] [r : ExprC])#:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct FuncallC ([name : Symbol] [argVal : ExprC])#:transparent)
(struct FundefC ([name : Symbol] [param : Symbol] [body : ExprC])#:transparent)


;function that maps binop symbol to arithetic operation
(define (arithmeticOperator [s : Sexp] [l : Real] [r : Real]) : Real
  (match s
    ['+ (+ l r)]
    ['* (* l r)])
  )

;evaluator/interpreter
(define (interp [target : ExprC]) : Real
  (match target
      [(numC n) n]
      [(leq0? a b c) (if (<= (interp a) 0) (interp b) (interp c))]
      [(binop o l r) (arithmeticOperator o (interp l) (interp r))]
      [else (error 'JYSS "unimplemented")]
    ))

;test-case
(check-equal? (interp (binop '+ (binop '* (numC 1) (numC 2)) (binop '+ (numC 2) (numC 3)))) 7)
(check-equal? (interp (leq0? (numC 4) (numC 4) (numC 800))) 800)
(check-equal? (interp (leq0? (numC -1) (numC 7) (numC -1))) 7)

;parse hard syntax JYSS to expressions
 (define (parse [s : Sexp]) : ExprC
    (cond
      [(symbol? s) (idC s)]
      [(real? s) (numC s)]
      [(list? s)
       (case (first s)
         [(+ *) (binop (first s) (parse (second s)) (parse (third s)))]
         [(leq0?) (leq0? (parse (second s)) (parse (third s)) (parse (fourth s)))]
         [else (match s
                 [(list (? symbol? a) b) (FuncallC a (parse b))]
                 [other (error 'JYSS "something went wrong")])
               ])]
      [else (error 'JYSS "invalid input")]))

;test-case
(check-equal? (parse '{+ {* 1 2} {+ 2 3}}) (binop '+ (binop '* (numC 1) (numC 2)) (binop '+ (numC 2) (numC 3))))
(check-equal? (parse '{leq0? 4 5 5}) (leq0? (numC 4) (numC 5) (numC 5)))
(check-equal? (parse '{fn x}) (FuncallC 'fn (idC 'x)))

;parsing hard syntax to JYSS fundefinitions
(define (parse-fundef [s : Sexp]) : FundefC
  (cond
    [(list? s)
     (case (first s)
       [(fn) (match (second s)
                 [(list (? symbol? a) (? symbol? b)) (FundefC a b (parse (third s)))])]
                 [() (error 'JYSS "where are the param and body?")]
       [else (error 'JYSS "where is fn?")])]
    [else (error 'JYSS "invalid fundef syntax")])
  )

;test-case
(check-equal? (parse-fundef '(fn (addone x) (+ 4 1))) (FundefC 'addone 'x (binop '+ (numC 4) (numC 1))))


