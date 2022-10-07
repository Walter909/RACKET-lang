#lang typed/racket

(require typed/rackunit)

;TODO
;Will need to genericize ArithC to ExprC
;Will need to parse with {} instead of ()
;Will need to handle what happens with 'main
;Will need to add function definition logic
;Will need to create mapping function for binary operations
;Will need to create other functions e.g top-interp

;Arith language top-definitons
(define-type ArithC (U numC binop leq0?))
(struct numC ([n : Real])#:transparent)
;(struct plusC ([l : ArithC] [r : ArithC])#:transparent)
;(struct multC ([l : ArithC] [r : ArithC])#:transparent)
(struct binop ([operator : Sexp] [l : ArithC] [r : ArithC])#:transparent)
(struct leq0? ([test : ArithC] [then : ArithC] [else : ArithC])#:transparent)


;function that maps binop symbol to arithetic operation
(define (arithmetic_operators [s : Sexp] [l : Real] [r : Real]) : Real
  (match s
    ['+ (+ l r)]
    ['* (* l r)])
  )

;evaluator/interpreter
(define (interp [target : ArithC]) : Real
  (match target
      [(numC n) n]
      ;[(plusC l r) (+ (interp l) (interp r))]
      ;[(multC  l r) (* (interp l) (interp r))]
      [(leq0? a b c) (if (<= (interp a) 0) (interp b) (interp c))]
      [(binop o l r) (arithmetic_operators o (interp l) (interp r))]
      [else (error 'JYSS "unimplemented")]
    ))

;test-case
;(check-equal? (interp (plusC (numC 5) (numC 5))) 10)
;(check-equal? (interp (multC (numC 5) (numC 5))) 25)
(check-equal? (interp (binop '+ (binop '* (numC 1) (numC 2)) (binop '+ (numC 2) (numC 3)))) 7)
(check-equal? (interp (leq0? (numC 4) (numC 4) (numC 800))) 800)
(check-equal? (interp (leq0? (numC -1) (numC 7) (numC -1))) 7)

;parser
 (define (parse [s : Sexp]) : ArithC
    (cond
      [(real? s) (numC s)]
      [(list? s)
       (case (first s)
         [(+ *) (binop (first s) (parse (second s)) (parse (third s)))]
         [(leq0?) (leq0? (parse (second s)) (parse (third s)) (parse (fourth s)))]
         [else (error 'JYSS "invalid list input")])]
      [else (error 'JYSS "invalid input")]))

;test-case
(check-equal? (parse '{+ {* 1 2} {+ 2 3}}) (binop '+ (binop '* (numC 1) (numC 2)) (binop '+ (numC 2) (numC 3))))
(check-equal? (parse '{leq0? 4 5 5}) (leq0? (numC 4) (numC 5) (numC 5)))




