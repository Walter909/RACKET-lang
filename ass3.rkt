#lang typed/racket

(require typed/rackunit)

;TODO - JYSS3
;Will need to implement the remaining functions
;Will also need to check that multiple functions do not have the same name
;Will need to implement other functions but have a check that main is in out list of fundefinitions
;Will need to handle what happens with first 'main function call

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
     (error 'get-fundef "reference to undefined function")]
    [(cons? fds)
     (cond
       [(equal? n (FundefC-fname (first fds))) (first fds)]
       [else (get-fundef n (rest fds))])]))

;test-case
(check-equal? (get-fundef 'B (list (FundefC 'A 'x (numC 5)) (FundefC 'B 'y (numC 6)))) (FundefC 'B 'y (numC 6)))
(check-exn (regexp (regexp-quote "reference to undefined function"))
           (lambda () (get-fundef 'C (list (FundefC 'A 'x (numC 5)) (FundefC 'B 'y (numC 6))))))

;evaluator/interpreter
(define (interp [exp : ExprC] [lstfunc : (Listof FundefC)]) : Real
  (match exp
      [(numC n) n]
      [(idC s) (error 'JYSS "undefined variable error ~e" s)]
      [(leq0? a b c) (if (<= (interp a lstfunc) 0) (interp b lstfunc) (interp c lstfunc))]
      [(binop o l r) (arithmeticOperator o (interp l lstfunc) (interp r lstfunc))]
      [(FuncallC f paramValue)(interpret-funcalls f paramValue lstfunc)]
    ))

;helper function for interpreting funcalls
(define (interpret-funcalls [fname : Symbol] [paramV : ExprC] [lstfunc : (Listof FundefC)]) : Real
  (define fn (get-fundef fname lstfunc))
  (interp (subst paramV (FundefC-param fn)  (FundefC-body fn)) lstfunc))

;test-case
(check-equal? (interp  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                                                               (binop '- (binop '* (numC 2) (numC 3)) (numC 3))) '()) 4)
(check-equal? (interp (leq0? (numC 4) (numC 4) (numC 800)) '()) 800)
(check-equal? (interp (leq0? (numC -1) (numC 7) (numC -1)) '()) 7)
(check-equal? (interp (FuncallC 'f (numC 3)) (list (FundefC 'f 'x (binop '+ (idC 'x) (numC 1))))) 4)
(check-exn (regexp (regexp-quote "undefined variable error"))
           (lambda () (interp (idC 'y) '())))

;parse concrete JYSS syntax to expressions for interpreter
 (define (parse [s : Sexp]) : ExprC
    (cond
      [(symbol? s) (idC s)]
      [(real? s) (numC s)]
      [(list? s)
       (case (first s)
         [(+ * - /) (binop (first s) (parse (second s)) (parse (third s)))]
         [(leq0?) (leq0? (parse (second s)) (parse (third s)) (parse (fourth s)))]
         [else (match s
                 [(list (? symbol? a) b) (FuncallC a (parse b))]
                 [other (error 'JYSS "something went wrong")])
               ])]
      [else (error 'JYSS "invalid input")]))

;test-case
(check-equal? (parse '{+ {* 1 (/ 2 2)} {- (* 2 3) 3}})  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                                                               (binop '- (binop '* (numC 2) (numC 3)) (numC 3))))
(check-equal? (parse '{leq0? 4 5 5}) (leq0? (numC 4) (numC 5) (numC 5)))
(check-equal? (parse '{fn x}) (FuncallC 'fn (idC 'x)))

;parsing concrete JYSS syntax to function definitions for interpreter
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

;parsing JYSS syntax to a list of function definitions where at least one is main for interepreter
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons fn r) (cond
                 [(list? fn) (cons (parse-fundef fn) (parse-prog r))]
                 [else (error 'JYSS "not a list form JYSS")])]
    [other (error 'JYSS "no valid fundef found")]
    ))

;test-case
(check-equal? (parse-prog '{{fn {f x} {+ x 14}} {fn {main init} {+ x 14}}}) (list (FundefC 'f 'x (binop '+ (idC 'x) (numC 14)))
                                                                                  (FundefC 'main 'init (binop '+ (idC 'x) (numC 14)))))