#lang typed/racket
(require typed/rackunit)

;JYSS5
;Will need to make all the interp test-cases use the top/default level of environments

;language top-definitons
(define-type ExprC (U numC binop leq0? idC FuncallC))
(struct numC ([n : Real])#:transparent)
(struct binop ([operator : Sexp] [l : ExprC] [r : ExprC])#:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct FuncallC ([func : ExprC] [paramVals : (Listof ExprC)])#:transparent)

;value definitions for primitive types
(define-type Value (U Real Boolean cloV))
;bindings definitions for environments
(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [val : Value])#:transparent)
(struct cloV ([params : (Listof Symbol)] [body : ExprC] [env : Env])#:transparent)
(struct lamC ([params : (Listof Symbol)] [body : ExprC])#:transparent)

;top/default environment definitions
(define top-level : Env (list (Bind 'true #t) (Bind 'false #f)))

;serialize function that takes any value and returns a string format of it
(define (serialize [x : Value]) : String
  (match x
      [(? real? r) (~v r)]
      [(? boolean? s) (cond
                        [(equal? s #t) "true"]
                        [else "false"])]
      [(cloV a b c) "#<procedure>"]
  ))

;test-case
(check-equal? (serialize 3) "3")
(check-equal? (serialize #t) "true")
(check-equal? (serialize #f) "false")
(check-equal? (serialize (cloV (list 's) (numC 5) '())) "#<procedure>")

;function that maps binop symbol to arithetic operation
(define (arithmeticOperator [s : Sexp] [l : Real] [r : Real]) : Real
  (match s
    ['+ (+ l r)]
    ['* (* l r)]
    ['- (- l r)]
    ['/ (cond
          [(not (zero? r)) (/ l r)]
          [else (error 'JYSS "division by 0")])]))


;a helper function that consrtucts a a list of bindings from a list of args and arg-values
(define (create-Bindings [l1 : (Listof Symbol)] [l2 : (Listof Value)]) : (Listof Bind)
  (match l1
    ['() '()]
    [(cons f r) (cons (Bind f (first l2)) (create-Bindings r (rest l2)))])
  )

;test-case
(check-equal? (create-Bindings (list 'f 'r 'h) (list 3 5 6)) (list (Bind 'f 3) (Bind 'r 5) (Bind 'h 6)))

;helper function for interpreting funcalls (eager property of interpreting args first)
(define (interpret-funcalls [func : ExprC] [env : Env] [paramVals : (Listof ExprC)]) : Value
  
  (match (interp func env)
    [(cloV a b c) (cond
                    [(not (equal? (length a) (length paramVals))) (error 'JYSS "different length of args and values")]
                    [else
                     (interp b (append (create-Bindings a (map (lambda ([paramV : ExprC])
                                                               (interp paramV env)) paramVals)) '()))])]))

;lookup function for id/var refs in enviromnents table
(define (lookup [id : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'JYSS "empty or not in environments table")]
    [(cons f r) (match f
                  [(Bind a b)(cond
                               [(equal? a id) b]
                               [else (lookup id r)])])]
    ))

;test-case
(check-equal? (lookup 'a (list (Bind 'f 5) (Bind 'a 4))) 4)
(check-exn (regexp (regexp-quote "empty or not in environments table"))
           (lambda () (lookup 'd (list (Bind 'f 5) (Bind 'a 4)))))

;helper function to create listof of exprC from args
(define (my-params [s : (Listof Any)]) : (Listof ExprC)
  (cast s (Listof ExprC)))


;the evaluator/interpreter which uses listof function definitions to evaluate expressions
(define (interp [exp : ExprC] [env : Env]) : Value
  (match exp
    [(numC n) n]
    [(idC s) (lookup s env)]
    [(lamC a b) (map (lambda (a) (cloV a b env)) env)]
    [(leq0? a b c) (if (<= (cast (interp a env) Real) 0) (interp b env) (interp c env))]
    [(binop o l r) (arithmeticOperator o (cast (interp l env) Real) (cast (interp r env) Real))]
    [(FuncallC f (list args ...)) (interpret-funcalls f env (my-params args))]))

;test-case
;(check-equal? (interp (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
;                              (binop '- (binop '* (numC 2) (numC 3)) (numC 3))) '() '()) 4)
;(check-equal? (interp (leq0? (numC 4) (numC 4) (numC 800)) '() '()) 800)
;(check-equal? (interp (leq0? (numC -1) (numC 7) (numC -1)) '() '()) 7)
(check-equal? (interp (FuncallC (idC 'f) (list (numC 3))) (list (Bind 'f (cloV (list 'x) (binop '+ (idC 'x) (numC 1)) top-level)))) 4)
;(check-exn (regexp (regexp-quote "division by 0"))
;           (lambda () (interp (binop '/ (numC 5) (numC 0)) '()'())))
;(check-exn (regexp (regexp-quote "empty or not in environments table"))
;           (lambda () (interp (idC 'y) '() '())))

;helper function to check if symbol is a valid id
(define (valid-ID? [s : Symbol]) : Boolean
  (if (member s '(+ - * / leq0? fn)) #f #t))

;helper function to determine the type that an sexp should return and prevent some collisions
(define (expected-type [s : Sexp]) : Symbol
  (match s
    [(? symbol? s) 'id]
    [(? real? r) 'num]
    [(? list? l)
     (cond
       [(equal? (first l) 'leq0?) 'leq0?]
       [(member (first l) '(+ - * /)) 'binop]
       [else 'funcallC])]))

;test-case
(check-equal? (expected-type '(leq0? x y g)) 'leq0?)
(check-equal? (expected-type '(f x y g)) 'funcallC)
(check-equal? (expected-type 'u) 'id)
(check-equal? (expected-type '2) 'num)
(check-equal? (expected-type '(+ e 5 f)) 'binop)

;parse concrete JYSS syntax to expressions for interpreter to understand
(define (parse [s : Sexp]) : ExprC
  (match (expected-type s)
    ['id (match s
           [(? symbol? a) (if (valid-ID? a) (idC a)
                              (error 'JYSS "cannot use a reserved symbol must use valid ID ~e" s))])]
    ['num (match s
            [(? real? r) (numC r)])]
    ['funcallC (match s
                 [(list (? symbol? a) b ...) (cond
                                               [(equal? (length b) 0) (FuncallC (idC a) (list))]
                                               [else (FuncallC (idC a) (map parse b))])]
                 [other (error 'JYSS "invalid function call syntax ~e" s)])]
    ['binop (match s
              [(list op l r) (binop op (parse l) (parse r))]
              [other (error 'JYSS "invalid binop syntax ~e" s)])]
    ['leq0? (match s
              [(list 'leq0? a b c) (leq0? (parse a) (parse b) (parse c))]
              [other (error 'JYSS "no matchhing clause for leq0?")])]))

;test-case
(check-equal? (parse '{+ {* 1 {/ 2 2}} {- (* 2 3) 3}})  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                                                               (binop '- (binop '* (numC 2) (numC 3)) (numC 3))))
(check-equal? (parse '{leq0? 4 5 5}) (leq0? (numC 4) (numC 5) (numC 5)))
(check-equal? (parse '{f x}) (FuncallC (idC 'f) (list (idC 'x))))
(check-equal? (parse '{f x y}) (FuncallC (idC 'f) (list (idC 'x) (idC 'y))))
(check-equal? (parse '{^ 3 4}) (FuncallC (idC '^) (list (numC 3) (numC 4))))
(check-exn (regexp (regexp-quote "no matchhing clause for leq0?"))
           (lambda () (parse '(leq0? (x 4) 3 (+ 2 9) 3))))
(check-exn (regexp (regexp-quote "invalid binop syntax"))
           (lambda () (parse '{/ 3 4 5})))
(check-exn (regexp (regexp-quote "invalid function call syntax"))
           (lambda () (parse '{7 3 4 5})))
(check-exn (regexp (regexp-quote "cannot use a reserved symbol must use valid ID"))
           (lambda () (parse '(+ + 3))))

;Combines parsing and interpretation be able to evaluate JYSS3
(define (top-interp [s : Sexp]) : Value
  3
  )

;test-case
(check-equal? (serialize (top-interp '{{fn {f x} {+ x 2}} {fn {g x} {+ x 2}} {fn {main} {f {g 11}}}})) "15")
(check-equal? (serialize (top-interp (quote ((fn (main) (leq0? (* 3 1) 3 (+ 2 9))))))) "11")

(check-exn (regexp (regexp-quote "we have duplicate function definitions"))
           (lambda () (top-interp '{{fn {f x} {+ x 2}} {fn {f x} {+ x 2}} {fn {main} {f {g 11}}}})))