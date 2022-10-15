#lang typed/racket
(require typed/rackunit)

;JYSS4
;I got to implement all the features and test-cases 
;Arith language top-definitons
(define-type ExprC (U numC binop leq0? idC FuncallC))
(struct numC ([n : Real])#:transparent)
(struct binop ([operator : Sexp] [l : ExprC] [r : ExprC])#:transparent)
(struct leq0? ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct FuncallC ([fname : Symbol] [paramVals : (Listof ExprC)])#:transparent)
(struct FundefC ([fname : Symbol] [params : (Listof Symbol)] [body : ExprC])#:transparent)

;function that maps binop symbol to arithetic operation
(define (arithmeticOperator [s : Sexp] [l : Real] [r : Real]) : Real
  (match s
    ['+ (+ l r)]
    ['* (* l r)]
    ['- (- l r)]
    ['/ (cond
          [(not (zero? r)) (/ l r)]
          [else (error 'JYSS "division by 0")])]))

;substitute one takes in what the value for a symbol in a function expressions with a value
(define (subst-one [what : ExprC] [for : Symbol] [in : ExprC]) : ExprC
  (match in
    [(numC n) in]
    [(idC s) (cond
               [(symbol=? s for) what]
               [else in])]
    [(FuncallC f vals) (FuncallC f (map (lambda ([val : ExprC])
                                          (subst-one what for val)) vals))]
    [(binop op l r) (binop op (subst-one what for l) (subst-one what for r))]
    [(leq0? a b c) (leq0? (subst-one what for a) (subst-one what for b) (subst-one what for c))]))

;test-case
(check-equal? (subst-one (numC 3) 'x (binop '+ (idC 'x) (idC 'y))) (binop '+ (numC 3) (idC 'y)))
(check-equal? (subst-one (numC 3) 'x (FuncallC 'f (list (idC 'x)))) (FuncallC 'f (list (numC 3))))
(check-equal? (subst-one (numC 3) 'x (FuncallC 'f (list (idC 'y)))) (FuncallC 'f (list (idC 'y))))
(check-equal? (subst-one (numC 3) 'x (leq0? (idC 'x) (idC 'x) (idC 'x))) (leq0? (numC 3) (numC 3) (numC 3)))
(check-equal? (subst-one (numC 3) 'x (numC 6)) (numC 6))

;substitution method for Functions param values with multiple params placeholders
(define (subst-many [vals : (Listof ExprC)] [for : (Listof Symbol)] [in : ExprC]) : ExprC
  (if (or (empty? vals) (empty? for)) in (subst-many (rest vals) (rest for) (subst-one (first vals) (first for) in))
      ))

(check-equal? (subst-many (list (numC 3) (numC 5)) (list 'x 'y)
                          (binop '+ (idC 'x) (idC 'y))) (binop '+ (numC 3) (numC 5)))
(check-equal? (subst-many (list (numC 3) (numC 5) (numC 8)) (list 'x 'y 'z)
                          (leq0? (idC 'z) (idC 'x) (idC 'y))) (leq0? (numC 8) (numC 3) (numC 5)))
(check-equal? (subst-many (list (numC 3) (numC 5)) (list 'x 'y)
                          (FuncallC 'f (list (idC 'x) (idC 'y)))) (FuncallC 'f (list (numC 3) (numC 5))))

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
(check-equal? (get-fundef 'B (list (FundefC 'A (list 'x) (numC 5))
                                   (FundefC 'B (list 'y) (numC 6))))
              (FundefC 'B (list 'y) (numC 6)))
(check-exn (regexp (regexp-quote "reference to undefined function"))
           (lambda () (get-fundef 'C (list (FundefC 'A (list 'x) (numC 5))
                                           (FundefC 'B (list 'y) (numC 6))))))

;helper function for interpreting funcalls (eager property of interpreting args first)
(define (interpret-funcalls [fname : Symbol] [paramVals : (Listof ExprC)] [lstfunc : (Listof FundefC)]) : Real
  (define fn (get-fundef fname lstfunc))
  (define all-params (FundefC-params fn))
  (cond
    [(not (equal? (length all-params) (length paramVals))) (error 'JYSS "different length of args and values")]
    [else (interp (subst-many (map (lambda ([paramV : ExprC])
                                     (numC (interp paramV lstfunc))) paramVals) all-params
                                                                                (FundefC-body fn)) lstfunc)]))
    
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
    [(FuncallC (? symbol? f) (list args ...)) (interpret-funcalls f (my-params args) lstfuncd)]))

;test-case
(check-equal? (interp  (binop '+ (binop '* (numC 1) (binop '/ (numC 2) (numC 2)))
                              (binop '- (binop '* (numC 2) (numC 3)) (numC 3))) '()) 4)
(check-equal? (interp (leq0? (numC 4) (numC 4) (numC 800)) '()) 800)
(check-equal? (interp (leq0? (numC -1) (numC 7) (numC -1)) '()) 7)
(check-equal? (interp (FuncallC 'f (list (numC 3)))
                      (list (FundefC 'f (list 'x) (binop '+ (idC 'x) (numC 1))))) 4)
(check-exn (regexp (regexp-quote "division by 0"))
           (lambda () (interp (binop '/ (numC 5) (numC 0)) '())))
(check-exn (regexp (regexp-quote "undefined variable error"))
           (lambda () (interp (idC 'y) '())))

;Finds and interprets the function body of main 
(define (interp-fns [lstfuncd : (Listof FundefC)]) : Real
  (match (get-fundef 'main lstfuncd)
    [(FundefC 'main (list) exp) (interp exp lstfuncd)]))

;test-case
(check-equal? (interp-fns (list (FundefC 'main (list) (binop '+ (numC 4) (numC 4))))) 8)

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
                                               [(equal? (length b) 0) (FuncallC a (list))]
                                               [else (FuncallC a (map parse b))])]
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
(check-equal? (parse '{f x}) (FuncallC 'f (list (idC 'x))))
(check-equal? (parse '{f x y}) (FuncallC 'f (list (idC 'x) (idC 'y))))
(check-equal? (parse '{^ 3 4}) (FuncallC '^ (list (numC 3) (numC 4))))
(check-exn (regexp (regexp-quote "no matchhing clause for leq0?"))
           (lambda () (parse '(leq0? (x 4) 3 (+ 2 9) 3))))
(check-exn (regexp (regexp-quote "invalid binop syntax"))
           (lambda () (parse '{/ 3 4 5})))
(check-exn (regexp (regexp-quote "invalid function call syntax"))
           (lambda () (parse '{7 3 4 5})))
(check-exn (regexp (regexp-quote "cannot use a reserved symbol must use valid ID"))
           (lambda () (parse '(+ + 3))))

;parsing concrete JYSS syntax to function definitions for interpreter
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'fn (list func-name arg-names ...) exp) (match (second s)
                                                     [(list (? symbol? a) (? symbol? b) ...)
                                                      (if (and (valid-ID? a) (map valid-ID? (cast b (Listof Symbol))))
                                                          (FundefC a (cast b (Listof Symbol)) (parse (third s)))
                                                          (error 'JYSS "not valid ID"))])]
    [other (error 'JYSS "invalid function definition syntax ~e" s)]))

;test-case
(check-equal? (parse-fundef '{fn {addone x} {+ 4 1}}) (FundefC 'addone (list 'x) (binop '+ (numC 4) (numC 1))))
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
    [(cons f r) (cons (FundefC-fname f) (all-fnames r))]))

;test-case
(check-equal? (all-fnames (list (FundefC 'f (list 'x) (numC 4)) (FundefC 'z (list 'e) (numC 4)))) '(f z))

;parsing JYSS syntax to a list of function definitions 
(define (parse-prog [s : Sexp]) : (Listof FundefC)
  (match s
    ['() '()]
    [(cons f r) (cond
                  [(list? f) (cons (parse-fundef f) (parse-prog r))]
                  [else (error 'JYSS "no valid fundef found")])]))

;test-case
(check-equal? (parse-prog '{{fn {f x} {+ x 14}} {fn {main} {f 14}}})
              (list (FundefC 'f (list 'x) (binop '+ (idC 'x) (numC 14)))
                    (FundefC 'main (list) (FuncallC 'f (list (numC 14))))))

;special interp-fns test-cases
(check-equal? (interp-fns (parse-prog '{{fn {f x y} {+ x y}}{fn {main} {f 1 2}}})) 3)
(check-equal? (interp-fns (parse-prog '{{fn {f} 5} {fn {main} {+ {f} {f}}}})) 10)
(check-exn #px"different length of args and values" (λ () (interp-fns (parse-prog '{{fn {f x y} {+ x y}}
                                                                                    {fn {main} {f 1}}}))))
(check-exn (regexp (regexp-quote "no valid fundef found"))
           (lambda () (parse-prog '{something})))

;Combines parsing and interpretation be able to evaluate JYSS3
(define (top-interp [s : Sexp]) : Real
  (cond
    [(equal? #f (check-duplicates (all-fnames (parse-prog s)))) (interp-fns (parse-prog s))]
    [else (error 'JYSS "we have duplicate function definitions")]))

;test-case
(check-equal? (top-interp '{{fn {f x} {+ x 2}} {fn {g x} {+ x 2}} {fn {main} {f {g 11}}}}) 15)
(check-equal? (top-interp (quote ((fn (main) (leq0? (* 3 1) 3 (+ 2 9)))))) 11)

(check-exn (regexp (regexp-quote "we have duplicate function definitions"))
           (lambda () (top-interp '{{fn {f x} {+ x 2}} {fn {f x} {+ x 2}} {fn {main} {f {g 11}}}})))


