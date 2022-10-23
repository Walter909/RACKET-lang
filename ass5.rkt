#lang typed/racket
(require typed/rackunit)

;JYSS5
;language top-definitons : parse output
(define-type ExprC (U numC ifC idC strC FuncallC lamC))
(struct numC ([n : Real])#:transparent)
(struct ifC ([test : ExprC] [then : ExprC] [else : ExprC])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct strC ([str : String])#:transparent)
(struct FuncallC ([func : ExprC] [paramVals : (Listof ExprC)])#:transparent)
(struct lamC ([params : (Listof Symbol)] [body : ExprC])#:transparent)

;value types : interp output
(define-type Value (U Real Boolean String cloV primopV))
(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [val : Value])#:transparent)
(struct cloV ([params : (Listof Symbol)] [body : ExprC] [env : Env])#:transparent)
(struct primopV ([operation : Symbol])#:transparent)


;top/default environment definitions
(define top-level : Env (list (Bind 'true #t) (Bind 'false #f)
                              (Bind '+ (primopV '+)) (Bind '- (primopV '-)) (Bind '* (primopV '*)) (Bind '/ (primopV '/))
                              (Bind '<= (primopV '<=))   (Bind 'equal? (primopV 'equal?))))

;serialize function that takes any value and returns a string format of it
(define (serialize [x : Value]) : String
  (match x
    [(? real? r) (~v r)]
    [(? boolean? s) (cond
                      [(equal? s #t) "true"]
                      [else "false"])]
    [(cloV a b c) "#<procedure>"]
    [(primopV s) "#<primop>"]
    ))

;test-case
(check-equal? (serialize 3) "3")
(check-equal? (serialize #t) "true")
(check-equal? (serialize #f) "false")
(check-equal? (serialize (cloV (list 's) (numC 5) '())) "#<procedure>")
(check-equal? (serialize (primopV '+)) "#<primop>")

;function that maps primitive binop symbol to its operation
(define (arithmeticOperator [s : Sexp] [l : Value] [r : Value]) : Value
  (match s
    ['+ (if (not (and (real? l) (real? r))) (error 'JYSS "args must both be real") (+ (cast l Real) (cast r Real)))]
    ['* (if (not (and (real? l) (real? r))) (error 'JYSS "args must both be real") (* (cast l Real) (cast r Real)))]
    ['- (if (not (and (real? l) (real? r))) (error 'JYSS "args must both be real") (- (cast l Real) (cast r Real)))]
    ['/ (cond
          [(not (and (real? l) (real? r))) (error 'JYSS "args must both be real")]
          [(not (zero? (cast r Real))) (/ (cast l Real) (cast r Real))]
          [else (error 'JYSS "division by 0")])]
    ['<= (if (not (and (real? l) (real? r))) (error 'JYSS "args must both be real") (<= (cast l Real) (cast r Real)))]
    ['equal? (cond
               [(and (real? l) (real? r)) (equal? l r)]
               [(and (boolean? l) (boolean? r)) (equal? l r)]
               [(and (string? l) (string? r)) (equal? l r)]
               [else #f]
               )]))

;a helper function that consrtucts a a list of bindings from a list of args and arg-values
(define (create-Bindings [l1 : (Listof Symbol)] [l2 : (Listof Value)]) : (Listof Bind)
  (match l1
    ['() '()]
    [(cons f r) (cons (Bind f (first l2)) (create-Bindings r (rest l2)))])
  )

;test-case
(check-equal? (create-Bindings (list 'f 'r 'h) (list 3 5 6)) (list (Bind 'f 3) (Bind 'r 5) (Bind 'h 6)))


;helper function for interpreting funcalls (eager property of interpreting args first)
(define (interpret-funcalls [callee : ExprC] [env : Env] [paramVals : (Listof ExprC)]) : Value
  (define clos-primop (interp callee env))
  
  (match clos-primop
    [(primopV op) (cond
                    [(not (equal? (length paramVals) 2)) (error 'JYSS "wrong number of args for a binary operation")]
                    [else (arithmeticOperator op (interp (first paramVals) env) (interp (second paramVals) env))])]
    [(cloV params body closure-env) (cond
                                      [(not (equal? (length params) (length paramVals))) (error 'JYSS "different length of args and values")]
                                      [else
                                       (interp body (append (create-Bindings params (map (lambda ([paramV : ExprC])
                                                                                           (interp paramV env)) paramVals)) closure-env))])]))
;lookup function for id/var refs in enviromnents table
(define (lookup [id : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'JYSS "empty or not in environments table ~e" id)]
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
    [(strC s) s]
    [(idC s) (lookup s env)]
    [(lamC a b) (cloV a b env)]
    [(ifC a b c) (if (interp a env) (interp b env) (interp c env))]
    [(FuncallC f (list args ...)) (interpret-funcalls f env (my-params args))]))

;test-case
(check-equal? (interp (ifC (FuncallC (idC '<=) (list (numC -1) (numC 7))) (idC 'true) (idC 'false)) top-level) #t)
(check-equal? (interp (ifC (FuncallC (idC '<=) (list (numC 10) (numC 7))) (idC 'true) (idC 'false)) top-level) #f)

(check-equal? (interp (FuncallC (idC '+) (list (numC 3) (numC 4))) top-level) 7)
(check-equal? (interp (FuncallC (idC '-) (list (numC 3) (numC 4))) top-level) -1)
(check-equal? (interp (FuncallC (idC '/) (list (numC 4) (numC 4))) top-level) 1)
(check-equal? (interp (FuncallC (idC '*) (list (numC 3) (numC 4))) top-level) 12)

(check-equal? (interp (FuncallC (idC 'equal?) (list (numC 4) (numC 4))) top-level) #t)
(check-equal? (interp (FuncallC (idC 'equal?) (list (idC 'true) (idC 'false))) top-level) #f)
(check-equal? (interp (FuncallC (idC 'equal?) (list (strC "howdy") (strC "hi"))) top-level) #f)
(check-equal? (interp (FuncallC (idC 'equal?) (list (numC 4) (idC 'true))) top-level) #f)

(check-exn (regexp (regexp-quote "wrong number of args for a binary operation"))
           (lambda () (interp (FuncallC (idC '+) (list (numC 3) (numC 3) (numC 4))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (FuncallC (idC '+) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (FuncallC (idC '-) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (FuncallC (idC '/) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (FuncallC (idC '*) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (FuncallC (idC '<=) (list (numC 3) (idC 'false))) top-level)))
(check-exn (regexp (regexp-quote "division by 0"))
           (lambda () (interp (FuncallC (idC '/) (list (numC 3) (numC 0))) top-level)))
(check-equal? (interp (FuncallC (idC 'f) (list (numC 3))) (list (Bind 'f (cloV (list 'x) (FuncallC (idC '+) (list (idC 'x) (numC 1))) top-level)))) 4)
(check-equal?
 (interp (lamC (list 'a 'b) (FuncallC (idC '+) (list (idC 'a) (idC 'b)))) top-level)
 (cloV (list 'a 'b) (FuncallC (idC '+) (list (idC 'a) (idC 'b))) top-level))
(check-equal? (interp (FuncallC (lamC (list 'a 'b) (FuncallC (idC '+) (list (idC 'a) (idC 'b)))) (list (numC 4) (numC 4))) top-level) 8)

;test-case
(check-equal? (interpret-funcalls (idC 'f) (list (Bind 'f (cloV (list 'a) (FuncallC (idC '+) (list (idC 'a) (numC 1))) top-level))) (list (numC 3))) 4)

;helper function to determine the type that an sexp should return and prevent some collisions
(define (expected-type [s : Sexp]) : Symbol
  (match s
    [(? string? st) 'str]
    [(? symbol? s) 'id]
    [(? real? r) 'num]
    [(? list? l)
     (cond
       [(equal? (first l) 'proc) 'proc]
       [(equal? (first l) 'if) 'if]
       [else 'funcallC])]))

;test-case
(check-equal? (expected-type '(if x y g)) 'if)
(check-equal? (expected-type '(f x y g)) 'funcallC)
(check-equal? (expected-type 'u) 'id)
(check-equal? (expected-type '2) 'num)
(check-equal? (expected-type '(+ e 5 f)) 'funcallC)

;parse concrete JYSS syntax to expressions for interpreter to understand
(define (parse [s : Sexp]) : ExprC
  (match (expected-type s)
    ['id (match s
           [(? symbol? a) (idC a)])]
    ['str (match s
            [(? string? str) (strC str)])]
    ['num (match s
            [(? real? r) (numC r)])]
    ['funcallC (match s
                 [(list a b ...) (cond
                                   [(equal? (length b) 0) (FuncallC (parse a) (list))]
                                   [else (FuncallC (parse a) (map parse b))])]
                 )]
    ['if (match s
           [(list 'if a b c) (ifC (parse a) (parse b) (parse c))]
           [other (error 'JYSS "no matching clause for if")])]
    ['proc (match s
             [(list 'proc (list (? symbol? id) ...) 'go exp) (lamC (cast id (Listof Symbol)) (parse exp))])]))

;test-case
(check-equal? (parse "Hello") (strC "Hello"))
(check-equal? (parse '{a})  (FuncallC (idC 'a) (list)))
(check-equal? (parse '{if 4 5 5}) (ifC (numC 4) (numC 5) (numC 5)))
(check-equal? (parse '{f x}) (FuncallC (idC 'f) (list (idC 'x))))
(check-equal? (parse '{f x y}) (FuncallC (idC 'f) (list (idC 'x) (idC 'y))))
(check-equal? (parse '{^ 3 4}) (FuncallC (idC '^) (list (numC 3) (numC 4))))
(check-equal? (parse '{proc {x y} go 3}) (lamC (list 'x 'y) (numC 3)))
(check-exn (regexp (regexp-quote "no matching clause for if"))
           (lambda () (parse '(if (x 4) 3 (+ 2 9) 3))))

;Combines parsing and interpretation be able to evaluate JYSS3
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-level)))

;test-case
(check-equal? (top-interp '{{proc {a b c} go {+ a {+ b c}}} 3 4 5}) "12")
(check-equal? (top-interp '{{proc {} go {if (<= 3 5) 11 5000}} }) "11")
(check-exn (regexp (regexp-quote "different length of args and values"))
           (lambda ()  (top-interp '{{proc {a b c} go {+ a {+ b c}}} 3 4})))
