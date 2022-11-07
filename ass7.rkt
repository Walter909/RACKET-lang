#lang typed/racket
(require typed/rackunit)

;JYSS7
;expression definitons : parse output
(define-type TyExprC (U numC ifC idC strC AppC lamC))
(struct numC ([n : Real])#:transparent)
(struct idC ([name : Symbol])#:transparent)
(struct strC ([str : String])#:transparent)
(struct ifC ([test : TyExprC] [then : TyExprC] [else : TyExprC])#:transparent)
(struct AppC ([func : TyExprC] [paramVals : (Listof TyExprC)])#:transparent)
(struct lamC ([params : (Listof Symbol)] [paramsTy : (Listof Type)] [body : TyExprC] [retT : Type])#:transparent)

;value definitions : interp output
(define-type Value (U Real Boolean String cloV primopV errorV))
(struct cloV ([params : (Listof Symbol)] [body : TyExprC] [env : Env])#:transparent)
(struct primopV ([operation : Symbol])#:transparent)
(struct errorV ([operation : Symbol])#:transparent)

;top/default level environment definitions
(define-type Env (Listof Bind))
(struct Bind ([name : Symbol] [val : Value])#:transparent)
(define top-level : Env (list (Bind 'true #t) (Bind 'false #f)
                              (Bind '+ (primopV '+)) (Bind '- (primopV '-))
                              (Bind '* (primopV '*)) (Bind '/ (primopV '/))
                              (Bind '<= (primopV '<=))   (Bind 'equal? (primopV 'equal?))
                              (Bind 'error (errorV 'error))))

;type definitions : type-check output
(define-type Type (U numT strT ifT boolT funT))
(struct numT ()#:transparent)
(struct boolT ()#:transparent)
(struct strT ()#:transparent)
(struct ifT ([l : Type] [r : Type])#:transparent)
(struct funT ([args : (Listof Type)] [ret : Type])#:transparent)

;type enviromnent
(define-type TyEnv (Listof TyBind))
(struct TyBind ([name : Symbol] [ty : Type])#:transparent)
(define top-level-types : TyEnv (list (TyBind 'true (boolT)) (TyBind 'false (boolT))
                                      (TyBind '+ (funT (list (numT) (numT)) (numT)))))

;lookup for id's in our type environments table
(define (lookup-type [id : Symbol] [env : TyEnv]) : Type
  (match env
    ['() (error 'JYSS "empty or not in environments table ~e" id)]
    [(cons f r) (match f
                  [(TyBind a b)(cond
                               [(equal? a id) b]
                               [else (lookup-type id r)])])]
    ))

;helper function for parsing AppC's (funcalls) types
(define (parse-types-appC [callee : TyExprC] [vals : (Listof TyExprC)] [tenv : TyEnv]) : Type
  (define ft (type-checker callee tenv))
  (define valst (map (lambda ([val : TyExprC]) (type-checker val tenv)) vals))
  (cond
    [(not (funT? ft)) (error 'JYSS "not a function type ~e" ft)]
    [(not (equal? (funT-args ft) valst)) (error 'JYSS "arg type mismatch ~e ~e" (funT-args ft) valst)]
    [else (funT-ret ft)]))

;a helper function that consrtucts a list of tyep bindings from a list of args and types
(define (create-type-Bindings [l1 : (Listof Symbol)] [l2 : (Listof Type)]) : (Listof TyBind)
  (match l1
    ['() '()]
    [(cons f r) (cons (TyBind f (first l2)) (create-type-Bindings r (rest l2)))])
  )

;our type checker that parses our language types
(define (type-checker [expr : TyExprC] [tenv : TyEnv]) : Type
  (match expr
    [(numC n) (numT)]
    [(strC s) (strT)]
    [(idC i) (lookup-type i tenv)]
    [(ifC a b c) (cond
                   [(boolT? (type-checker a tenv)) (ifT (type-checker b tenv) (type-checker c tenv))]
                   [else (error 'JYSS "type mismatch for if conditional ~e" a)])]
    [(AppC f (list a ...)) (parse-types-appC f a tenv)]
    [(lamC args argsTy bdy retT) (if (equal? (type-checker bdy (append (create-type-Bindings args argsTy) tenv)) retT)
                                     (funT argsTy retT)
                                     (error 'JYSS "type mismatch for our function ~e" bdy))]
    ))

;test-case
(check-equal? (type-checker (numC 4) '()) (numT))
(check-equal? (type-checker (strC "house") '()) (strT))
(check-equal? (type-checker (idC 'false) top-level-types) (boolT))
(check-equal? (type-checker (ifC (idC 'true) (numC 3) (strC "car")) top-level-types) (ifT (numT) (strT)))

(check-equal? (type-checker (AppC (idC '+) (list (numC 3) (numC 4))) top-level-types) (numT))

(check-exn (regexp (regexp-quote "type mismatch for if conditional"))
           (lambda () (type-checker (ifC (numC 9) (numC 3) (strC "car")) top-level-types)))


;serialize function that takes any value and returns a string format of it
(define (serialize [x : Value]) : String
  (match x
    [(? real? r) (~v r)]
    [(? boolean? s) (cond
                      [(equal? s #t) "true"]
                      [else "false"])]
    [(? string? s) (~v s)]
    [(cloV a b c) "#<procedure>"]
    [(primopV s) "#<primop>"]
    ))

;function that maps primitive operations to a symbol
(define (primitiveOperator [s : Sexp] [l : Value] [r : Value]) : Value
  (match s
    ['+ (if (not (and (real? l) (real? r)))
            (error 'JYSS "args must both be real") (+ (cast l Real) (cast r Real)))]
    ['* (if (not (and (real? l) (real? r)))
            (error 'JYSS "args must both be real") (* (cast l Real) (cast r Real)))]
    ['- (if (not (and (real? l) (real? r)))
            (error 'JYSS "args must both be real") (- (cast l Real) (cast r Real)))]
    ['/ (cond
          [(not (and (real? l) (real? r))) (error 'JYSS "args must both be real")]
          [(not (zero? (cast r Real))) (/ (cast l Real) (cast r Real))]
          [else (error 'JYSS "division by 0")])]
    ['<= (if (not (and (real? l) (real? r)))
             (error 'JYSS "args must both be real") (<= (cast l Real) (cast r Real)))]
    ['equal? (cond
               [(and (real? l) (real? r)) (equal? l r)]
               [(and (boolean? l) (boolean? r)) (equal? l r)]
               [(and (string? l) (string? r)) (equal? l r)]
               [else #f]
               )]
    ))

;a helper function that constuct a list of bindings from a list of args and arg-values
(define (create-Bindings [l1 : (Listof Symbol)] [l2 : (Listof Value)]) : (Listof Bind)
  (match l1
    ['() '()]
    [(cons f r) (cons (Bind f (first l2)) (create-Bindings r (rest l2)))])
  )

;helper function for interpreting funcalls (eager property of interpreting args first)
(define (interpret-funcalls [callee : TyExprC] [env : Env] [paramVals : (Listof TyExprC)]) : Value
  (define clos-primop (interp callee env))
  
  (match clos-primop
    [(errorV op) (cond
                  [(and (equal? op 'error) (equal? 1 (length paramVals))) (error 'JYSS "user-error ~v" op)]
                  [else (error 'JYSS "error only takes 1 param")])]
    [(primopV op) (cond
                    [(not (equal? (length paramVals) 2))
                     (error 'JYSS "wrong number of args for a binary operation")]
                    [else (primitiveOperator op (interp (first paramVals) env)
                                             (interp (second paramVals) env))])]
    [(cloV params body closure-env) (cond
                                      [(not (equal? (length params) (length paramVals)))
                                       (error 'JYSS "different length of args and values")]
                                      [else
                                       (interp body
                                               (append (create-Bindings params
                                                       (map (lambda ([paramV : TyExprC])
                                                       (interp paramV env)) paramVals))
                                                       closure-env))])]
    [other (error 'JYSS "no binding found for the expression ~v" clos-primop)]))

;lookup function for id/var refs in enviromnents table
(define (lookup [id : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'JYSS "empty or not in environments table ~e" id)]
    [(cons f r) (match f
                  [(Bind a b)(cond
                               [(equal? a id) b]
                               [else (lookup id r)])])]
    ))

;helper function to create listof of TyExprC from args
(define (my-params [s : (Listof Any)]) : (Listof TyExprC)
  (cast s (Listof TyExprC)))

;the evaluator/interpreter which uses listof function definitions to evaluate expressions
(define (interp [exp : TyExprC] [env : Env]) : Value
  (match exp
    [(numC n) n]
    [(strC s) s]
    [(idC s) (lookup s env)]
    [(lamC a ARET b RET) (cloV a b env)]
    [(ifC a b c) (cond
                  [(boolean? (interp a env)) (if (interp a env) (interp b env) (interp c env))] 
                  [else (error 'JYSS "the cond must return a boolean: ~v" a)])]
    [(AppC f (list args ...)) (interpret-funcalls f env (my-params args))]))

;helper function to check if a symbol is a valid id
(define (valid-ID? [s : Symbol]) : Boolean
  (if (member s '(vars: body: if proc go)) #f #t))

;helper function to determine the type that an sexp should return and prevent some collisions
(define (expected-type [s : Sexp]) : Symbol
  (match s
    [(? string? st) 'str]
    [(? symbol? s) 'id]
    [(? real? r) 'num]
    [(? list? l)
     (cond
       [(equal? (first l) 'proc) 'proc]
       [(equal? (first l) 'vars:) 'vars:]
       [(equal? (first l) 'if) 'if]
       [else 'AppC])]))

;helper function to get the symbols from the vars
(define (get-symboles [s : (Listof Any)]) : (Listof Symbol)
  (match s
    ['() '()]
    [(cons f r) (match f
                  [(list (? symbol? a) '= b) (cond
                                               [(valid-ID? a) (cons a (get-symboles r))]
                                               [else (error 'JYSS "cannot use reserved word ~v" a)])]
    )]))

;helper function to get the paramVals from the vars
(define (get-paramVals [s : (Listof Any)]) : (Listof TyExprC)
  (match s
    ['() '()]
    [(cons f r) (match f
                  [(list (? symbol? a) '= b) (cons (parse (cast b Sexp)) (get-paramVals r))])]
    ))

;parse concrete JYSS syntax to expressions for interpreter to understand
(define (parse [s : Sexp]) : TyExprC
  (match (expected-type s)
    ['id (match s
           [(? symbol? a) (if (valid-ID? a) (idC a) (error 'JYSS "this is not a valid ID ~v" a))])]
    ['str (match s
            [(? string? str) (strC str)])]
    ['num (match s
            [(? real? r) (numC r)])]
    ['AppC (match s
                 [(list a b ...) (cond
                                   [(equal? (length b) 0) (AppC (parse a) (list))]
                                   [else (AppC (parse a) (map parse b))])]
                 )]
    ['if (match s
           [(list 'if a b c) (ifC (parse a) (parse b) (parse c))]
           [other (error 'JYSS "no matching clause for if")])]
    ['vars: (match s
              [(list 'vars: a ... 'body: b) (cond
                                             [(check-duplicates (get-symboles a)) (error 'JYSS "no duplicate params")]
                                             [else (AppC (lamC (get-symboles a) (parse b) (numT)) (get-paramVals a))])]
              [other (error 'JYSS "invalid vars: body: syntax: ~v" s)])]
    ['proc (match s
             [(list 'proc (list (? symbol? id) ...) 'go exp) (cond
                                                               [(check-duplicates (cast id (Listof Symbol)))
                                                                (error 'JYSS "no duplicate params")]
                                                               [else (lamC (cast id (Listof Symbol)) (parse exp) (numT))])]
             [other (error 'JYSS "invalid proc syntax: ~v" s)])]))

;Combines parsing and interpretation be able to evaluate JYSS3
(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-level)))

;test-case
(check-equal? (top-interp '{{proc {a b c} go {+ a {+ b c}}} 3 4 5}) "12")
(check-equal? (top-interp '{{proc {} go {if (<= 3 5) 11 5000}} }) "11")
(check-exn (regexp (regexp-quote "error only takes 1 param"))
           (lambda ()   (top-interp '(error "1234" "23244"))))
(check-exn (regexp (regexp-quote "user-error"))
           (lambda ()   (top-interp '(+ 4 (error "1234")))))
(check-exn (regexp (regexp-quote "different length of args and values"))
           (lambda ()  (top-interp '{{proc {a b c} go {+ a {+ b c}}} 3 4})))
(check-exn (regexp (regexp-quote "no binding found for the expression"))
           (lambda ()  (top-interp '{23 4 4})))

;test-case
(check-equal? (parse "Hello") (strC "Hello"))
(check-equal? (parse '{a})  (AppC (idC 'a) (list)))
(check-equal? (parse '{if 4 5 5}) (ifC (numC 4) (numC 5) (numC 5)))
(check-equal? (parse '{f x}) (AppC (idC 'f) (list (idC 'x))))
(check-equal? (parse '{f x y}) (AppC (idC 'f) (list (idC 'x) (idC 'y))))
(check-equal? (parse '{^ 3 4}) (AppC (idC '^) (list (numC 3) (numC 4))))
(check-equal? (parse '{proc {x y} go 3}) (lamC (list 'x 'y) (numC 3) (numT)))

(check-equal? (parse '{vars: [z = 3] [x = 3] body: {+ x z}})
              (AppC (lamC (list 'z 'x) (AppC (idC '+) (list (idC 'x) (idC 'z))) (numT))
                        (list (numC 3) (numC 3))))

(check-exn (regexp (regexp-quote "this is not a valid ID"))
           (lambda () (parse '(+ if 4))))
(check-exn (regexp (regexp-quote "no matching clause for if"))
           (lambda () (parse '(if (x 4) 3 (+ 2 9) 3))))
(check-exn (regexp (regexp-quote "no duplicate params"))
           (lambda () (parse '(vars: (z = (proc () go 3)) (z = 9) body: (z)))))
(check-exn (regexp (regexp-quote "no duplicate params"))
           (lambda () (parse '{proc {x x} go {+ x x}})))
(check-exn (regexp (regexp-quote "invalid proc syntax: "))
           (lambda () (parse '{proc {3 4 5} go 6})))
(check-exn (regexp (regexp-quote "invalid vars: body: syntax: "))
           (lambda () (parse '{vars: })))
(check-exn (regexp (regexp-quote "cannot use reserved word"))
           (lambda () (parse '{vars: [if = 3] body: {+ if 3}})))

;test-case
(check-equal? (get-paramVals '([s = 3] [e = 5] [r = 3])) (list (numC 3) (numC 5) (numC 3)))

;test-case
(check-equal? (get-symboles '([s = x] [e = t] [r = i])) (list 's 'e 'r))

;test-case
(check-equal? (expected-type '(if x y g)) 'if)
(check-equal? (expected-type '(f x y g)) 'AppC)
(check-equal? (expected-type 'u) 'id)
(check-equal? (expected-type '2) 'num)
(check-equal? (expected-type '(+ e 5 f)) 'AppC)

;test-case
(check-equal? (create-Bindings (list 'f 'r 'h) (list 3 5 6)) (list (Bind 'f 3) (Bind 'r 5) (Bind 'h 6)))

;test-case
(check-equal? (serialize 3) "3")
(check-equal? (serialize "3") "\"3\"")
(check-equal? (serialize #t) "true")
(check-equal? (serialize #f) "false")
(check-equal? (serialize (cloV (list 's) (numC 5) '())) "#<procedure>")
(check-equal? (serialize (primopV '+)) "#<primop>")


;test-case
(check-equal? (interp (ifC (AppC (idC '<=) (list (numC -1) (numC 7))) (idC 'true) (idC 'false)) top-level) #t)
(check-equal? (interp (ifC (AppC (idC '<=) (list (numC 10) (numC 7))) (idC 'true) (idC 'false)) top-level) #f)

(check-equal? (interp (AppC (idC '+) (list (numC 3) (numC 4))) top-level) 7)
(check-equal? (interp (AppC (idC '-) (list (numC 3) (numC 4))) top-level) -1)
(check-equal? (interp (AppC (idC '/) (list (numC 4) (numC 4))) top-level) 1)
(check-equal? (interp (AppC (idC '*) (list (numC 3) (numC 4))) top-level) 12)
(check-equal? (interp (AppC (idC 'equal?) (list (numC 4) (numC 4))) top-level) #t)
(check-equal? (interp (AppC (idC 'equal?) (list (idC 'true) (idC 'false))) top-level) #f)
(check-equal? (interp (AppC (idC 'equal?) (list (strC "howdy") (strC "hi"))) top-level) #f)
(check-equal? (interp (AppC (idC 'equal?) (list (numC 4) (idC 'true))) top-level) #f)

(check-exn (regexp (regexp-quote "wrong number of args for a binary operation"))
           (lambda () (interp (AppC (idC '+) (list (numC 3) (numC 3) (numC 4))) top-level)))
(check-exn (regexp (regexp-quote "the cond must return a boolean"))
           (lambda () (interp (ifC (numC 4) (numC 4) (numC 5)) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (AppC (idC '+) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (AppC (idC '-) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (AppC (idC '/) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (AppC (idC '*) (list (numC 3) (idC 'true))) top-level)))
(check-exn (regexp (regexp-quote "args must both be real"))
           (lambda () (interp (AppC (idC '<=) (list (numC 3) (idC 'false))) top-level)))
(check-exn (regexp (regexp-quote "division by 0"))
           (lambda () (interp (AppC (idC '/) (list (numC 3) (numC 0))) top-level)))
(check-equal? (interp (AppC (idC 'f) (list (numC 3)))
                      (list (Bind 'f (cloV (list 'x) (AppC (idC '+) (list (idC 'x) (numC 1))) top-level)))) 4)
(check-equal?
 (interp (lamC (list 'a 'b) (AppC (idC '+) (list (idC 'a) (idC 'b))) (numT)) top-level)
 (cloV (list 'a 'b) (AppC (idC '+) (list (idC 'a) (idC 'b))) top-level))
(check-equal? (interp (AppC (lamC (list 'a 'b) (AppC (idC '+) (list (idC 'a) (idC 'b))) (numT))
                                (list (numC 4) (numC 4))) top-level) 8)
;test-case
(check-equal? (interpret-funcalls (idC 'f)
                                  (list (Bind 'f (cloV (list 'a)
                                                       (AppC (idC '+) (list (idC 'a) (numC 1))) top-level)))
                                  (list (numC 3))) 4)
;test-case
(check-equal? (lookup 'a (list (Bind 'f 5) (Bind 'a 4))) 4)
(check-exn (regexp (regexp-quote "empty or not in environments table"))
           (lambda () (lookup 'd (list (Bind 'f 5) (Bind 'a 4)))))
