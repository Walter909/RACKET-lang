#lang typed/racket

(require typed/rackunit)

;2.1 textbook problems
;Each customer pays $5 per ticket
;Every performance costs the theater $20, plus $.50 per attendee. Develop the function total-profit.
(define (total-profit [attendees : Real]) : Real
  (define sp (* 5 attendees))
  (define cp (* 0.5 attendees))
  (- sp (+ cp 20)))

;test-case
(check-equal? (total-profit 20) 70.0)

;Develop area-cylinder. The program consumes the radius of the cylinder's base disk
;and its height. Its result is the surface area of the cylinder.
(define (area-cylinder [r : Real] [h : Real]) : Real
  (+ (* r (* h (* 2 pi)))  (* 2 (* pi (* r r))))
)

;test-case
(check-= (area-cylinder 9 4) 735 1 "work")
(check-= (area-cylinder 4 9) 326 1 "work")

;2.2
;top definitions
(define-type magic-trick (U Card-Trick Guillotine))
(struct Card-Trick ([decks : Real] [volunteers : Real])#:transparent)
(struct Guillotine ([realism : Real] [has-tiger : Boolean])#:transparent)

;Assume that a card trick requires one minute per deck of cards and that
;its length is doubled for every volunteer required and a guillotine trick requires
;ten minutes unless it has a tiger, in which case it requires 20.
(define (trick-minutes [target : magic-trick]) :  Real
  (match target
    [(Card-Trick decks volunteers) (* decks (expt 2 volunteers))]
    [(Guillotine realism has-tiger) (if has-tiger 20 10)]))


;test-cases
(define c1 (Card-Trick 4 4))
(define c2 (Card-Trick 5 5))
(define g1 (Guillotine 5 #t))
(define g2 (Guillotine 5 #f))

(check-equal? (trick-minutes c1) 64)

(check-equal? (trick-minutes g1) 20)

(check-equal? (trick-minutes g2) 10)

;2.3
;top definition
(define-type Polynomial (U Linear Quadratic))
(struct Linear ([A : Real] [B : Real])#:transparent)
(struct Quadratic ([A : Real] [B : Real] [C : Real])#:transparent)

;define the function interp that accepts a polynomial and a value for the variable and produces the
;result of plugging in the value for the variable.
(define (interp [target : Polynomial] [x : Real]) : Real
    (cond 
    [(Linear? target) (match target
                        [(Linear A B) (+ (* A x) B)])]
    [(Quadratic? target) (match target
                           [(Quadratic A B C) (+ (+ (* A (expt x 2)) (* B x)) C)])]))

;test-case
(define l1 (Linear 10 10))
(define q1 (Quadratic 10 10 10))
(check-equal? (interp l1 13) 140)
(check-equal? (interp q1 13) 1830)

;2.4
;takes a polynomial and returns another polynomial that represents its derivative
(define (derivative [p : Polynomial]) : Polynomial 
  
  (cond
    [(Linear? p) (match p
                        [(Linear A B) (Linear 0 A)])]
    [(Quadratic? p) (match p
                           [(Quadratic A B C) (Linear (* 2 A) B)])]))

;test-case
(define l2 (Linear 5 5))
(define q2 (Quadratic 5 5 5))
(define q3 (Quadratic 0 6 3))
(check-equal? (derivative l2) (Linear 0 5))
(check-equal? (derivative q2) (Linear (* 2 (Quadratic-A q2)) (Quadratic-B q2)))

;2.5
;BTree data definition
(define-type BTree (U Leaf Node))
(struct Node ([val : Symbol] [left : BTree] [right : BTree])#:transparent)
(struct Leaf ()#:transparent)

;examples
(define lf1 (Leaf))
(define lf2 (Leaf))
(define n1 (Node '+ lf1 lf2))
(define n2 (Node '- lf1 lf2))
(define n3 (Node '~ n1 n2))

;2.6
;define a mirror function that takes a btree and reflects left to right
(define (mirror [b1 : BTree]) : BTree
  (match b1
    [(Leaf) (Leaf)]
    [(Node v l r) (Node v (mirror r) (mirror l))]))

;test-case
(define n4 (Node '~ n2 n1))
(check-equal? (mirror n3) n4)

;2.7
;develop the left-spine function that accepts a binary tree and returns a new
;one which has only the “left spine” of the given tree, and each right branch is simply a leaf
(define (left-spine [b1 : BTree]) : BTree
  (match b1
    [(Leaf) (Leaf)]
    [(Node v l r) (Node v (left-spine l) (Leaf))]))

;test-case
(define n5 (Node '@ n1 n2)) 
(define n6 (Node '! n3 n5))
(check-equal? (left-spine n6) (Node '! (Node '~ (Node '+ lf1 lf2) lf1) lf1))

;2.8
;the occurrences function accepts a binary tree and a symbol and returns a number
;indicating how many nodes in the tree have the given symbol as their value
(define (occurrences [b1 : BTree] [target : Symbol]) : Real
  (match b1
    [(Leaf) 0]
    [(Node v l r) (cond
                    [(equal? v target) (+ (+ (occurrences r target) (occurrences l target)) 1)]
                    [else (+ (occurrences r target) (occurrences l target))])]))

;test-case
(check-equal? (occurrences n6 '+) 2)
(check-equal? (occurrences n2 '-) 1)
(check-equal? (occurrences n4 '*) 0)

;2.9
;the subst function accepts a src BTree and replacement symbol and replacement BTree
;that replaces all the nodes with that symbol in the src BTree with the replacement BTree
(define (subst [src : BTree] [target : Symbol] [repl : BTree]) : BTree
  (match src
    [(Leaf) (Leaf)]
    [(Node v l r) (if (equal? target v) repl (Node v (subst l target repl) (subst r target repl)))]))

;test-case
(define n7 (Node '$ lf1 lf2))
(check-equal? (subst n6 '+ n7)
              (Node '! (Node '~ (Node '$ lf1 lf2) (Node '- lf1 lf2))
                    (Node '@ (Node '$ lf1 lf2) (Node '- lf1 lf2))))
(check-equal? (subst n1 '+ n7) n7)
