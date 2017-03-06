#lang racket
(require plai/datatype)
(require "listof.rkt")
(require "token.rkt")

(provide (except-out (all-defined-out) fn-for-maybe))


;;
;; parse-tree.rkt - Expression Language LL(1) Grammar Parse Trees
;;


;;
;; Data Types for Extended BNF
;;

;; RG: Any way to make this polymorphically checked? Prolly need macrology
;; (Maybe X)

;; Polymorphic maybe type
(define-type maybe
  [None]
  [Just (x (λ (x) true))])
  
(define (Just-of p? x)
  (if (p? x)
      (Just x)
      (error 'Just "predicate failure")))

(define (maybe-of? p?)
  (λ (m)
    (and (maybe? m)
         (type-case maybe m
           [None () true]
           [Just (x) (p? x)]))))

(define (fn-for-maybe m)
  (type-case maybe m
    [None () #f]
    [Just (x) (#f x)]))

(define (fold-maybe c b m)
  (type-case maybe m
    [None () b]
    [Just (x) (c x)]))

;; Program ::= (PRINT) Expression
;;           | Statement Program
(define-type pgm
  [print-pgm (e expr?)]
  [stmt-cons (s stmt?) (p pgm?)])


;; Statement ::= Assign
;; Assign ::= (IDENTIFIER i) (EQUAL) Expression (SEMICOLON)
;; skipping separate assign datatype
(define-type stmt
  [asgn-stmt (i IDENTIFIER?) (e expr?)])


;; Expression ::= CompExpression ExprSuffix
;; ExprSuffix ::= ε
;;              | (QUESTION) Expression (COLON) Expression  
;(define-type _expr
;  [expr (c comp-expr?) (e (maybeee? expr-sfx?))])
(define-type _expr
  [expr (c comp-expr?) (e (maybe-of? expr-sfx?))])

;; RG: Consider dropping maybe, use a direct datatype?
(define-type _expr-sfx
  [expr-sfx (conseq expr?) (altern expr?)])


;; CompExpression ::= AddExpression CompSuffix
;; CompSuffix ::= ε
;;              | (SMALLER) AddExpression CompSuffix
(define-type _comp-expr
  [comp-expr (a add-expr?) (cs (listof? add-expr?))])


;; AddExpression ::= MultExpression AddSuffix
;; AddSuffix ::= ε
;;            | AddOp MultExpression AddSuffix
;; AddOP ::= (PLUS)
;;         | (MINUS)
;; RG: Note the cheating below
(define-type _add-expr
  [add-expr (m mult-expr?) (as (listof add-sfx?))])

(define-type _add-sfx
  [add-sfx (o add-op?) (m mult-expr?)])

(define (add-op? x) (or (PLUS? x) (MINUS? x)))


;; MultExpression ::= NotExpression MultSuffix
;; MultSuffix ::= ε
;;           | (MULT) NotExpression MultSuffix
(define-type _mult-expr
  [mult-expr (n not-expr?) (ms (listof not-expr?))])


;; NotExpression ::= (NOT) NotExpression
;;                 | PrimaryExpression
(define-type not-expr
  [not-not (n not-expr?)]
  [not-prim (p prim-expr?)])


;; PrimaryExpression ::= (INTEGER n)
;;                     | (IDENTIFIER i)
;;                     | (LPAREN) Expression (RPAREN)
;;
(define-type prim-expr
  [int-prim (i INTEGER?)]
  [id-prim (i IDENTIFIER?)]
  [expr-prim (e expr?)])


(define (fn-for-program p0)
  (let ([... (void)])
    (local
      [(define (fn-for-pgm p)
         (type-case pgm p
           [print-pgm (e) (... (fn-for-expr e))]
           [stmt-cons (s p) (... (fn-for-stmt s) (fn-for-pgm p))]))
       (define (fn-for-stmt s)
         (type-case stmt s
           [asgn-stmt (i e) (... (fn-for-identifier i) (fn-for-expr e))]))
       (define (fn-for-expr e)
         (type-case _expr e
           [expr (c e)
                 (... (fn-for-comp-expr c) (fn-for-maybe-expr-suffix e))]))
       (define (fn-for-maybe-expr-suffix m)
         (type-case maybe m
           [None () (...)]
           [Just (es) (... (fn-for-expr-sfx es))]))
       (define (fn-for-expr-sfx es)
         (type-case _expr-sfx es
           [expr-sfx (c a) (... (fn-for-expr c) (fn-for-expr a))]))
       (define (fn-for-comp-expr ce)
         (type-case _comp-expr ce
           [comp-expr (a cs)
                      (... (fn-for-add-expr a) (fn-for-comp-sfx cs))]))
       (define (fn-for-comp-sfx cs)
         (cond [(null? cs) (...)]
               [else (...  (fn-for-add-expr (first cs))
                           (fn-for-comp-sfx (rest cs)))]))
       (define (fn-for-add-expr ae)
         (type-case _add-expr ae
           [add-expr (m as)
                     (... (fn-for-mult-expr m) (fn-for-add-sfx as))]))
       (define (fn-for-add-sfx as)
         (cond [(null? as) (...)]
               [else 
                (type-case _add-sfx (first as)
                  [add-sfx (o m)
                           (... (fn-for-add-op o)
                                (fn-for-mult-expr m)
                                (fn-for-add-sfx (rest as)))])]))
       (define (fn-for-add-op o)
         (cond
           [(PLUS? o) (...)]
           [(MINUS? o) (...)]))         
       (define (fn-for-mult-expr me)
         (type-case _mult-expr me
           [mult-expr (n ms)
                      (... (fn-for-not-expr n) (fn-for-mult-sfx ms))]))
       (define (fn-for-mult-sfx ms)
         (cond [(null? ms) (...)]
               [else
                (... (fn-for-not-expr (first ms))
                     (fn-for-mult-sfx (rest ms)))]))
       (define (fn-for-not-expr ne)
         (type-case not-expr ne
           [not-not (n) (... (fn-for-not-expr n))]
           [not-prim (p) (... (fn-for-prim-expr p))]))
       (define (fn-for-prim-expr pe)
         (type-case prim-expr  pe
           [int-prim (i) (... (fn-for-integer i))]
           [id-prim (i) (... (fn-for-identifier i))]
           [expr-prim (e) (... (fn-for-expr e))]))
       (define (fn-for-integer i) (... (INTEGER-n i)))
       (define (fn-for-identifier i) (... (IDENTIFIER-i i)))]
      (fn-for-pgm p0))))

