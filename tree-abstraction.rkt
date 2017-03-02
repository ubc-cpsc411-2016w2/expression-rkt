#lang racket
(require plai/datatype)

(module+ test
  (require test-engine/racket-tests))
(require "token.rkt")
(require "parse-tree.rkt")
(require "ast.rkt")

(provide abstract-parse-tree)

;; pgm -> Program

;; The corresponding abstract syntax tree is soooo much nicer!
(module+ test
(check-expect
 (abstract-parse-tree (print-pgm (expr
                                  (comp-expr
                                   (add-expr
                                    (mult-expr
                                     (not-prim (int-prim (INTEGER 0)))
                                     empty)
                                    empty)
                                   empty)
                                  (None))))
 (Program empty (Print (IntegerLiteral 0)))))

(define (abstract-parse-tree p0)
  (local
    [(define (fn-for-pgm p)
       (local [(define (Program-cons s p)
                 (Program (cons s (Program-a* p)) (Program-pr p)))]
         (type-case pgm p
           [print-pgm (e) (Program empty (Print (fn-for-expr e)))]
           [stmt-cons (s p) (Program-cons (fn-for-stmt s) (fn-for-pgm p))])))
     (define (fn-for-stmt s)
       (type-case stmt s
         [asgn-stmt (i e) (Assign (IDENTIFIER-i i) (fn-for-expr e))]))
     ;; General technique for transforming LL parse trees into abstract trees:
     ;; Pass the result of the first natural recursion to the second one
     ;; the second argument is essentially a "lost context" accumulator.
     (define (fn-for-expr e)
       (type-case _expr e
         [expr (c e)
               (fn-for-maybe-expr-suffix e (fn-for-comp-expr c))]))
     ;; Keep passing the result of the first recursion to the second
     (define (fn-for-maybe-expr-suffix m roe)
       (type-case maybe m
         [None () roe]
         [Just (es) (fn-for-expr-sfx es roe)]))
     ;; Now we can weave the first recursion into the second
     (define (fn-for-expr-sfx es roe)
       (type-case _expr-sfx es
         [expr-sfx (c a) (Conditional roe (fn-for-expr c) (fn-for-expr a))]))

     ;; This is analogous to the maybe case, but must handle suffix lists
     (define (fn-for-comp-expr ce)
       (type-case _comp-expr ce
         [comp-expr (a cs)
                    (fn-for-comp-sfx cs (fn-for-add-expr a))]))
     (define (fn-for-comp-sfx cs roe)
       (cond [(null? cs) roe]
             [else
              (LessThan roe
                        (fn-for-comp-sfx (rest cs)
                                         (fn-for-add-expr (first cs))))]))
     (define (fn-for-add-expr ae)
       (type-case _add-expr ae
         [add-expr (m as)
                   (fn-for-add-sfx as (fn-for-mult-expr m))]))
     (define (fn-for-add-sfx as roe)
       (cond [(null? as) roe]
             [else 
              (type-case _add-sfx (first as)
                [add-sfx (o m)
                         (fn-for-add-op
                          o
                          roe
                          (fn-for-add-sfx (rest as)
                                          (fn-for-mult-expr m)))])]))
     (define (fn-for-add-op o roe rsfx)
       (cond
         [(PLUS? o) (Plus roe rsfx)]
         [(MINUS? o) (Minus roe rsfx)]))         
     (define (fn-for-mult-expr me)
       (type-case _mult-expr me
         [mult-expr (n ms)
                    (fn-for-mult-sfx ms (fn-for-not-expr n))]))
     (define (fn-for-mult-sfx ms roe)
       (cond [(null? ms) roe]
             [else
              (Times roe
                     (fn-for-mult-sfx (rest ms)
                                      (fn-for-not-expr (first ms))))]))
     (define (fn-for-not-expr ne)
       (type-case not-expr ne
         [not-not (n) (Not (fn-for-not-expr n))]
         [not-prim (p) (fn-for-prim-expr p)]))
     (define (fn-for-prim-expr pe)
       (type-case prim-expr  pe
         [int-prim (i) (fn-for-integer i)]
         [id-prim (i) (fn-for-identifier i)]
         [expr-prim (e) (fn-for-expr e)]))
     (define (fn-for-integer i) (IntegerLiteral (INTEGER-n i)))
     (define (fn-for-identifier i) (IdentifierExp (IDENTIFIER-i i)))]
    (fn-for-pgm p0)))


(module+ test
  (test))

;;
;; Exercise the Syntax Tree Abstractor using real examples
;;

;; to use the following submodule:
;; (require (submod "." test))


(module+ test
  (require "scanner.rkt")
  (require "parser.rkt")
  (require "samples.rkt")
  (provide (all-defined-out))
  (define (file-tests)
    (map (λ (p)
           (cons (find-relative-path (current-directory) p)
                 (with-handlers ([exn:fail? (λ (e) "fail")])
                   (begin
                     (abstract-parse-tree
                      (parse-tokens
                       (scan-file p)))
                     true))))
         samples))
  (file-tests)
  ) ; module

