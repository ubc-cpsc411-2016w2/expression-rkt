#lang racket
(require plai/datatype)
(require "ast.rkt")

(provide interp-pgm)

;;
;; interp.rkt - Interpreter for Expression Language
;;


;;
;; Environment - bind Expression variables to Racket mutable boxes.
;;

;; env ::= empty
;;       | x=e, env      
(define-type Env
  [env/empty]
  [env/entry (id symbol?) (binding box?) (tail Env?)])

;; Env symbol -> Box or false
;; produce box bound to symbol in environment, or #false if not found
(define (lookup env x)
  (type-case Env env
    [env/empty () false]
    [env/entry (y e env0)
                 (if (symbol=? x y)
                     e
                     (lookup env0 x))]))


;; Expression Env -> Int
;; interpret the given Expression
(define (interp-expr e env)
  (type-case Expression e
    [IdentifierExp (i) (unbox (lookup env i))]
    [IntegerLiteral (n) n]
    [Plus (lhs rhs) (+ (interp-expr lhs env)
                       (interp-expr rhs env))]
    [Minus (lhs rhs) (- (interp-expr lhs env)
                        (interp-expr rhs env))]
    [Times (lhs rhs) (* (interp-expr lhs env)
                        (interp-expr rhs env))]
    [LessThan (lhs rhs) (if (< (interp-expr lhs env)
                               (interp-expr rhs env))
                            1
                            0)]
    [Not (e) (if (zero? (interp-expr e env))
                 1
                 0)]
    [Conditional (pred conseq altern)
                 (if (not (zero? (interp-expr pred env)))
                     (interp-expr conseq env)
                     (interp-expr altern env))]))



;; Statement Env -> Env
;; interpret the given Statement, which may update the environment
(define (interp-stmt s env)
  (type-case Statement s
    [Assign (i e)
            (let ([v (interp-expr e env)]
                  [env^ (if (lookup env i)
                            env
                           (env/entry i (box 0) env))])
              (let ([b (lookup env^ i)])
                (begin (set-box! b v)
                       env^)))]
    [Print (e) (begin
                 (printf "~s\n" (interp-expr e env))
                 env)]))

;; Program -> Void
;; interpret the given Program
(define (interp-pgm p)
  (type-case _Program p
    [Program (a* pr)
             (let ([cmb (λ (s env) (interp-stmt s env))]
                   [env0 (env/empty)])
               (let ([env-pr (foldl cmb env0 a*)])
                 (begin (interp-stmt pr env-pr)
                        (void))))]))

(module* test #f
  (require "scanner.rkt")
  (require "parser.rkt")
  (require "tree-abstraction.rkt")
  (provide (all-defined-out))
  (define (fib)
    (interp-pgm
     (abstract-parse-tree
      (parse-tokens
       (scan-file "sample/fib.exp"))))))

