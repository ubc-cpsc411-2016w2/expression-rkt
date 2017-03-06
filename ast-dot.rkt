#lang racket

(require plai/datatype)
(require "ast.rkt")


;; ast-dot.rkt - Visualize a given Expression abstract syntax tree

(define edges "")
(define nodes  "")

(define (reset)
  (begin
    (set! edges "")
    (set! nodes "")))

(define (emit-node n lbl)
  (set! nodes (string-append nodes (format "~a [label=\"~a\"]\n" n lbl))))

(define (emit-edge x)
  (set! edges (string-append edges x)))

(define (print-graph)
  (begin
    (printf "digraph ast {\n")
    (printf nodes)
    (printf edges)
    (printf "}\n")))
    

;; Expr -> Symbol
;; produces a unique node symbol corresponding to e
;; Effect: emits node and edge statements for e
(define (expr->dot e)
  (type-case Expression e
    [IdentifierExp (i)
                    (let ([g (gensym)]
                          [g1 (gensym)])
                      (emit-node g "Id" #;"IdentifierExp")
                      (emit-node g1 (format "~a" i))
                      (emit-edge (format "~a -> { ~a }\n" g g1))
                      g)]
    [IntegerLiteral (n)
                    (let ([g (gensym)]
                          [g1 (gensym)])
                      (emit-node g "Int" #;"IntegerLiteral")
                      (emit-node g1 (format "~a" n))
                      (emit-edge (format "~a -> { ~a }\n" g g1))
                      g)]
    [Plus (lhs rhs)
          (let ([g (gensym)])
            (emit-node g "+" #;"Plus")
            (let ([g1 (expr->dot lhs)]
                  [g2 (expr->dot rhs)])
              (emit-edge (format "~a -> { ~a ~a }\n" g g1 g2))
              g))]
    [Minus (lhs rhs)
          (let ([g (gensym)])
            (emit-node g "-" #;"Minus")
            (let ([g1 (expr->dot lhs)]
                  [g2 (expr->dot rhs)])
              (emit-edge (format "~a -> { ~a ~a }\n" g g1 g2))
              g))]
    [Times (lhs rhs)
          (let ([g (gensym)])
            (emit-node g "*" #;"Times")
            (let ([g1 (expr->dot lhs)]
                  [g2 (expr->dot rhs)])
              (emit-edge (format "~a -> { ~a ~a }\n" g g1 g2))
              g))]
    [LessThan (lhs rhs)
          (let ([g (gensym)])
            (emit-node g "<" #;"LessThan")
            (let ([g1 (expr->dot lhs)]
                  [g2 (expr->dot rhs)])
              (emit-edge (format "~a -> { ~a ~a }\n" g g1 g2))
              g))]
    [Not (e)
          (let ([g (gensym)])
            (emit-node g "!" #;"Not")
            (let ([g1 (expr->dot e)])
              (emit-edge (format "~a -> { ~a }\n" g g1))
              g))]
    [Conditional (pred conseq altern)
          (let ([g (gensym)])
            (emit-node g "?:" #;"Conditional")
            (let ([g1 (expr->dot pred)]
                  [g2 (expr->dot conseq)]
                  [g3 (expr->dot altern)])
              (emit-edge (format "~a -> { ~a ~a ~a }\n" g g1 g2 g3))
              g))]))


;; Stmt -> Symbol
;; produces a unique node symbol corresponding to s
;; Effect: emits node and edge statements for s
(define (stmt->dot s)
  (type-case Statement s
    [Assign (i e)
            (let ([g (gensym)]
                  [g1 (gensym)])
              (emit-node g "=" #;"Assign")
              (emit-node g1 (format "~a" i))
              (let ([g2 (expr->dot e)])
                (emit-edge (format "~a -> { ~a ~a }\n" g g1 g2))
                g))]
    [Print (e)
           (let ([g (gensym)])
             (emit-node g "print" #;"Print")
             (let ([g1 (expr->dot e)])
               (emit-edge (format "~a -> { ~a }\n" g g1)))
             g)]))


;; (ilistof Stmt) -> Symbol
;; produces a unique node symbol corresponding to s*
;; Effect: emits node and edge statements for s*
(define (stmt*->dot s*)
  (cond
    [(cons? s*)
     (let ([g (gensym)])
       (emit-node g ";")
       (let ([g1 (stmt->dot (car s*))]
             [g2 (stmt*->dot (cdr s*))])
         (emit-edge (format "~a -> { ~a ~a }\n" g g1 g2))
         g))]
    [else ;; last statement
     (stmt->dot s*)]))


;; FileName Program -> Void
;; Emit the DOT file for p to standard out
(define (pgm->dot p)
  (begin
    (reset)
    (type-case _Program p
      [Program (a* pr)
               (let ([pgm (gensym)])
                 (emit-node pgm "Program")
                 (let ([g (stmt*->dot (append a* pr))]) ;; Improper list!
                   (emit-edge (format "~a -> { ~a }" pgm g))
                   pgm))])  
    (print-graph)))


;; FileName Program -> Void
;; Emit the DOT file for p to fname
(define (pgm->dotfile fname p)
  (with-output-to-file fname
    (λ () (pgm->dot p))))


(define ex1
  (Program  (list) (Print (IntegerLiteral 3))))

(define fib-ast
  (Program
   (list
    (Assign 'zero (IntegerLiteral 0))
    (Assign 'one (IntegerLiteral 1))
    (Assign 'two (Plus (IdentifierExp 'one) (IdentifierExp 'zero)))
    (Assign 'three (Plus (IdentifierExp 'two) (IdentifierExp 'one)))
    (Assign 'four (Plus (IdentifierExp 'three) (IdentifierExp 'two)))
    (Assign 'five (Plus (IdentifierExp 'four) (IdentifierExp 'three))))
   (Print (IdentifierExp 'five))))


(define cond-ast
  (Program
   (list (Assign 'l (IntegerLiteral 5)) (Assign 'r (IntegerLiteral 8)))
   (Print
    (Conditional
     (LessThan (IdentifierExp 'l) (IdentifierExp 'r))
     (IntegerLiteral 10)
     (IntegerLiteral 20)))))


;; Example output of the code
(define cond-dot
  "
digraph cond {
  g1 [label = \"Program\"]
  g2 [label = \";\"]

  g3 [label = \"Assign\"]
  g31 [label = \"l\"]
  g32 [label = \"IntegerLiteral\"]
  g33 [label = \"5\"]
  g4 [label = \";\"]
 

  g5 [label = \"Assign\"]
  g51 [label = \"r\"]
  g52 [label = \"IntegerLiteral\"]
  g53 [label = \"8\"]

  g6 [label = \"Print\"]

  g7 [label = \"Conditional\"]
  g8 [label = \"LessThan\"]

  g11 [label = \"IdentifierExp\"]
  g111 [label = \"l\"]
  g12 [label = \"IdentifierExp\"]
  g121 [label = \"r\"]

  g9 [label = \"IntegerLiteral\"]
  g91 [label = \"10\"]

  g10 [label = \"IntegerLiteral\"]
  g101 [label = \"20\"]

  g1 -> { g2 }

  g2 -> { g3 g4 }
  g3 -> { g31 g32 }
  g32 -> { g33 }

  g4 -> {g5 g6}

  g5 -> { g51 g52 }
  g52 -> { g53 }

  g6 -> { g7 }

  g7 -> { g8 g9 g10 }

  g8 -> { g11 g12 }

  g9 -> { g91 }

  g10 -> { g101 }

  g11 -> { g111 }

  g12 -> { g121 }
}
")

  

(module* read #f
  (require "scanner.rkt")
  (require "parser.rkt")
  (require "tree-abstraction.rkt")
  (provide (all-defined-out))
  (define (ast file)
    (abstract-parse-tree
     (parse-tokens
      (scan-file file)))))

