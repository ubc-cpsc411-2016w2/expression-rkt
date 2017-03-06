#lang racket

(require plai/datatype)
(require "ast.rkt")


;; ast-dot.rkt - Visualize a given Expression abstract syntax tree

(define edges "")
(define nodes  "")

(define (reset-graph)
  (begin
    (set! edges "")
    (set! nodes "")))

;; Symbol String -> Void
;; emit a node to the graph
;; Effect: updates nodes
(define (emit-node n lbl)
  (set! nodes (string-append nodes (format "~a [label=\"~a\"]\n" n lbl))))

(define (new-node lbl)
  (let ([g (gensym)])
    (begin
      (emit-node g lbl)
      g)))

;; Symbol (Symbol ...) -> Void
;; emit a set of edges to the graph
;; Effect: updates edges
(define (emit-edges s . t*)
  (let ([e (format "~a -> ~a"
                   s
                   (string-append 
                    "{ "
                    (apply string-append (map (λ (t) (format "~a " t)) t*))
                    "}"))])
    (set! edges (string-append edges e))))

;; -> Void
;; Print the graph
;; Effect: print to standard output
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
                   (let* ([g (new-node "Id" #;"IdentifierExp")]
                          [g1 (new-node i)]
                          [_ (emit-edges g g1)])
                     g)]
    [IntegerLiteral (n)
                    (let* ([g (new-node "Int" #;"IntegerLiteral")]
                           [g1 (new-node n)]
                           [_ (emit-edges g g1)])
                      g)]
    [Plus (lhs rhs)
          (let* ([g (new-node "+" #;"Plus")]
                 [g1 (expr->dot lhs)]
                 [g2 (expr->dot rhs)]
                 [_ (emit-edges g g1 g2)])           
            g)]
    [Minus (lhs rhs)
           (let* ([g (new-node "-" #;"Minus")]
                  [g1 (expr->dot lhs)]
                  [g2 (expr->dot rhs)]
                  [_ (emit-edges g g1 g2)])           
             g)]
    [Times (lhs rhs)
           (let* ([g (new-node "*" #;"Times")]
                  [g1 (expr->dot lhs)]
                  [g2 (expr->dot rhs)]
                  [_ (emit-edges g g1 g2)])           
             g)]
    [LessThan (lhs rhs)
              (let* ([g (new-node "<" #;"LessThan")]
                     [g1 (expr->dot lhs)]
                     [g2 (expr->dot rhs)]
                     [_ (emit-edges g g1 g2)])           
                g)]
    [Not (e)
         (let* ([g (new-node "!" #;"Not")]
                [g1 (expr->dot e)]
                [_ (emit-edges g g1)])
           g)]
    [Conditional (pred conseq altern)
                 (let* ([g (new-node "?:" #;"Conditional")]
                        [g1 (expr->dot pred)]
                        [g2 (expr->dot conseq)]
                        [g3 (expr->dot altern)]
                        [_ (emit-edges g g1 g2 g3)])
                   g)]))


;; Stmt -> Symbol
;; produces a unique node symbol corresponding to s
;; Effect: emits node and edge statements for s
(define (stmt->dot s)
  (type-case Statement s
    [Assign (i e)
            (let* ([g (new-node "=" #;"Assign")]
                   [g1 (new-node i)]
                   [g2 (expr->dot e)]
                   [_ (emit-edges g g1 g2)])
              g)]
    [Print (e)
           (let* ([g (new-node "print" #;"Print")]
                  [g1 (expr->dot e)]
                  [_ (emit-edges g g1)])
             g)]))


;; (ilistof Stmt) -> Symbol
;; produces a unique node symbol corresponding to s*
;; Effect: emits node and edge statements for s*
(define (stmt*->dot s*)
  (cond
    [(cons? s*)
     (let* ([g (new-node ";")]
            [g1 (stmt->dot (car s*))]
            [g2 (stmt*->dot (cdr s*))]
            [_ (emit-edges g g1 g2)])
       g)]
    [else ;; last statement
     (stmt->dot s*)]))


;; FileName Program -> Void
;; Emit the DOT file for p to standard out
(define (pgm->dot p)
  (begin
    (reset-graph)
    (type-case _Program p
      [Program (a* pr)
               (let* ([g (new-node "Program")]
                      [g1 (stmt*->dot (append a* pr))]
                      [_ (emit-edges g g1)])
                 g)])  
    (print-graph)))


;; FileName Program -> Void
;; Emit the DOT file for p to fname
(define (pgm->dotfile fname p)
  (with-output-to-file fname
    (λ () (pgm->dot p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;; Corresponding DOT file
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

(define (fib)
  (delete-file "fib.dot")
  (pgm->dotfile "fib.dot" fib-ast)
  (system "/opt/local/bin/dot -Tpng fib.dot > fib.png"))

(define (condd)
  (delete-file "cond.dot")
  (pgm->dotfile "cond.dot" fib-ast)
  (system "/opt/local/bin/dot -Tpng cond.dot > cond.png"))



(module* read #f
  (require "scanner.rkt")
  (require "parser.rkt")
  (require "tree-abstraction.rkt")
  (provide (all-defined-out))

  ;; FileName -> Program
  ;; produce an Expression AST from the given file
  (define (ast file)
    (abstract-parse-tree
     (parse-tokens
      (scan-file file)))))

