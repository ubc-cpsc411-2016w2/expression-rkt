#lang racket

(require plai/datatype)
(require "parse-tree.rkt")
(require "token.rkt")
(require "graphviz.rkt")

;; parse-dot.rkt - Visualize a given Expression parse tree

(define (program->dot p0)
  (let ([... (void)])
    (local
      [(define (fn-for-pgm p)
         (type-case pgm p
           [print-pgm (e)
                      (let* ([g (new-node "Program")]
                             [g1 (token->dot (PRINT))]
                             [g2 (fn-for-expr e)]
                             [_ (emit-edges g g1 g2)])
                        g)]
           [stmt-cons (s p)
                      (let* ([g (new-node "Program")]
                             [g1 (fn-for-stmt s)]
                             [g2 (fn-for-pgm p)]
                             [_ (emit-edges g g1 g2)])
                        g)]))
       (define (fn-for-stmt s)
         (type-case stmt s
           [asgn-stmt (i e)
                      (let* ([g (new-node "Assign")]
                             [g1 (fn-for-identifier i)]
                             [g2 (token->dot (ASSIGN))]
                             [g3 (fn-for-expr e)]
                             [g4 (token->dot (SEMICOLON))]
                             [_ (emit-edges g g1 g2 g3 g4)])
                        g)]))
       (define (fn-for-expr e)
         (type-case _expr e
           [expr (c e)
                 (let* ([g (new-node "Expr")]
                        [g1 (fn-for-comp-expr c)]
                        [g2 (fn-for-maybe-expr-suffix e)]
                        [_ (emit-edges g g1 g2)])
                   g)]))
       (define (fn-for-maybe-expr-suffix m)
         (type-case maybe m
           [None () (let* ([g (new-node "ExprSuffix")]
                           [g1 (new-node "empty")]
                           [_ (emit-edges g g1)])
                      g)]
           [Just (es)
                 (type-case _expr-sfx es
                   [expr-sfx (c a)
                             (let* ([g (new-node "ExprSuffix")]
                                    [g1 (token->dot (QUESTION))]
                                    [g2 (fn-for-expr c)]
                                    [g3 (token->dot (COLON))]
                                    [g4 (fn-for-expr a)]
                                    [_ (emit-edges g g1 g2 g3 g4)])
                               g)])]))
       (define (fn-for-comp-expr ce)
         (type-case _comp-expr ce
           [comp-expr (a cs)
                      (let* ([g (new-node "CompExpr")]
                             [g1 (fn-for-add-expr a)]
                             [g2 (fn-for-comp-sfx cs)]
                             [_ (emit-edges g g1 g2)])
                        g)]))
       (define (fn-for-comp-sfx cs)
         (cond [(null? cs) (let* ([g (new-node "CompSuffix")]
                                  [g1 (new-node "empty")]
                                  [_ (emit-edges g g1)])
                             g)]
               [else (let* ([g (new-node "CompSuffix")]
                            [g1 (token->dot (SMALLER))]
                            [g2 (fn-for-add-expr (first cs))]
                            [g3 (fn-for-comp-sfx (rest cs))]
                            [_ (emit-edges g g1 g2 g3)])
                       g)]))
       (define (fn-for-add-expr ae)
         (type-case _add-expr ae
           [add-expr (m as)
                     (let* ([g (new-node "AddExpr")]
                            [g1 (fn-for-mult-expr m)]
                            [g2 (fn-for-add-sfx as)]
                            [_ (emit-edges g g1 g2)])
                       g)]))
       (define (fn-for-add-sfx as)
         (cond [(null? as) (let* ([g (new-node "AddSuffix")]
                                  [g1 (new-node "empty")]
                                  [_ (emit-edges g g1)])
                             g)]
               [else 
                (type-case _add-sfx (first as)
                  [add-sfx (o m)
                           (let* ([g (new-node "AddSuffix")]
                                  [g1 (fn-for-add-op o)]
                                  [g2 (fn-for-mult-expr m)]
                                  [g3 (fn-for-add-sfx (rest as))]
                                  [_ (emit-edges g g1 g2 g3)])
                             g)])]))
       (define (fn-for-add-op o)
         (cond
           [(PLUS? o) (token->dot o)]
           [(MINUS? o) (token->dot o)]))
       (define (fn-for-mult-expr me)
         (type-case _mult-expr me
           [mult-expr (n ms)
                      (let* ([g (new-node "MultExpr")]
                             [g1 (fn-for-not-expr n)]
                             [g2 (fn-for-mult-sfx ms)]
                             [_ (emit-edges g g1 g2)])
                        g)]))
       (define (fn-for-mult-sfx ms)
         (cond [(null? ms) (let* ([g (new-node "MultSuffix")]
                                  [g1 (new-node "empty")]
                                  [_ (emit-edges g g1)])
                             g)]
               [else
                (let* ([g (new-node "MultSuffix")]
                       [g1 (token->dot (MULT))]
                       [g2 (fn-for-not-expr (first ms))]
                       [g3 (fn-for-mult-sfx (rest ms))]
                       [_ (emit-edges g g1 g2 g3)])
                  g)]))
       (define (fn-for-not-expr ne)
         (type-case not-expr ne
           [not-not (n)
                    (let* ([g (new-node "NotExpr")]
                           [g1 (token->dot (NOT))]
                           [g2 (fn-for-not-expr n)]
                           [_ (emit-edges g g1 g2)])
                      g)]
           [not-prim (p) (let* ([g (new-node "NotExpr")]
                                [g1 (fn-for-prim-expr p)]
                                [_ (emit-edges g g1)])
                           g)]))
       (define (fn-for-prim-expr pe)
         (type-case prim-expr  pe
           [int-prim (i) (let* ([g (new-node "PrimExpr")]
                                [g1 (fn-for-integer i)]
                                [_ (emit-edges g g1)])
                           g)]
           [id-prim (i) (let* ([g (new-node "PrimExpr")]
                               [g1 (fn-for-identifier i)]
                               [_ (emit-edges g g1)])
                          g)]

           [expr-prim (e) (let* ([g (new-node "PrimExpr")]
                                 [g1 (token->dot (LPAREN))]
                                 [g2 (fn-for-expr e)]
                                 [g3 (token->dot (RPAREN))]
                                 [_ (emit-edges g g1 g2 g3)])
                            g)]))
       (define (fn-for-integer i)
         (token->dot (INTEGER (INTEGER-n i))))
       (define (fn-for-identifier i)
         (token->dot (IDENTIFIER (IDENTIFIER-i i))))]
      (begin
        (reset-graph)
        (let* ([g (fn-for-pgm p0)])
          (print-graph))))))


;; Token -> Symbol
;; emit a node for the relevant token
(define (token->dot tok)
  (new-node (format "\\\"~v\\\"" tok)))


(define (pgm->dotfile fname pgm)
  (with-output-to-file fname
    (λ () (program->dot pgm))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define print-5-tree
  (print-pgm
   (expr
    (comp-expr
     (add-expr
      (mult-expr
       (not-prim
        (int-prim (INTEGER 5)))
       '())
      '())
     '())
    (None))))
  
(module* read #f
  (require "scanner.rkt")
  (require "parser.rkt")
  (require "tree-abstraction.rkt")
  (provide exp->dotfile print-5)

  (define (force-delete-file p)
    (if (file-exists? p)
        (delete-file p)
        (void)))
  
  ;; FileName -> Pgm
  ;; produce an Expression parse tree from the given file
  (define (parse file)
    (parse-tokens
      (scan-file file)))

  (define (print-5)
    (force-delete-file "print5.dot")
    (pgm->dotfile "print5.dot"  print-5-tree)
    (system "/opt/local/bin/dot -Tpng print5.dot > print5.png"))

  (define (exp->dotfile name)
    (force-delete-file (format "~a-pt.dot" name))
    (pgm->dotfile (format "~a-pt.dot" name)
                  (parse (format "sample/~a.exp" name)))
    (system
     (format "/opt/local/bin/dot -Tpng ~a-pt.dot > ~a-pt.png" name name)))
  )