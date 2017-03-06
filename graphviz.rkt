#lang racket

;; graphviz.rkt - Tools for generating dot files for graphviz

(provide reset-graph
         new-node
         emit-edges
         print-graph)

         
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
                    "}\n"))])
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
