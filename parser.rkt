#lang racket
(require plai/datatype)
(module+ test
  (require test-engine/racket-tests))

(require "token.rkt") ;; for all of the token operators
(require "parse-tree.rkt") ;; for the parse tree definition

(provide parse-tree->tokens parse-tokens)

;;
;; parser.rkt - Produce a Concrete Parse Tree from a Stream of Tokens
;; (and vice-versa).
;;


;;
;; There is a 1-to-1 relationship between parse trees and streams of
;; tokens: see parse-tree->tokens and parse-tokens
;;



;; Pgm -> (listof Token)
;; Produce the list of tokens corresponding to the given parse tree
;; i.e. this is an unparser (inverse of parsing)

;; Just look how horrifying a concrete parse tree is!
;; All this to get LL(1) and to get precedence right!
(module+ test
    (check-expect (parse-tree->tokens
               (print-pgm (expr
                           (comp-expr
                            (add-expr
                             (mult-expr
                              (not-prim (int-prim (INTEGER 0)))
                              empty)
                             empty)
                            empty)
                           (None))))
              (list (PRINT) (INTEGER 0))))

(define (parse-tree->tokens p0)
  (local
    [(define (fn-for-pgm p)
       (type-case pgm p
         [print-pgm (e) (cons (PRINT) (fn-for-expr e))]
         [stmt-cons (s p) (append (fn-for-stmt s) (fn-for-pgm p))]))
     (define (fn-for-stmt s)
       (type-case stmt s
         [asgn-stmt (i e) (append (fn-for-identifier i)
                                  (list (ASSIGN))
                                  (fn-for-expr e)
                                  (list (SEMICOLON)))]))
     (define (fn-for-expr e)
       (type-case _expr e
         [expr (c es) (append (fn-for-comp-expr c)
                              (fold-maybe fn-for-expr-sfx empty es))]))
     (define (fn-for-expr-sfx es)
       (type-case _expr-sfx es
         [expr-sfx (c a) (append (list (QUESTION))
                                 (fn-for-expr c)
                                 (list (COLON))
                                 (fn-for-expr a))]))
     (define (fn-for-comp-expr ce)
       (type-case _comp-expr ce
         [comp-expr (a cs)
                    (append (fn-for-add-expr a)
                            (fn-for-comp-sfx cs))]))
     (define (fn-for-comp-sfx cs)
       (cond [(null? cs) empty]
             [else  (append (list (SMALLER))
                            (fn-for-add-expr (first cs))
                            (fn-for-comp-sfx (rest cs)))]))
     (define (fn-for-add-expr ae)
       (type-case _add-expr ae
         [add-expr (m a*) (append (fn-for-mult-expr m)
                                  (apply append
                                         (map fn-for-add-sfx a*)))]))
     (define (fn-for-add-sfx as)
       (type-case _add-sfx as
         [add-sfx (o m) (cons (fn-for-add-op o) (fn-for-mult-expr m))]))
     (define (fn-for-add-op o)
       (cond
         [(PLUS? o) (PLUS)]
         [(MINUS? o) (MINUS)]))         
     (define (fn-for-mult-expr me)
       (type-case _mult-expr me
         [mult-expr (n m*) (append (fn-for-not-expr n)
                                   (apply append
                                          (map fn-for-mult-sfx m*)))]))
     (define (fn-for-mult-sfx ms) (cons (MULT) (fn-for-not-expr ms)))
     (define (fn-for-not-expr ne)
       (type-case not-expr ne
         [not-not (n) (cons (NOT) (fn-for-not-expr n))]
         [not-prim (p) (fn-for-prim-expr p)]))
     (define (fn-for-prim-expr pe)
       (type-case prim-expr pe
         [int-prim (i) (fn-for-integer i)]
         [id-prim (i) (fn-for-identifier i)]
         [expr-prim (e) (append (list (LPAREN))
                                (fn-for-expr e)
                                (list (RPAREN)))]))
     (define (fn-for-integer i) (list (INTEGER (INTEGER-n i))))
     (define (fn-for-identifier i) (list (IDENTIFIER (IDENTIFIER-i i))))]
    (fn-for-pgm p0)))


;; (listof Token) -> Pgm
;; Produce a concrete parse tree corresponding to the given string of tokens

(module+ test
(check-expect (parse-tokens (list (PRINT) (INTEGER 5)))
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

(check-satisfied (list (PRINT) (INTEGER 5)) inverts?))

(define (parse-tokens tks0)
  ;; curr is (listof Token)
  ;; tokens left to be parsed (imperative accumulator)
  (let ([curr tks0]
        [... (void)])
    (local
      [(define (peek)
         (cond [(empty? curr) false]
               [else (first curr)]))
       (define (eat pred?)
         (cond [(empty? curr)  (pred? empty) #;(error "no tokens left")]
               [else
                (let ([result (first curr)])
                  (if (pred? result)
                      (begin
                        (set! curr (rest curr))
                        result)
                      (error "token mismatch" result)))]))

       (define (parse-pgm)
         (cond
           [(PRINT? (peek))
            (let* ([_ (eat PRINT?)]
                   [e (parse-expr)]
                   [_ (eat empty?)])
                   (print-pgm e))]
           [(IDENTIFIER? (peek))
            (stmt-cons (parse-stmt) (parse-pgm))]))
       (define (parse-stmt)
         (let* ([i (eat IDENTIFIER?)]
                [_ (eat ASSIGN?)]
                [e (parse-expr)]
                [_ (eat SEMICOLON?)])
           (asgn-stmt i e)))
       (define (parse-expr)
         (expr (parse-comp-expr) (parse-expr-sfx)))
       (define (parse-expr-sfx)
         (cond
           [(QUESTION? (peek))
            (let* ([_ (eat QUESTION?)]
                   [c (parse-expr)]
                   [_ (eat COLON?)]
                   [a (parse-expr)])
              (Just (expr-sfx c a)))]
           [else (None)])) ;; check follow sets
       (define (parse-comp-expr)
         (comp-expr (parse-add-expr) (parse-comp-sfx)))
       (define (parse-comp-sfx)
         (cond
           [(SMALLER? (peek))
            (let* ([_ (eat SMALLER?)]
                   [a (parse-add-expr)]
                   [c* (parse-comp-sfx)])
              (cons a c*))]
           [else empty])) ;; check follow sets
       (define (parse-add-expr)
         (add-expr (parse-mult-expr) (parse-add-sfx)))
       (define (parse-add-sfx)
         (cond
           [(add-op? (peek))
            (let* ([o (eat add-op?)]
                   [m (parse-mult-expr)]
                   [a* (parse-add-sfx)])
              (cons (add-sfx o m) a*))]
           [else empty]))    ;; check follow sets            
       (define (parse-mult-expr)
         (mult-expr (parse-not-expr) (parse-mult-sfx)))
       (define (parse-mult-sfx)
         (cond
           [(MULT? (peek))
            (let* ([_ (eat MULT?)]
                  [n (parse-not-expr)]
                  [m* (parse-mult-sfx)])
              (cons n m*))]
           [else empty])) ;; check follow sets
       (define (parse-not-expr)
         (cond
           [(NOT? (peek))
            (eat NOT?)
            (not-not (parse-not-expr))]
           [else (not-prim (parse-prim-expr))])) ;; check follow sets
       (define (parse-prim-expr)
         (cond
           [(INTEGER? (peek)) (int-prim (eat INTEGER?))]
           [(IDENTIFIER? (peek)) (id-prim (eat IDENTIFIER?))]
           [else
            (let* ([_ (eat LPAREN?)]
                   [e (parse-expr)]
                   [_ (eat RPAREN?)])
              (expr-prim e))])) ;; check follow sets
       ]
      (parse-pgm))))



;;
;; Some interesting testing infrastructure for parse-tokens and
;; parse-tree->tokens
;; 

;; (listof Token) -> Boolean
;; Produce true if parsing and the unparsing a list of tokens reconstructs it
(define (inverts? lot)
  (equal? lot
          (parse-tree->tokens (parse-tokens lot))))


(module+ test
  (test))


;;
;; Test the parser on Expression language samples
;;  (make sure that parsing is invertible to token strings)
;;

;; to use the following submodule:
;; (require (submod "." test))


(module+ test
  (require "scanner.rkt")
  (require "samples.rkt")
  (provide (all-defined-out))
  ;; String -> Boolean
  ;; Produce true if tokenizing the given file produces an inverting token list
  (define (check-parse fname)
    (inverts? (scan-file fname)))
  (define (file-tests)
    (map (λ (p) (cons (find-relative-path (current-directory) p)
                   (with-handlers ([exn:fail? (λ (e) "fail")])
                     (inverts? (scan-file p)))))
         samples))
  (file-tests)
  ) ; module

