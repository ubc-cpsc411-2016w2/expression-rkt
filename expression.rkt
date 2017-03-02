#lang racket
(require plai/datatype)
(require test-engine/racket-tests)
;(require test-engine/text-ui)
;; expression.rkt - Aspects of the Expression language, implemented in Racket


;;
;; Scanning
;;

;; Tokens: tokens represent the *output* of scanning.  They do not reflect
;; fully the *input* to scanning,

;; Note that the grouping parentheses are "quoted" using
;; backslashes.  This is one way to distinguish the characters "(" and ")" from
;; the regular expression grouping constructs.
(define-type token
  [LPAREN]                           ;; "("
  [RPAREN]                           ;; ")"
  [ASSIGN]                           ;; "="
  [QUESTION]                         ;; "?"
  [COLON]                            ;; ":"
  [PLUS]                             ;; "+"
  [MINUS]                            ;; "-"
  [MULT]                             ;; "*"
  [SMALLER]                          ;; "<"
  [NOT]                              ;; "!"
  [SEMICOLON]                        ;; ";"
  [INTEGER (n integer?)]             ;; "[0-9]+"
  [IDENTIFIER (i symbol?)]           ;; "[A-za-z]\([A-za-z]|"_"|[0-9])*"
  [PRINT])


(define (fn-for-token tok)
  (type-case token tok
    [LPAREN () #f]                           ;; "("
    [RPAREN () #f]                           ;; ")"
    [ASSIGN () #f]                           ;; "="
    [QUESTION () #f]                         ;; "?"
    [COLON () #f]                            ;; ":"
    [PLUS () #f]                             ;; "+"
    [MINUS () #f]                            ;; "-"
    [MULT () #f]                             ;; "*"
    [SMALLER () #f]                          ;; "<"
    [NOT () #f]                              ;; "!"
    [SEMICOLON () #f]                        ;; ";"
    [INTEGER (n) (#f n)]                     ;; "[0-9]+"
    [IDENTIFIER (i) (#f i)]                  ;; "[A-za-z]\([A-za-z]|"_"|[0-9])*"
    [PRINT () #f]))


;; String -> (listof Token)
;; produce the list of tokens corresponding to the given string.
(check-expect (scan-string "") empty)
(check-expect (scan-string "(") (list (LPAREN)))
(check-expect (scan-string "()") (list (LPAREN) (RPAREN)))
(check-expect (scan-string "(77)22d65") (list (LPAREN) (INTEGER 77) (RPAREN)
                                              (INTEGER 22) (IDENTIFIER 'd65)))
(check-expect (scan-string "44//fee fi fo fum\ndid")
              (list (INTEGER 44) (IDENTIFIER 'did)))
(check-expect (scan-string "44/* fee fi fo * fum*/did")
              (list (INTEGER 44) (IDENTIFIER 'did)))

(define (scan-string str)
  (scan (string->list str)))


;; (listof Char) -> (listof Token)
;; produce the list of tokens corresponding to the given list of characters
(define (scan loc0)
  ;; Accumulator: loc is (listof Char)
  ;; interp: remaining characters
  ;; Accumulator: curr is (listof Char) or false
  ;; interp: longest current scan 
  (local [;; Initial State of the scanner
          ;; (listof Char) String -> (listof Token)
          (define (start loc curr)
            (cond
              [(empty? loc) empty]
              [else
               (case (first loc)
                 [(#\() (cons (LPAREN) (start (rest loc) false))]
                 [(#\)) (cons (RPAREN) (start (rest loc) false))]
                 [(#\=) (cons (ASSIGN) (start (rest loc) false))]
                 [(#\?) (cons (QUESTION) (start (rest loc) false))]
                 [(#\:) (cons (COLON) (start (rest loc) false))]
                 [(#\+) (cons (PLUS) (start (rest loc) false))]
                 [(#\-) (cons (MINUS) (start (rest loc) false))]
                 [(#\*) (cons (MULT) (start (rest loc) false))]
                 [(#\<) (cons (SMALLER) (start (rest loc) false))]
                 [(#\!) (cons (NOT) (start (rest loc) false))]
                 [(#\;) (cons (SEMICOLON) (start (rest loc) false))]
                 ;[(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                 [(#\/) (to-comment (rest loc) "/")]
                 [else
                  (cond
                    [(char-numeric? (first loc))
                     (in-int (rest loc) (string (first loc)))]
                    [(char-alphabetic? (first loc))
                     (in-ident (rest loc) (string (first loc)))]
                    [(char-whitespace? (first loc))
                     (start (rest loc) false)]
                    [else (error "Bad character" (first loc))])])]))
          
          ;; (listof Char) String -> (listof Token)
          (define (in-ident loc curr)
            (cond
              [(empty? loc) (cons (IDENTIFIER (string->symbol curr))
                                  (start loc false))]
              [(or (char-alphabetic? (first loc))
                   (char=? (first loc) #\_)
                   (char-numeric? (first loc)))
               (in-ident (rest loc) (string-append curr (string (first loc))))]
              [else
               (let* ([sym (string->symbol curr)]
                      [tok (if (equal? sym 'print)
                               (PRINT)
                               (IDENTIFIER sym))])
                 (cons tok (start loc false)))]))
          
          ;; (listof Char) String -> (listof Token)
          (define (in-int loc curr)
            (cond
              [(empty? loc) (cons (INTEGER (string->number curr))
                                  (start loc false))]
              [(char-numeric? (first loc))
               (in-int (rest loc) (string-append curr (string (first loc))))]
              [else (cons (INTEGER (string->number curr))
                          (start loc false))]))
          
          ;; (listof Char) String -> (listof Token)
          (define (to-comment loc curr)
            (cond
              [(empty? loc) (error "Trailing /.")]
              [(case  (first loc)
                 [(#\/) (line-comment (rest loc) (string-append curr "/"))]
                 [(#\*) (block-comment (rest loc) (string-append curr "*"))]
                 [else (error "Bad character after /." (first loc))])]))
          
          ;; (listof Char) String -> (listof Token)
          (define (line-comment loc curr)
            (cond
              [(empty? loc) (start empty false)]
              [(char=? (first loc) #\newline) (start (rest loc) false)]
              [else
               (line-comment (rest loc)
                             (string-append curr (string (first loc))))])) 
          
          ;; (listof Char) String -> (listof Token)
          (define (block-comment loc curr)
            (cond
              [(empty? loc) (error "String ended inside block comment.")]
              [(char=? (first loc) #\*)
               (may-end-block (rest loc) (string-append curr "*"))]
              [else
               (block-comment (rest loc)
                              (string-append curr (string (first loc))))]))
          (define (may-end-block loc curr)
            (cond
              [(empty? loc) (error "String ended inside block comment.")]
              [(char=? (first loc) #\/) (start (rest loc) false)]
              [else
               (block-comment (rest loc)
                              (string-append curr (string (first loc))))]))]
    
    ;; Accumulator Trampoline
    (start loc0 false)))

;; String -> String
;; produce a string of the given file.
(define (file->string fname)
  (port->string (open-input-file fname)))

;; String -> (listof Char)
;; produce a list of characters from the given file.
(define (file->list fname)
  (string->list (file->string fname)))

;; String -> (listof Token)
;; Scan the file with the given name
(define (scan-file fname)
  (scan-string (port->string (open-input-file fname))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parsing - Produce a Concrete Parse Tree from a Stream of Tokens
;; (and vice-versa).  There is a 1-to-1 relationship between
;; parse trees and streams of tokens:
;;  see parse-tree->tokens and parse-tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core Context-Free LL(1) Grammar (desugared)
;;
(define (maybe? p?)
  (λ (x)
    (or (empty? x) (p? x))))

(define (fn-for-maybe e)
  (let ([... (void)])
    (cond
      [(empty? e) (...)]
      [else (... e)])))

(define (fold-maybe c b e)
  (cond [(empty? e) b]
        [else (c e)]))

(define (listof? p?)
  (λ (x)
    (and (list? x)
         (andmap p? x))))

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
(define-type _expr
  [expr (c comp-expr?) (e (maybe? expr-sfx?))])

(define-type _expr-sfx
  [expr-sfx (conseq expr?) (altern expr?)])


;; CompExpression ::= AddExpression CompSuffix
;; CompSuffix ::= ε
;;              | (SMALLER) AddExpression
(define-type _comp-expr
  [comp-expr (a add-expr?) (c* (listof? comp-sfx?))])

(define-type _comp-sfx
  [comp-sfx (a add-expr?)])


;; AddExpression ::= MultExpression AddList
;; AddList ::= ε
;;            | AddOp MultExpression AddList
;; AddOP ::= (PLUS)
;;         | (MINUS)
;; RG: Note the cheating below
(define-type _add-expr
  [add-expr (m mult-expr?) (a* (listof add-sfx?))])

(define-type _add-sfx
  [add-sfx (o add-op?) (m mult-expr?)])

(define (add-op? x) (or (PLUS? x) (MINUS? x)))


;; MultExpression ::= NotExpression MultList
;; MultList ::= ε
;;           | (MULT) NotExpression MultList
(define-type _mult-expr
  [mult-expr (n not-expr?) (m* (listof mult-sfx?))])

(define (mult-sfx? x) (not-expr? x))



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
           [expr (c e) (... (fn-for-comp-expr c) (fn-for-maybe e))]))
       (define (fn-for-expr-sfx es)
         (type-case _expr-sfx es
           [expr-sfx (c a) (... (fn-for-expr c) (fn-for-expr a))]))
       (define (fn-for-comp-expr ce)
         (type-case _comp-expr ce
           [comp-expr (a c*)
                      (... (fn-for-add-expr a) (map fn-for-comp-sfx c*))]))
       (define (fn-for-comp-sfx cs)
         (type-case _comp-sfx cs
           [comp-sfx (a) (... (fn-for-add-expr a))]))
       (define (fn-for-add-expr ae)
         (type-case _add-expr ae
           [add-expr (m a*)
                     (... (fn-for-mult-expr m) (map fn-for-add-sfx a*))]))
       (define (fn-for-add-sfx as)
         (type-case _add-sfx as
           [add-sfx (o m) (... (fn-for-add-op o) (fn-for-mult-expr m))]))
       (define (fn-for-add-op o)
         (cond
           [(PLUS? o) (...)]
           [(MINUS? o) (...)]))         
       (define (fn-for-mult-expr me)
         (type-case _mult-expr me
           [mult-expr (n m*)
                      (... (fn-for-not-expr n) (map fn-for-mult-sfx m*))]))
       (define (fn-for-mult-sfx ms) (... (fn-for-not-expr ms)))
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


;; Pgm -> (listof Token)
;; Produce the list of tokens corresponding to the given parse tree
;; i.e. this is an unparser (inverse of parsing)

;; Just look how horrifying a concrete parse tree is!
;; All this to get LL(1) and to get precedence right!
(check-expect (parse-tree->tokens
               (print-pgm (expr
                           (comp-expr
                            (add-expr
                             (mult-expr
                              (not-prim (int-prim (INTEGER 0)))
                              empty)
                             empty)
                            empty)
                           empty)))
              (list (PRINT) (INTEGER 0)))

;(define (parse-tree->tokens p) empty) ; stub

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
         [comp-expr (a c*)
                    (append (fn-for-add-expr a)
                            (apply append
                                   (map fn-for-comp-sfx c*)))]))
     (define (fn-for-comp-sfx cs)
       (type-case _comp-sfx cs
         [comp-sfx (a) (cons (SMALLER) (fn-for-add-expr a))]))
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

(define (inverts? lot)
  (equal? lot
          (parse-tree->tokens (parse-tokens lot))))

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
                '())))

(check-satisfied (list (PRINT) (INTEGER 5)) inverts?)

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
              (expr-sfx c a))]
           [else empty])) ;; check follow sets
       (define (parse-comp-expr)
         (comp-expr (parse-add-expr) (parse-comp-sfx)))
       (define (parse-comp-sfx)
         (cond
           [(SMALLER? (peek))
            (let* ([_ (eat SMALLER?)]
                   [a (parse-add-expr)]
                   [c* (parse-comp-sfx)])
              (cons (comp-sfx a) c*))]
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


(define (check-parse fname)
  (inverts? (scan-file fname)))

;; Test the parser on Expression language samples
#;(map (λ (p)
         (let ([p (string-append "samples/" (path->string p))])
           (cons p
                 (with-handlers ([exn:fail? (λ (e) "fail")])
                   (inverts? (scan-file p))))))
       (directory-list "samples"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract Syntax Trees
;; This is the *real* goal:  The abstract syntax tree captures our
;; high-level understanding of programs, without concern for
;; parentheses, operator precedence, and the other concerns that arise
;; from representing a conceptually tree-structured information as
;; linear text.
;;
;; The parse tree recovers enough of that structure from the source
;; program that we can push it the rest of the way in one step.
;;
;; Note, though, that abstract syntax trees DO NOT have a 1-to-1
;; relationship with parse trees.  Many parse trees can correspond
;; to the same abstract syntax tree.  To see this, think of how many
;; ways you can add extra parentheses to a program and how it affects
;; the parse tree but NOT the abstract syntax tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(test)