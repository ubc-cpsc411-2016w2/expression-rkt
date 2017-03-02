#lang racket
(require plai/datatype)

(module+ test
  (require test-engine/racket-tests))

(require "token.rkt")  ;; Token datatype for scanning
(provide scan scan-file scan-string)

;;
;; scanner.rkt - Scanner for the Expressions Language
;;


;; String -> (listof Token)
;; produce the list of tokens corresponding to the given string.
(module+ test
  (check-expect (scan-string "") empty)
  (check-expect (scan-string "(") (list (LPAREN)))
  (check-expect (scan-string "()") (list (LPAREN) (RPAREN)))
  (check-expect (scan-string "(77)22d65") (list (LPAREN) (INTEGER 77) (RPAREN)
                                                (INTEGER 22) (IDENTIFIER 'd65)))
  (check-expect (scan-string "44//fee fi fo fum\ndid")
                (list (INTEGER 44) (IDENTIFIER 'did)))
  (check-expect (scan-string "44/* fee fi fo * fum*/did")
                (list (INTEGER 44) (IDENTIFIER 'did))))


;; The following function implements a scanner as a deterministic finite
;; automaton (DFA), derived from the regular expressions for Expression
;; language tokens.  States are represented as functions, and transitions
;; are represented using tail calls.

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


;;
;; Helpers for acquiring and scanning a string of characters
;;

;; String -> String
;; produce a string of the given file.
(define (file->string fname)
  (port->string (open-input-file fname)))

;; String -> (listof Char)
;; produce a list of characters from the given file.
(define (file->list fname)
  (string->list (file->string fname)))


;; String -> (listof Char)
;; scan the given string
(define (scan-string str)
  (scan (string->list str)))

;; String -> (listof Token)
;; Scan the file with the given name
(define (scan-file fname)
  (scan-string (port->string (open-input-file fname))))

(module+ test 
  (test))