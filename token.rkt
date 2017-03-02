#lang racket

(require plai/datatype)
(provide (except-out (all-defined-out) fn-for-token))

;; tokens.rkt
;; Tokens: tokens represent the *output* of scanning.  They do not reflect
;; fully the *input* to scanning, which is arbitrary strings of ascii text.

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

;; Template for token functions
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

;; Token -> String
;; produce the string representation of the given token
(define (token->string tok)
  (type-case token tok
    [LPAREN () "("]                          ;; "("
    [RPAREN () ")"]                          ;; ")"
    [ASSIGN () "="]                          ;; "="
    [QUESTION () "?"]                        ;; "?"
    [COLON () ":"]                           ;; ":"
    [PLUS () "+"]                            ;; "+"
    [MINUS () "-"]                           ;; "-"
    [MULT () "*"]                            ;; "*"
    [SMALLER () "<"]                         ;; "<"
    [NOT () "!"]                             ;; "!"
    [SEMICOLON () ";"]                       ;; ";"
    [INTEGER (n) (number->string n)]         ;; "[0-9]+"
    [IDENTIFIER (i) (symbol->string i)]      ;; "[A-za-z]\([A-za-z]|"_"|[0-9])*"
    [PRINT () "print"]))