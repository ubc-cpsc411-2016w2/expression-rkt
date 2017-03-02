#lang racket
(require plai/datatype)
(require "listof.rkt")

(provide (all-defined-out))

;;
;; ast.rkt - The Expression language abstract syntax
;;
(define-type Expression
  [IdentifierExp (i symbol?)]
  [IntegerLiteral (n integer?)]
  [Plus (lhs Expression?) (rhs Expression?)]
  [Minus (lhs Expression?) (rhs Expression?)]
  [Times (lhs Expression?) (rhs Expression?)]
  [LessThan (lhs Expression?) (rhs Expression?)]
  [Not (e Expression?)]
  [Conditional (pred Expression?) (conseq Expression?) (altern? Expression?)])

;; Technically Expression doesn't need Statement, since
;; Program doesn't use it.  We place it here for later generalization.
;; It is common for imperative programming languages to distinguish expressions
;; from statements
(define-type Statement
  [Assign (i symbol?) (e Expression?)]
  [Print (e Expression?)])

;; Programs
(define-type _Program
  [Program (a* (listof? Assign?)) (pr Print?)])


