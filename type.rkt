#lang racket
(require plai/datatype)
(provide (all-defined-out))

;; type.rkt - Types and type checker

;; Types don't belong in this file
(define-type Type
  [IntegerType]
  [BooleanType]
  [UnknownType])