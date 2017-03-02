#lang racket

(provide (all-defined-out))

(define (listof? p?)
  (λ (x)
    (and (list? x)
         (andmap p? x))))