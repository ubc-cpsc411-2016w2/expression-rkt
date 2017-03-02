#lang racket

;; samples.rkt - sample Expression programs to use for testing

(provide samples)

(define (complete-path p) (build-path (current-directory) "sample" p))
(define (has-exp-extension? p) (equal? (path-get-extension p) #".exp"))
(define (replace-exp-with-out p) (path-replace-extension p #".out"))

(define samples
  (map complete-path
       (filter has-exp-extension?
               (directory-list "sample"))))
