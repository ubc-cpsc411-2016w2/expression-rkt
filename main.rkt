#lang racket

;;
;; driver.rkt - Top-level driver function for the entire
;;   Expression interpreter
;;

(require "scanner.rkt")
(require "parser.rkt")
(require "tree-abstraction.rkt")
(require "interp.rkt")

(provide interp-file)

;; String -> Void
(define (interp-file fname)
    (interp-pgm
     (abstract-parse-tree
      (parse-tokens
       (scan-file fname)))))


(module* main #f
  (define args (current-command-line-arguments))
  (if (zero? (vector-length args))
      (void)
      (let ([fname (vector-ref args 0)])
        (interp-file fname)))
  ) ; module


(module* test #f
  (require rackunit)
  (require rackunit/text-ui)
  (require "samples.rkt")
  (provide file-tests)

  (define (file->string fname)
    (port->string (open-input-file fname)))

  ;; Path Path -> Boolean
  ;; Compare the printed output of interpreting exp-p to the content of out-p
  (define (check-interp exp-p out-p)
    (let ([actual  (with-output-to-string (λ () (interp-file exp-p)))]
          [desired (file->string out-p)])
      (check-equal? actual desired)
      (string=? actual desired)))

  ;; Test the interpreter on all of the sample files.
  (define (file-tests)
    (define (replace-exp-with-out p) (path-replace-extension p #".out"))

    (let ([actuals (map replace-exp-with-out samples)])
      (map (λ (exp-p out-p)
             (cons (find-relative-path (current-directory) exp-p)
                   (with-handlers ([exn:fail? (λ (e) "fail")])
                     (check-interp exp-p out-p))))
           samples actuals)))
  (file-tests)
  ) ; module