#lang racket

(provide define*)

(define-syntax define*
  (syntax-rules ()
    [(define* [a b]) (define a b)]
    [(define* (a b) (c d) ...) (begin
                       (define a b)
                       (define* (c d) ...))]))

