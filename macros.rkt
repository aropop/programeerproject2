#lang racket

;---------------------------------------------------------------------
;|
;|    Content-provider.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Provides content from the database
;|
;---------------------------------------------------------------------


(provide define*)

(define-syntax define*
  (syntax-rules ()
    [(define* [a b]) (define a b)]
    [(define* (a b) (c d) ...) (begin
                       (define a b)
                       (define* (c d) ...))]))

