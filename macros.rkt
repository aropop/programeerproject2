#lang racket

;---------------------------------------------------------------------
;|
;|    macros.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Provides some macros
;|
;---------------------------------------------------------------------


(provide define* accumulate)

;Macro that makes it possible to define multiple private fields
(define-syntax define*
  (syntax-rules ()
    [(define* [a b]) (define a b)]
    [(define* (a b) (c d) ...) (begin
                       (define a b)
                       (define* (c d) ...))]))

;Accumulate procedure
;proc has to take 2 arguments first is an element of the list second is the result
(define (accumulate proc zero lst)
  (let lp
    ([res zero]
     [remaining lst])
    (if (empty? remaining)
        res
        (lp (proc (car remaining) res)
            (cdr remaining)))))

