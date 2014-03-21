#lang racket

;---------------------------------------------------------------------
;|
;|    steward-wraper.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Abstraction over the steward, so that we can work whith real
;|    Racket classes instead of having to make an exception for the
;|    steward only
;|
;---------------------------------------------------------------------

(require "macros.rkt"
         racket/tcp)

(provide steward-wrapper%)

(define steward-wrapper%
  (class object%
    (super-new)
    
    (init-field
     (ip~ "127.0.0.1")
     (master~ 'nothing)
     (devices~ '())
     (port~ 0))
    
    (define*
      [input-port~ '()]
      [output-port~ '()]
      [steward-id~ -1]
      )
    
    ;private functions
    (define/private (send-to-pi mes)
      (if (null? input-port~)
          (begin 
            (connect-to-pi)
            (send-to-pi mes))
          (begin
            (write mes output-port~)
            (newline output-port~)
            (flush-output output-port~)
            (read input-port~))))
    
    (define/private (connect-to-pi)
      ;with-handlers is try in scheme
      (with-handlers ((exn:fail:network (lambda (exn) 
                                         (error "Could not connect to steward at " 
                                                ip~ ":" port~ ", probaly offline")
                                          #f)))
                    (let-values (((i o) (tcp-connect ip~ port~)))
                      (set! input-port~ i)
                      (set! output-port~ o))))
    
    
    ;Public interface
    ;Returns only info about these devices
    (define/public (get-devices)
      (send-to-pi '(get-devices)))
    
    ;Make some functions local for performance
    (define/public (get-type)
      'steward)
    
    (define/public (is-already-stored?)
      (> steward-id~ 0))
    
    
    (define/public (has-device device-id)
      (send-to-pi `(has-device ,device-id)))
    
    (define/public (set-id! id)
      (cond [(not (is-already-stored?))
             (set! steward-id~ id)
             (send-to-pi `(set-id! ,id))]))
    
    
    
    )
  )