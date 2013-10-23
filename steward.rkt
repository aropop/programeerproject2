#lang racket

(require "macros.rkt")

(define steward%
  (class object%
    
    (init master
          devices
          place)
    
    (define*
      [place-in-house~ place]
      [devices~ devices]
      [master~ master]
      )
    
    (super-new)
    
    (define/private (get-device device-symbol)
      (let devices-iter
        ([index 0])
        (let ([current-device (vector-ref index devices~)])
          (cond [(eq? device-symbol (send current-device get-device-symbol))
                 current-device]
                [else (devices-iter (+ index 1))]))))
    
    (define/public (get-data-from-devices device-symbol type-of-data)
      (let* ([device (get-device device-symbol)]
             [device-output-port (send device get-output-port)]
             [device-input-port (send device get-input-port)])
        (write `(GET ,type-of-data) device-output-port)
        (read device-input-port)))
    
    
  
  )      
)