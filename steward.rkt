#lang racket

(require "macros.rkt")
(provide steward%)

(define steward%
  (class object%
    
    (init master
          devices
          place
          (steward-id -1) ;it is not stored in the db yet
          (is-already-stored #f))
    
    (define*
      [place-in-house~ place]
      [devices~ devices]
      [master~ master]
      [steward-id~ steward-id]
      [is-already-stored~ is-already-stored]
      )
    
    (field [is-in-db~ is-already-stored])
    
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
    
    ;storer cond support
    (define/public (get-type)
      'steward)
    
    ;this way its able to edit id when converting from local to stored steward
    (define/public (set-id! id)
      (if (> steward-id~ 0)
          (error "This object already has an id" this)
          (set! steward-id~ id)))
    
  
  )      
)