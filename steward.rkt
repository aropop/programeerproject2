#lang racket

(require "macros.rkt")
(provide steward%)

(define steward%
  (class object%
    
    (init master
          devices
          (is-already-stored #f))
    
    
    (init-field 
     [name~ "Unnamed device"]
     [serial-number~ 0]
     [communication-adress~ "No address"]
     [steward-id~ -1]
     [place~ 'no-where]
     )
    
    
    (define*
      [devices~ devices]
      [master~ master]
      )
    
    
    (super-new)
    
    (define/private (get-device device-id)
      (let devices-iter
        ([index 0])
        (let ([current-device (vector-ref index devices~)])
          (cond [(eq? device-id (get-field id current-device))
                 current-device]
                [else (devices-iter (+ index 1))]))))
    
    (define/public (get-device-list)
      devices~)
    
    (define/public (get-data-from-devices device-symbol type-of-data)
      (let* ([device (get-device device-symbol)]
             [device-output-port (send device get-output-port)]
             [device-input-port (send device get-input-port)])
        (write `(GET ,type-of-data) device-output-port)
        (read device-input-port)))
    
    ;defines if this steward is already in the database
    (define/public (is-already-stored?)
      (> steward-id~ 0))
    
    ;storer cond support
    (define/public (get-type)
      'steward)
    
    ;this way its able to edit id when converting from local to stored steward
    (define/public (set-id! id)
      (if (> steward-id~ 0)
          (error "This object already has an id" this)
          (set! steward-id~ id)))
    
    
    ;hier komt in de 2de fase een methode om wireless te communiceren, voorlopig gewoon send message sturen
    (define/public (get-input-port device-id)
      (let ([device (get-device device-id)])
        (send device get-input-port)
        )
      )
    
    (define/public (get-ouput-port device-id)
      (let ([device (get-device device-id)])
        (send device get-output-port)
        )
      )
    
    
    )      
  )