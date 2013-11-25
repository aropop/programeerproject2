#lang racket

;---------------------------------------------------------------------
;|
;|    Content-provider.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Provides content from the database
;|
;---------------------------------------------------------------------

(require "macros.rkt"
         "parser.rkt")
(provide steward%)

(define steward%
  (class object%
    
    (init master
          devices
          (is-already-stored #f))
    
    
    (init-field 
     [steward-id~ -1]
     [place~ 'no-where]
     )
    
    
    (define*
      [devices~ devices]
      [master~ master]
      )
    
    
    (super-new)
    
    
    ;returns the device for the given id
    ;filter:  http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
    (define/private (get-device device-id)
      (let ([filtered
             (filter (lambda (device)
                       (= device-id (get-field device-id~ device)))
                     devices~)])
        (if (empty? filtered)
            (error "No such device in this steward, device id:" device-id " steward id:" steward-id~)
            (car filtered))))
    
    ;returns #t if the device is in the list
    ;ormap : http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._ormap%29%29
    (define/public (has-device device-id)
      (ormap (lambda (device)
               (= device-id (get-field device-id~ device)))
             devices~))
    
    ;returns the list of device objects
    (define/public (get-device-list)
      devices~)
    
    ;gets the data from a device
    ;this garantees that there is always an equal amount of read and
    ;writes which is needed for the integrity of the ports
    (define/public (get-data-from-devices device-symbol message)
      (let* ([device (get-device device-symbol)]
             [device-output-port (send device get-output-port)]
             [device-input-port (send device get-input-port)]
             [parser (new parser%)]
             [p-message (if (pair? message) ;test if the type is correct, if not unparse it
                            message
                            (send parser unparse-generic-data message))])
        
        ;write the message to the output port
        (write p-message device-output-port)
        ;read the message
        (send parser parse-message (read device-input-port) device-symbol)))
    
    ;sends the same message to all devices
    ;returns a list of a list of parsed answers
    (define/public (message-all-devices message)
      (map
       (lambda (device)
         (get-data-from-devices
          (get-field device-id~ device)
          message))
       devices~))
    
    ;returns the status
    (define/public (get-device-status device-id)
      (car ;Take the first element of the list because the status should only be 1 generic-data object
       (get-data-from-devices
        device-id 
        (send (get-device device-id) get-status-message))))
    
    
    
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
    
    ;Adds a device
    (define/public (add-device device)
      (set! devices~ (cons device devices~)))
    
    
    )      
  )