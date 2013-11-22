#lang racket

;---------------------------------------------------------------------
;|
;|    Master.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Abstraction for the master computer
;|
;---------------------------------------------------------------------


(require "content-provider.rkt"
         "content-storer.rkt"
         "steward.rkt"
         "macros.rkt"
         "database-manager.rkt")
(provide master%)
(define master%
  (class object%
    (super-new)
    
    (define*
      [stewards~ '()]
      ;procedure init initialises these objects to prevent 2 database connections
      [content-provider~ 'uninitialised] 
      [content-storer~ 'uninitialised]
      )
    
    ;loads everything in place 
    (define/public (init)
      ;make a database manager object
      (let ([db-manager (new database-manager%)])
        ;initialise both content provider and content storer
        (set! content-provider~ (new content-provider% [database-manager db-manager]))
        (set! content-storer~ (new content-storer% 
                                   [content-provider content-provider~]
                                   [database-manager db-manager]))
        (set! stewards~ (send content-provider~ get-stewards this)) 
        )
      )
                                   
    ;gets data out of the database
    (define/public (get-stored-data which)
      (send content-provider~ get-stored-data which)
      )
    
    ;sends the message directely to the device 
    (define/public (get-direct-data device-id message)
      (let ([steward (get-steward-for-device device-id)])
        (send steward get-data-from-device device-id message)))
    
    ;Getter for the steward list
    (define/public (get-stewards)
      stewards~)
    
    ;Adds a steward
    (define/public (add-device name place serial-port communication-address)
      'dummy)
    
    ;Returns a list with all the rooms in the system
    (define/public (get-all-rooms)
      (send content-provider~ get-rooms))

    
    ;returns the steward which has the device 
    ;this adds an extra check whith correct error handling
    (define/private (get-steward-for-device device-id)
      (let ([filtered (filter (lambda (steward)
                                (send steward has-device? device-id)))])
        (if (empty? filtered)
            (error "No such device on any of the masters' stewards, device id:" device-id)
            (car (filtered)))))
    
    )
  )
