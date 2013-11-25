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
         "device.rkt"
         "macros.rkt"
         "database-manager.rkt"
         "parser.rkt"
         "generic-data.rkt")
(provide master%)
(define master%
  (class object%
    (super-new)
    
    (define*
      [stewards~ '()]
      ;procedure init initialises these objects to prevent 2 database connections
      [content-provider~ 'uninitialised] 
      [content-storer~ 'uninitialised]
      [data-thread~ 'uninitialised]
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
        
        (set! data-thread~ (thread (lambda () 
                                     (define x (current-milliseconds))
                                     (let loop ()
                                       (if (> (- (current-milliseconds) x) 10000)
                                           (begin (collect-data)
                                                  (set! x (current-milliseconds))
                                                  (loop))
                                           (loop))))))
        
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
    (define/public (add-device type name place serial-port communication-address)
      (define steward 'none-found)
      (define create-device (lambda (type name serial-number com-adr) 
                              (cond [(eq? type 'switch) 
                                     (new switch%
                                          [place~ place]
                                          [communication-address~ com-adr]
                                          [name~ name]
                                          [serial-number~ serial-number])]
                                    [(eq? type 'thermometer)
                                     (new thermometer%
                                          [place~ place]
                                          [communication-address~ com-adr]
                                          [name~ name]
                                          [serial-number~ serial-number])]
                                    
                                    [else
                                     (error "Cannot create device from type " type)])))
      (map (lambda (s)
             (if (equal? (get-field place~ s) place)
                 (set! steward s)
                 'niets))
           (get-stewards))
      (if (eq? steward 'none-found)
          (error "No steward for supplied room")
          (send steward add-device (create-device (string->symbol type) name serial-port communication-address))))
    
    
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
    
    ;Has to be called upon shutting the system down
    (define/public (destruct)
      (save)
      (kill-thread data-thread~)
      )
    ;saves 
    (define/private (save)
      (map (lambda (stew)
             (send content-storer~ store stew))
           stewards~)
      )
    
    (define/private (collect-data)
      (define pars (new parser%))
      (let ([message (new generic-data% [name 'GET] [value 'ALL])])
        ;iterate
        (map
         (lambda (steward)
           (map
            (lambda (list-of-responses-of-one-device)
              (map
               (lambda (parsed-message)
                 (send content-storer~ store parsed-message))
               list-of-responses-of-one-device))
            (send steward message-all-devices message)))
         stewards~)
        ;debugging
        (display "Collected data on ")
        (display (current-milliseconds))
        (newline)
        )
      )
    
    
    )
  )
