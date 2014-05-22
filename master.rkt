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
         "device.rkt"
         "macros.rkt"
         "database-manager.rkt"
         "parser.rkt"
         "steward-wraper.rkt"
         "generic-data.rkt"
         "settings.rkt")
(provide master%)
(define master%
  (class object%
    (super-new)
    
    (define*
      [stewards~ '()]
      ;procedure init initialises these objects to prevent 2 database connections
      [content-provider~ 'uninitialised] 
      [content-storer~ 'uninitialised]
      ;threads responsible for collecting data and saving the state of the system
      [data-thread~ 'uninitialised]
      [save-thread~ 'uninitialised]
      
      [last-saved~ (current-milliseconds)]
      
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
        ;set the data collectin thread
        (set! data-thread~ (thread (lambda () 
                                     (define x (current-milliseconds))
                                     (let loop ()
                                       (if (> (- (current-milliseconds) x) 
                                              (get-field data-get-interval SETTINGS))
                                           (begin (collect-data)
                                                  (set! x (current-milliseconds))
                                                  (loop))
                                           (loop))))))
        ;set the save thread
        (set! save-thread~ (thread (lambda ()
                                     (let loop ()
                                       (if (> (- (current-milliseconds) last-saved~)
                                              (get-field save-interval SETTINGS))
                                           (begin (save)
                                                  (loop))
                                           (loop))))))
        
        )
      )
    
    
    ;sends the message directely to the device 
    (define/public (get-direct-data device-id message)
      (let ([steward (get-steward-for-device device-id)])
        (send steward get-data-from-device device-id message)))
    
    ;Getter for the steward list
    (define/public (get-stewards)
      stewards~)
    
    (define/public (add-steward ip port place)
      (define stew (new steward-wrapper%
                        [ip~ ip]
                        [port~ port]
                        [place~ place]
                        [master~ this]))
      (set! stewards~ (cons stew stewards~))
      (save)
      stew)
    
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
      (kill-thread data-thread~))
    
    
    ;saves 
    (define/private (save)
      ;do save
      (map (lambda (stew)
             (send content-storer~ store stew))
           stewards~)
      ;debugging
      (display "Saved")
      (newline)
      ;set the last save time
      (set! last-saved~ (current-milliseconds)))
    
    ;Procedure that returns facts about the system
    (define/public (get-facts fact)
      (cond ;dispatch
        [(eq? fact 'amount-data)
         (send content-provider~ get-amount-of-data)]
        [(eq? fact 'last-stored-data)
         (send content-provider~ last-data-stored-timestamp)]
        [(eq? fact 'amount-devices)
         (accumulate
          (lambda (steward numbr)
            (+ (length
                (send steward get-devices))
               numbr))
          0
          (get-stewards))]
        [(eq? fact 'amount-stewards)
         (length (get-stewards))]))
           
    
    ;Procedure that will be executed to collect data
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
        (newline)))
    
    ;procedure that dispatches the calls for data from a front end
    (define/public (get-data which . options)
      (cond
        [(eq? which 'all)
         (send content-provider~ get-all-data)]
        [else
         (error "Cannot get-data for which is: " which)]))
    
    
    )
  )
