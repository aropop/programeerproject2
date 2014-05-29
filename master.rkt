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
      [actions~ '()]
      ;procedure init initialises these objects to prevent 2 database connections
      [content-provider~ 'uninitialised] 
      [content-storer~ 'uninitialised]
      ;threads responsible for collecting data and saving the state of the system
      [data-action-thread~ 'uninitialised]
      [save-thread~ 'uninitialised]
      
      [last-saved~ (current-milliseconds)])
    
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
        (set! actions~ (send content-provider~ get-actions))
        ;set the data collectin thread
        (set! data-action-thread~ (thread (lambda () 
                                     (define x (current-milliseconds))
                                     (let loop ()
                                       (if (> (- (current-milliseconds) x) 
                                              (get-field data-get-interval SETTINGS))
                                           (begin (collect-data)
                                                  (check-actions)
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
                                           (loop))))))))
    
    
    ;sends the message directely to the device 
    (define/public (get-direct-data device-id message)
      (let ([steward (get-steward-for-device device-id)])
        (send steward get-data-from-device device-id message)))
    
    ;Getter for the steward list
    (define/public (get-stewards)
      stewards~)
    
    ;Adds a new steward
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
    
    ;Get the steward for the id
    (define/public (get-steward steward-id)
      (let ((type (filter-map (lambda (steward)
                                (and
                                 (equal? steward-id (get-field steward-id~ steward))
                                 steward))
                              stewards~)))
        (if (null? type)
            (error "No steward for id (get-steward): " steward-id
                   (map (lambda (s) (get-field steward-id~ s)) stewards~))
            (car type))))
    
    ;Get actions
    (define/public (get-actions)
      actions~)
    
    ;Add a new action
    (define/public (add-action ac)
      (send content-storer~ store ac)
      (set! actions~ (cons ac actions~)))
    
    ;Deletes an action
    (define/public (delete-action id)
      (define action-to-delete '())
      (set! actions~ (filter
                      (lambda (act)
                        (if (= (get-field action-id~ act) id)
                            (begin
                              (set! action-to-delete act)
                              #f)
                            #t))
                      actions~))
      (when (null? action-to-delete)
        (error "Cannot delete action id:" id))
      (send content-storer~ unstore action-to-delete))
    
    
    ;returns the steward which has the device 
    ;this adds an extra check whith correct error handling
    (define/public (get-steward-for-device device-id)
      (let ([filtered (filter-map (lambda (steward)
                                    (and 
                                     (send steward has-device? device-id)
                                     steward))
                                  stewards~)])
        (if (null? filtered)
            (error "No such device on any of the masters' stewards, device id:" device-id
                   (map (lambda (s) (send s get-devices)) stewards~))
            (car filtered))))
    
    ;Has to be called upon shutting the system down
    (define/public (destruct)
      (save)
      (kill-thread data-action-thread~)
      (kill-thread save-thread~))
    
    
    ;saves 
    (define/private (save)
      ;do save
      (map (lambda (stew)
             ;store devices first
             (map (lambda (dev)
                    (send content-storer~ store dev))
                  (send stew get-devices))
             ;Then store steward itself
             (send content-storer~ store stew))
           stewards~)
      ;debugging
      (displayln "Saved")
      ;set the last save time
      (set! last-saved~ (current-milliseconds)))
    
    
     ;Procedure that will be executed to collect data
    (define/private (collect-data)
      ;iterate
      (map
       (lambda (steward)
         (map
          (lambda (device)
            (map
             (lambda (parsed-message)
               (send content-storer~ store parsed-message))
             ;Status may return a string, this is the loading status for the front end
             (let ((status-list (send steward get-device-status (get-field id~ device)))) 
               (if (string? status-list)
                   '()
                   status-list))))
          (send steward get-devices)))
       stewards~)
      ;debugging
      (display "Collected data on ")
      (displayln (current-milliseconds)))
    
    ;Procedure will check all the actions, executed right after data-collects thread
    ;so we have "fresh" information on the stewards
    (define/private (check-actions)
      (map
       (lambda (action)
         (let ([steward-d (get-steward-for-device
                         (get-field destination-device-id~ action))]
               [steward-s (get-steward-for-device
                         (get-field source-device-id~ action))])
         (send action execute steward-d steward-s)))
       actions~)
      (display "Checked all actions")
      (newline))
    
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
            (+ (if (send steward online?)
                   (length
                    (send steward get-devices))
                   0)
               numbr))
          0
          (get-stewards))]
        [(eq? fact 'amount-stewards)
         (length (get-stewards))]))
    
    ;procedure that dispatches the calls for data from a front end
    (define/public (get-data which . options)
      (cond
        [(eq? which 'all)
         (send content-provider~ get-all-data)]
        [else
         (error "Cannot get-data for which is: " which)]))))
