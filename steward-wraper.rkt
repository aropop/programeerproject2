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
         "database-saveable.rkt"
         racket/tcp)

(provide steward-wrapper% steward-wrapper$)

(define steward-wrapper%
  (class* object% (database-saveable<%>)
    (super-new)
    
    (init-field
     (ip~ "127.0.0.1")
     (master~ 'none)
     (devices~ '())
     (place~ "nowhere")
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
      (with-handlers ((exn:fail:network? (lambda (exn) 
                                          (error "Could not connect to steward at " 
                                                 ip~ ":" port~ ", probaly offline")
                                          #f)))
        (let-values (((i o) (tcp-connect ip~ port~)))
          (set! input-port~ i)
          (set! output-port~ o))))
    
    
    ;Public interface
    ;Returns only info about these devices
    (define/public (get-devices)
      (set! devices~ (send-to-pi '(get-device-list)))
      devices~)
    
    (define/public (send-message-to-device device-id mes)
      (send-to-pi `(send-message-to-device ,device-id ,mes)))
    
    (define/public (get-device-type device-id)
      (send-to-pi `(get-device-type ,device-id)))
    
    (define/public (get-device-status device-id)
      (send-to-pi `(send-message-to-device ,device-id "GET")))
    
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
    
    (define/public (store-sql)
      (cond [(is-already-stored?) ;device is already in the database so we need to update
             (let ([query (string-append "UPDATE Steward SET "
                                         "room_name='" place~ "', "
                                         "ip='" ip~"', "
                                         "port=" (number->string port~) " "
                                         "WHERE steward_id=" (number->string steward-id~))])
               ;store the query
               query)]
            [else ;steward is not stored already
             ;build the query to store the steward
             (let ([query (string-append
                           "INSERT INTO Steward (room_name, ip, port) VALUES ('"
                           place~ "', '" ip~"', " (number->string port~) ")")])
               ;return query
               query)]))
    
    ;Makes a steward-wrapper object
    (define/public (create-lambda)
      (lambda (id place ip port devices master) 
        (new steward-wrapper% ;make a wrapper will connect when needed
             [place~ place]
             [devices~ devices]
             [ip~ ip]
             [port~ port]
             [steward-id~ id])))
    
    (define/public (get-sql)
      "SELECT steward_id, room_name, ip, port FROM Steward")))

(define steward-wrapper$ (new steward-wrapper%))