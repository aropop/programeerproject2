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
         "device.rkt"
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
     (port~ 0)
     (steward-id~ -1))
    
    (define*
      [input-port~ '()]
      [output-port~ '()]
      [status~ 'offline])
    
    ;private functions
    (define/private (send-to-pi mes)
      (cond
        [(not (online?))
         (connect-to-pi)
         (if (not (online?))
             '()
             (send-to-pi mes))]
        [(null? input-port~)
         
         (connect-to-pi)
         (send-to-pi mes)]
        [else
         (write mes output-port~)
         (newline output-port~)
         (flush-output output-port~)
         (read input-port~)]))
    
    (define/private (connect-to-pi)
      ;with-handlers is try in scheme
      (with-handlers ((exn:fail:network? (lambda (exn) 
                                           (display "Steward @")
                                           (display ip~)
                                           (display ":")
                                           (display port~)
                                           (displayln " is offline")
                                           (set! status~ 'offline))))
        (let-values (((i o) (tcp-connect ip~ port~)))
          (set! input-port~ i)
          (set! output-port~ o)
          (set! status~ 'online))))
    
    (define/public (online?)
      (eq? 'online status~))
    
    ;Public interface
    ;Returns only info about these devices
    (define/public (get-devices)
      (define (device-vector->device-obj vect)
        (new device-wrapper%
             [place~ (vector-ref vect 3)]
             [com-adr~ (vector-ref vect 2)]
             [id~ (vector-ref vect 1)]
             [type~ (vector-ref vect 4)]
             [steward-wrapper~ this]))
      (set! devices~ (map device-vector->device-obj (send-to-pi '(get-device-list))))
      devices~)
    
    (define/public (send-message-to-device device-id mes)
      (send-to-pi `(send-message-to-device ,device-id ,mes)))
    
    (define/public (get-device-type device-id)
      (send-to-pi `(get-device-type ,device-id)))
    
    (define/public (get-device-status device-id) ;TODO:parse data
      (send-to-pi `(send-message-to-device ,device-id "GET")))
    
    (define/public (message-all-devices)
      (map
       (lambda (device)
         (get-device-status (get-field id~ device)))
       devices~))
    
    
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
                           place~ "', '" ip~"', " (number->string port~) ")")]
                   [update-lambda (lambda (new-id)
                                    (set! steward-id~ new-id))])
               ;return query
               (cons query update-lambda))]))
    
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
      "SELECT steward_id, room_name, ip, port FROM Steward")
    
    ;initialisation
    (when (not (equal? ip~ "static"))
      (send-to-pi `(set-place ,place~)))))

(define steward-wrapper$ (new steward-wrapper% [ip~ "static"]))