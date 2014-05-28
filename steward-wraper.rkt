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
         "settings.rkt"
         "parser.rkt"
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
      [status~ 'offline]
      [communication-lock~ #f]
      [searched-for-devices-once #f])
    
    ;private functions
    (define/private (send-to-pi mes)
      (cond
        [(not (online?))
         (connect-to-pi)
         (if (not (online?))
             '("OFFLINE")
             (send-to-pi mes))]
        [(null? input-port~)
         (connect-to-pi)
         (send-to-pi mes)]
        ;Make sure an other thread isn't sending, scheduling will switch sometime so no deadlocks
        [communication-lock~
         (send-to-pi mes)]
        [else
         (set! communication-lock~ #t)
         (write mes output-port~)
         (newline output-port~)
         (flush-output output-port~)
         (let ((ret (read input-port~)))
           (set! communication-lock~ #f)
           (when (eof-object? ret)
             (displayln "Steward went offline")
             (set! status~ 'offline)
             (set! ret '()))
           ret)]))
    
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
    
    (define/private (get-device device-id)
      (let 
          ((type (filter-map (lambda (device)
                               (and
                                (equal? device-id (get-field id~ device))
                                device))
                             devices~)))
        (if (null? type)
            (error "No device for id (get-device): " device-id)
            (car type))))
    
    
    ;Public interface
    
    ;Tells if the actual pi is online
    (define/public (online?)
      (eq? 'online status~))
    
    ;Returns only info about these devices
    (define/public (get-devices)
      devices~)
    
    ;For front end displaying
    (define/public (get-devices-count)
      (cond
        [searched-for-devices-once
         (length devices~)]
        [(online?)
         "Searching.."]
        [else 
         "OFFLINE"]))
    
    ;Will ask the new devices from the 
    (define/public (get-devices-force-discovery)
      (define (device-vector->device-obj vect)
        (if (has-device? (vector-ref vect 1))
              (let ((d (get-device (vector-ref vect 1))))
                (when (not (get-field is-found?~ d))
                  (set-field! is-found?~ d #t)
                  (set-field! last-status~ d "Found, loading status..."))
                d); Get existing device
            (new device-wrapper%
                 [place~ (vector-ref vect 3)]
                 [com-adr~ (vector-ref vect 2)]
                 [id~ (vector-ref vect 1)]
                 [type~ (vector-ref vect 4)]
                 [steward-wrapper~ this])))
      (when (online?)
        (set! devices~ (map device-vector->device-obj (send-to-pi '(get-device-list))))
        (set! searched-for-devices-once #t))      
      devices~)
    
    (define/public (send-message-to-device device-id mes)
      (send-to-pi `(send-message-to-device ,device-id ,mes)))
    
    (define/public (get-device-type device-id)
      (let 
          ((type (filter-map (lambda (device)
                               (and
                                (equal? device-id (get-field id~ device))
                                (get-field type~ device)))
                             devices~)))
        (if (null? type)
            (error "No device for id (get-device-type): " device-id)
            (car type))))
    
    (define/public (get-device-status device-id)
      (let ((type (filter-map (lambda (device)
                                (and
                                 (equal? device-id (get-field id~ device))
                                 (get-field last-status~ device)))
                              devices~)))
        (if (null? type)
            (error "No device for id (get-device-status): " device-id)
            (car type))))
    
    (define/public (get-devices-status-force-message device-id)
      (define dev (get-device device-id))
      (set-field! last-status~ dev (send parser$
                                         parse-message
                                         (send-to-pi `(send-message-to-device ,device-id "GET"))
                                         device-id)))
    
    (define/public (message-all-devices mes)
      (send-to-pi `(send-message-to-all-devices ,mes)))
    
    
    (define/public (is-already-stored?)
      (> steward-id~ 0))
    
    
    (define/public (has-device? device-id)
      (let  ((lst (filter-map (lambda (device)
                                (and
                                 (equal? device-id (get-field id~ device))
                                 device))
                              devices~)))
        (not (null? lst))))
    
    (define/public (set-id! id)
      (cond [(not (is-already-stored?))
             (set! steward-id~ id)
             (send-to-pi `(set-id! ,id))]))
    
    ;Returns a cons of a lambda to be executed after this
    (define/public (store-sql)
      (cond [(is-already-stored?) ;device is already in the database so we need to update
             (let ([query (string-append "UPDATE Steward SET "
                                         "room_name='" place~ "', "
                                         "ip='" ip~"', "
                                         "port=" (number->string port~) " "
                                         "WHERE steward_id=" (number->string steward-id~))]
                   [update-lambda (lambda (new-id content-storer)
                                    (map
                                     (lambda (device)
                                       (send content-storer store device))
                                     devices~))])
               ;store the query
               (cons query update-lambda))]
            [else ;steward is not stored already
             ;build the query to store the steward
             (let ([query (string-append
                           "INSERT INTO Steward (room_name, ip, port) VALUES ('"
                           place~ "', '" ip~"', " (number->string port~) ")")]
                   [update-lambda (lambda (new-id content-storer)
                                    (set! steward-id~ new-id)
                                    (map
                                     (lambda (device)
                                       (send content-storer store device))
                                     devices~))])
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
      ;This will start a thread setting the value, it will make a connection too
      (thread (lambda ()
                (send-to-pi `(set-place ,place~))))
      ;Thread will ask for the devices to the real steward
      (thread (lambda () 
                (define x (current-milliseconds))
                (let loop ()
                  (if (> (- (current-milliseconds) x) 
                         (get-field devices-get-interval SETTINGS))
                      (begin (get-devices-force-discovery)
                             (map (lambda (dv) (get-devices-status-force-message (get-field id~ dv)))
                                  devices~)
                             (set! x (current-milliseconds))
                             (loop))
                      (loop))))))))

(define steward-wrapper$ (new steward-wrapper% [ip~ "static"]))