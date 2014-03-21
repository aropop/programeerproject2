#lang r5rs

;---------------------------------------------------------------------
;|
;|    Steward.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Code that runs on the raspberry
;|
;---------------------------------------------------------------------

;(#%require "parser-r5rs.rkt")
(#%require racket/tcp)
(#%require (only racket/base let-values))
(#%provide steward%)

(define (steward% master devices id place)
  (let 
      
      ;public
      ((master~ master)
       (devices~ devices)
       (is-already-stored #f)
       (steward-id~ id)
       (place~ place))
    
    
    
    (define (get-field field device)
      'iets)
    
    (define (error . mes)
      (display mes))
    
    
    ;returns the device for the given id
    ;filter:  http://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Flist..rkt%29._filter%29%29
    (define (get-device device-id)
      (define (filter lam list)
        (let lp ((r '())
                 (rest list))
          (cond
            ((null? rest) r)
            ((lam (car rest))
             (lp (cons (car rest) r) (cdr rest)))
            (else 
             (lp r (cdr rest))))))
      (let ((filtered
             (filter (lambda (device)
                       (= device-id (get-field 'device-id~ device)))
                     devices~)))
        (if (null? filtered)
            (error "No such device in this steward, device id:" device-id " steward id:" steward-id~)
            (car filtered))))
    
    ;returns #t if the device is in the list
    (define (has-device device-id)
      (define has-device-bool #f)
      (map (lambda (device)
             (if (= device-id (get-field 'device-id~ device))
                 (set! has-device-bool #t)))
           devices~)
      has-device-bool)
    
    ;returns the list of device objects
    (define (get-device-list)
      devices~)
    
    ;gets the data from a device
    ;this garantees that there is always an equal amount of read and
    ;writes which is needed for the integrity of the ports
    (define (get-data-from-devices device-symbol message)
      (let* ((device (get-device device-symbol))
             (device-output-port (get-output-port device-symbol))
             (device-input-port (get-input-port device-symbol)))
        ;(parser (new parser%))
        ;(p-message (if (pair? message) ;test if the type is correct, if not unparse it
        ;               message
        ;               (unparse-generic-data message))))
        
        ;write the message to the output port
        (write message device-output-port)
        ;read the message
        (read device-input-port)))
    
    ;sends the same message to all devices
    ;returns a list of a list of parsed answers
    (define (message-all-devices message)
      (map
       (lambda (device)
         (get-data-from-devices
          (get-field 'device-id~ device)
          message))
       devices~))
    
    
    
    
    ;defines if this steward is already in the database
    (define (is-already-stored?)
      (> steward-id~ 0))
    
    ;storer cond support
    (define (get-type)
      'steward)
    
    ;this way its able to edit id when converting from local to stored steward
    (define (set-id! id)
      (if (> steward-id~ 0)
          (error "This object already has an id")
          (set! steward-id~ id)))
    
    
    ;hier komt in de 2de fase een methode om wireless te communiceren, voorlopig gewoon send message sturen
    (define (get-input-port device-id)
      ;(let ((device (get-device device-id)))
      ;  (send device get-input-port)))
      ;TODO: get connection shit here
      'iets)
    
    (define (get-output-port device-id)
      ;(let ((device (get-device device-id)))
      ;  (send device get-output-port)))
      'iets)
    
    ;Adds a device
    (define (add-device device)
      (set! devices~ (cons device devices~)))
    
    (define (dispatch mes . args)
      (cond ((eq? mes 'get-type) (get-type))
            ((eq? mes 'has-device) (apply has-device args))
            ((eq? mes 'is-already-stored?) (is-already-stored?))
            ((eq? mes 'add-device) (apply add-device args))
            ;((eq? mes 'get-device-status) (apply get-device-status args))
            ((eq? mes 'message-all-devices) (apply message-all-devices args))
            ((eq? mes 'get-data-from-devices) (apply get-data-from-devices args))
            ((eq? mes 'get-device-list) (get-device-list))
            ((eq? mes 'set-id!) (apply set-id! args))))
    
    
    (let-values (((in out) (tcp-accept (tcp-listen 12345))))
      (display "connected")
      (let loop ()
        (let 
            ((mes (read in)))
          (display mes)
          (write (dispatch mes) out)
          (newline out)
          (flush-output out)
          (loop))))   
    ))

(define steward (steward% 1 2 3 4))