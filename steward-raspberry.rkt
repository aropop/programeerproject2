#lang r5rs


;---------------------------------------------------------------------
;|
;|    Steward.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Code that runs on the raspberry (this is a simulated version
;|    that wil run on any racket device)
;|
;---------------------------------------------------------------------

(#%require racket/tcp)
(#%require (only racket/base let-values))
(#%require "device-r5rs.rkt" "xbee-simulation.rkt")
(#%provide steward%)


(define SUPPORTED-DEVICES
  (list 
   (vector 'plug "PID=ZBS-110" "Plug")
   (vector 'multiSensor "PID=ZBS-121" "MultiSensor")))

(define package-recieved-code 144)
(define package-recieved-data-offset 12)
(define package-recieved-end-length 2)
(define package-transmit-info-code 139)
(define package-transmit-succes 0)
(define package-transmit-offset 4)


(define (steward% id port)
  (let 
      
      ;public
      ((devices~ '())
       (steward-id~ id)
       (place~ "none")
       ;private
       (xbee (xbee-init "/dev/ttyUSB0" 9600)))
    
    
    ;SIMULATED ONLY
    (define con (tcp-listen port))
    
    
    ;Help procedure to facilitate debuging
    (define (displayln mes)
      (display mes)
      (newline))
    
    ;Displays an error (slips defines error but it's just a display of the first argument
    (define (error . mes)
      (map
       display
       mes)
      (newline))
    
    ;Sleeps for seconds
    (define (sleep)
      (define time (+ 3e+3 (clock))) 
      (define (lp)
        (if (not (>= (clock) time))
            (lp)))
      (lp))
    
    ;Sleeps till there is a message
    (define (sleep2)
      (if (not (xbee-ready? xbee))
          (sleep2)))
    
    
    ;returns the device for the given id
    (define (get-device device-id)
      (define (filter lam list)
        (define (lp r rest)
          (cond
            ((null? rest) r)
            ((lam (car rest))
             (lp (cons (car rest) r) (cdr rest)))
            (else 
             (lp r (cdr rest)))))
        (lp '() list))
      (let ((filtered
             (filter (lambda (device)
                       (equal? device-id (device 'get-id)))
                     devices~)))
        (if (null? filtered)
            (error "No such device in this steward, device id:" device-id " steward id:" steward-id~)
            (car filtered))))
    
    ;returns #t if the device is in the list
    (define (has-device device-id)
      (define has-device-bool #f)
      (map (lambda (device)
             (if (equal? device-id (device 'get-id))
                 (set! has-device-bool #t)))
           devices~)
      has-device-bool)
    
    
    ;defines if this steward is already in the database
    (define (is-already-stored?)
      (> steward-id~ 0))
    
    
    ;this way its able to edit id when converting from local to stored steward
    (define (set-id! id)
      (if (> steward-id~ 0)
          (error "This object already has an id")
          (set! steward-id~ id)))
    
    ;Raspberry does not know wherer it is
    (define (set-place! new-place)
      (set! place~ new-place)
      place~)
    
    ;Converts a byte vector to a vector with hexadecimal strings
    (define (byte-vector->hex-vector byte-vector)
      (define l (bytevector-length byte-vector))
      (define ret (make-vector (bytevector-length byte-vector)))
      (define (lp idx)
        (define (to-hex d) ;http://en.wikipedia.org/wiki/Hexadecimal#Binary_conversion
          (define r (modulo d 16))
          (if (= 0 (- d r))
              (to-char r)
              (string-append (to-hex (/ (- d r) 16)) (to-char r))))
        (define (to-char n)
          (define chars (vector "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))
          (vector-ref chars n))
        
        (if (>= idx l)
            ret
            (begin
              (vector-set! ret idx (to-hex (bytevector-ref byte-vector idx)))
              (lp (+ idx 1)))))
      (lp 0))
    
    ;Converts to a string
    (define (bytevector->string byte-vector . fromTo)
      (define l (-
                 (bytevector-length byte-vector)
                 (cond ;String might be too big
                   ((null? fromTo) 0)
                   ((null? (cdr fromTo)) (car fromTo))
                   (else (+ (car fromTo) (- (bytevector-length byte-vector) (cadr fromTo)))))))
      (define (lp idx str-idx str)
        (if (or
             (>= str-idx l)
             (and 
              (= (length fromTo) 2)
              (>= idx (cadr fromTo))))
            str
            (begin
              (display str-idx)(display " ") (display idx) (display " ") (displayln  str)
              (if (= (bytevector-ref byte-vector idx) 10)
                  (string-set! str str-idx #\,)
                  (string-set! str str-idx (integer->char (bytevector-ref byte-vector idx))))
              (lp (+ idx 1) (+ str-idx 1) str))))
      (if (>= (length fromTo) 1)
          (lp (car fromTo) 0 (make-string l #\a))
          (lp 0 0 (make-string l #\a))))
    
    ;Slip has not yet implemented this procedure so we define it here
    (define (list->bytevector list)
      (define v (make-bytevector (+ (length list) 1)))
      (define i 0)
      (define (lp lst)
        (if (null? lst)
            v
            (begin
              (bytevector-set! v i (car lst))
              (set! i (+ i 1))
              (lp (cdr lst)))))
      (bytevector-set! v (length list) 10)
      (lp list))
    
    ;Slip has not yet implemented this procedure so we define it here
    (define (bytevector-equal? bv1 bv2)
      (define (eq-lp idx)
        (if (>= idx (bytevector-length bv1))
            #t
            (and (= (bytevector-ref bv1 idx) (bytevector-ref bv2 idx))
                 (eq-lp (+ idx 1)))))
      (and
       (= (bytevector-length bv1) (bytevector-length bv2))
       (eq-lp 0)))
    
    
    ;Slip has not yet implemented this procedure so we define it here
    (define (string->list str)
      (define l (string-length str))
      (define (lp i lst)
        (if (< i 0)
            lst
            (lp (- i 1) (cons (string-ref str i) lst))))
      (lp (- l 1) '()))
    
    ;Adds a device
    ;Device should be a list whith the arguments to make an raspberry device ADT
    (define (add-device device-list)
      (let ((device (apply device-list device-slip))) 
        (set! devices~ (cons device devices~))))
    
    ;Converts a message into a bytevector to send
    (define (string->bytevector string)
      (list->bytevector (map char->integer (string->list string)))) 
    
    (define (send-message-to-all-devices mes)
      (map
       (lambda (dev)
         (send-message-to-device (dev 'get-address) mes))
       devices~))
    
    
    ;Sends the actual data
    (define (send-bytes-to-device bytes adr)
      (define (frame-type frame)
        (bytevector-ref frame 0))
      (define (frame-address frame)
        (define frame-adr (make-bytevector 8))
        (define (lp idx)
          (if (> idx 8)
              frame-adr
              (begin
                (bytevector-set! frame-adr (- idx 1) (bytevector-ref frame idx))
                (lp (+ idx 1)))))
        (lp 1))
      (define (read-loop)
        (sleep2)
        (let ((current-frame  (xbee-read xbee)))
          (cond
            ;Frame is a recieved package
            ((and
              (= (frame-type current-frame) package-recieved-code)
              (bytevector-equal? (frame-address current-frame) adr))
             (display "Got frame, message: ") 
             (displayln (bytevector->string current-frame 
                                            package-recieved-data-offset 
                                            (- (bytevector-length current-frame) 
                                               package-recieved-end-length)))
             current-frame)
            ;Transmit info package
            ((= (frame-type current-frame) package-transmit-info-code)
             (if (= (bytevector-ref current-frame package-transmit-offset) package-transmit-succes)
                 (begin
                   (displayln "Transmitting succesfull, waiting for response packet")
                   (read-loop))
                 (begin
                   (displayln "Not correctly sent, retrying")
                   (send-bytes-to-device bytes adr)))) 
            (else ;Unknown message type
             (read-loop)))))
      (xbee-write xbee adr bytes)
      (xbee-tick xbee)
      (sleep)
      (read-loop))
    
    ;Sends a single message to a device
    (define (send-message-to-device device-id mes)
      (define dev (get-device device-id))
      (define message-bytes (string->bytevector mes))
      (let ((frame (send-bytes-to-device message-bytes (dev 'get-address))))
        (bytevector->string frame package-recieved-data-offset (- (bytevector-length frame) 
                                                                  package-recieved-end-length))))
    
    ;Creates a device object from an xbee-node
    (define (create-device xbee-node)
      (define (get-device-type idstring)
        (substring idstring 0 11))
      (device-slip
       (car xbee-node)
       (cadr xbee-node)
       place~
       (get-device-type (car xbee-node))))
    
    
    
    ;returns the list of device objects
    (define (get-device-list)
      (set! devices~ (map create-device (xbee-list)))
      (map (lambda (dev)
             (dev 'serialize))
           devices~))
    
    ;Reconnects when master goes down
    (define (reconnect)
      ;(let ((io (tcp-accept (tcp-listen port)))
      ;      (in (car io))
      ;       (out (cdr io)))
      (let-values (((in out) (tcp-accept con)))
        (displayln "Connected over TCP/IP")
        (loop in out xbee)))
    
    
    (define (dispatch mes . args)
      (set! args (car args))
      (cond 
        ((eq? mes 'has-device) (apply has-device args))
        ((eq? mes 'is-already-stored?) (is-already-stored?))
        ((eq? mes 'add-device) (apply add-device (car args)))
        ((eq? mes 'send-message-to-device) (apply send-message-to-device args))
        ((eq? mes 'send-message-to-all-devices) (apply send-message-to-all-devices args))
        ((eq? mes 'get-device-list) (get-device-list))
        ((eq? mes 'set-id!) (apply set-id! (car args)))
        ((eq? mes 'set-place) (apply set-place! args))
        (else (displayln mes) 
              '(Unknown Message))))
    
    
    
    (define (loop in out xbee)
      (let
          ((mes (read in))
           (devices (xbee-list)))
        (xbee-tick xbee)
        (sleep)
        (display "Device-list: ") (displayln devices)
        (set! devices~ (map create-device devices))
        (display "Got: ")(displayln mes)
        (if (eof-object? mes)
            (begin
              (displayln "Master went down, waiting for new")
              (reconnect))
            ;2 lets because we need to have the devices updated
            (let ((response (dispatch (car mes) (cdr mes))))
              (display "Response: ")(displayln response)                              
              (write response out)
              (newline out)
              (flush-output out)
              (loop in out xbee)))))
    
    ;listens to the steward wrapper for the messages
    ;(let ((io (tcp-accept port))
    ;      (in (car io))
    ;       (out (cdr io)))
    (let-values (((in out) (tcp-accept con)))
      (displayln "Connected over TCP/IP")
      
      (xbee-discover-nodes xbee)
      
      (loop in out xbee))))