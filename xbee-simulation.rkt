#lang r5rs


;---------------------------------------------------------------------
;|
;|    Steward.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Simulates the behaviour that originialy is built into slip
;|    (or comes with the xbee library)
;|
;---------------------------------------------------------------------

(#%provide xbee-initialise xbee-init
           xbee-write
           xbee-tick 
           xbee-list-nodes xbee-list
           xbee-discover-nodes xbee-discover 
           xbee-read-frame xbee-read
           print-frame
           list->bytevector
           make-bytevector
           bytevector-length
           bytevector-ref
           bytevector-set!
           bytevector?)
(#%require "device-r5rs.rkt")
(#%require (only racket/base random))

(define SIM-DEVICES 
  (list
   (device-slip "ABCDEFGH" (vector 0 0 0 0 0 0 0 0) "Kitchen" 'multiSensor)
   (device-slip "IJKLMNOP" (vector 0 0 0 0 0 0 0 1) "KidsRoom"  'plug)))

(define DEVICE2-STATE 'on)

(define MESSAGES
  (list
   ;vector message-string message-bytes answer-bytes address lambda-executed-when-called
   ;Device 1 multisensor
   (vector "DEV PID" 
           #(68 69 86 32 80 73 68) 
           (vector 
            ;message type
            144 
            ;64 bit address
            0 0 0 0 0 0 0 0
            ;16 bit address
            0 0
            ;Recieve options 1
            1
            ;Data packet
            80 73 68 61 90 66 83 45 49 50 49 10 ; PID=ZBS-121
            ;line feeds 
            10 10)
           (vector 0 0 0 0 0 0 0 0)
           (lambda (message-bytevector) #t))
   (vector "GET" 
           #(71 69 84)
           (vector 
            ;message type
            144 
            ;64 bit address
            0 0 0 0 0 0 0 1
            ;16 bit address
            0 0
            ;Recieve options 1
            1
            ;Data packet
            84 69 77 61 50 55 46 53 176 67 10 ;TEM=27.5°C
            66 82 73 61 51 50 108 120 10 ;BRI=32lx
            72 85 77 61 54 53 37 10 ;HUM=65% 
            80 82 69 83 61 49 48 49 53 46 52 54 104 80 97 10 ;PRES=1015.46hPa 
            66 65 84 61 79 75 10 ;BAT=OK
            ;line feeds 
            10 10)
           (vector 0 0 0 0 0 0 0 0)
           (lambda (message-bytevector) 
             (bytevector-set! message-bytevector 21 (random 10))
             (bytevector-set! message-bytevector 21 (+ 1 (random 2)))))
   ;Device 2 (ABCDEFGH) Plug
   (vector "DEV PID" 
           #(68 69 86 32 80 73 68) 
           (vector 
            ;message type
            144 
            ;64 bit address
            0 0 0 0 0 0 0 1
            ;16 bit address
            0 0
            ;Recieve options 1
            1
            ;Data packet
            80 73 68 61 90 66 83 45 49 49 48 10 ; PID=ZBS-110
            ;line feeds 
            10 10)
           (vector 0 0 0 0 0 0 0 1)
           (lambda (message-bytevector) #t))
   (vector "GET" 
           #(71 69 84)
           (vector 
            ;message type
            144 
            ;64 bit address
            0 0 0 0 0 0 0 1
            ;16 bit address
            0 0
            ;Recieve options 1
            1
            ;Data packet
            80 79 87 61 79 78 10 ;POW=ON
            70 82 69 81 61 52 57 46 56 49 50 53 72 122 10 ; 
            86 82 77 83 61 50 50 55 86 10 
            73 82 77 83 61 49 57 56 56 109 65 10
            76 79 65 68 61 52 52 51 87 10 
            87 79 82 75 61 48 46 48 52 54 107 87 104
            ;line feeds 
            10 10)
           (vector 0 0 0 0 0 0 0 1)
           (lambda (message-bytevector) #t))
   (vector "GET POW" 
           #(71 69 84 32 80 79 87)
           (vector 
            ;message type
            144 
            ;64 bit address
            0 0 0 0 0 0 0 1
            ;16 bit address
            0 0
            ;Recieve options 1
            1
            ;Data packet
            80 79 87 61 79 78 10 ;POW=ON
            ;line feeds 
            10 10)
           (vector 0 0 0 0 0 0 0 1)
           (lambda (message-bytevector) #t))
   (vector "SET POW" 
           #(71 69 84 32 80 79 87)
           (vector 
            ;message type
            144 
            ;64 bit address
            0 0 0 0 0 0 0 1
            ;16 bit address
            0 0
            ;Recieve options 1
            1
            ;Data packet
            80 79 87 61 79 78 10 ;POW=ON
            ;line feeds 
            10 10)
           (vector 0 0 0 0 0 0 0 1)
           (lambda (message-bytevector) 
             (if (eq? DEVICE2-STATE 'on)
                 (begin
                   (set! DEVICE2-STATE 'off)
                   (bytevector-set! message-bytevector 16 79)
                   (bytevector-set! message-bytevector 16 70))
                 (begin
                   (set! DEVICE2-STATE 'on)
                   (bytevector-set! message-bytevector 16 79)
                   (bytevector-set! message-bytevector 16 78)))))))

(define UNKNOWN-MESSAGE 
  (vector 
   ;message type
   144 
   ;64 bit address
   0 0 0 0 0 0 0 0
   ;16 bit address
   0 0
   ;Recieve options 1
   1
   ;Data packet
   85 78 75 78 79 87 78 32 77 69 83 83 65 71 69 10
   ;line feeds 
   10 10)) 

(define TRANSMIT-MESSAGE 
  (vector 
   ;message type
   139 
   ;16 bit address
   0 0
   ;retry count
   0
   ;delivery status
   0
   ;discovery status 
   3)) 


(define ANSWERS '())

(define (generate-answer type mes adr)
  (define (insert-address ans)
    (vector-set! ans 1 (vector-ref adr 0))
    (vector-set! ans 2 (vector-ref adr 1))
    (vector-set! ans 3 (vector-ref adr 2))
    (vector-set! ans 4 (vector-ref adr 3))
    (vector-set! ans 5 (vector-ref adr 4))
    (vector-set! ans 6 (vector-ref adr 5))
    (vector-set! ans 7 (vector-ref adr 6))
    (vector-set! ans 8 (vector-ref adr 7))
    ans)
  (define (get-ans lst)
    (cond
      ((null? lst)
       (insert-address UNKNOWN-MESSAGE))
      ((and (equal? (vector-ref (car lst) 1) mes)
            (equal? (vector-ref (car lst) 3) adr))
       ((vector-ref (car lst) 4) (vector-ref (car lst) 2)) ;Execute lambda
       (insert-address (vector-ref (car lst) 2)))  
      (else
       (get-ans (cdr lst)))))
  (set! ANSWERS (cons 
                 TRANSMIT-MESSAGE
                 (cons 
                 (get-ans MESSAGES) ANSWERS))))

(define (xbee-initialise ign ore)
  SIM-DEVICES)
(define xbee-init xbee-initialise)

(define (xbee-write xb adr mes)
  (define (get-right lst)
    (if (null? lst)
        (display "could not write")
        (if (equal? ((car lst) 'get-address) adr)
            (generate-answer ((car lst) 'get-type) mes adr)
            (get-right (cdr lst)))))
  (get-right SIM-DEVICES))

(define (xbee-tick xb)
  (length SIM-DEVICES))

(define (xbee-list-nodes)
  (map 
   (lambda (device)
     (list 
      (device 'get-id)
      (device 'get-address)))
   SIM-DEVICES))
(define xbee-list xbee-list-nodes)

(define (xbee-read-frame xb)
  (if (null? ANSWERS)
      0
      (let ((ret (car ANSWERS)))
        (set! ANSWERS (cdr ANSWERS))
        ret)))
(define xbee-read xbee-read-frame)

(define (xbee-discover-nodes xb)
  #t)

(define xbee-discover xbee-discover-nodes)

(define print-frame display)

(define bytevector-length vector-length)
(define bytevector-ref vector-ref)
(define bytevector-set! vector-set!)
(define list->bytevector list->vector)
(define make-bytevector make-vector)
(define bytevector? vector?)
