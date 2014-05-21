#lang r5rs


;xbee sim

(#%provide xbee-initialise xbee-write xbee-tick xbee-list-nodes xbee-discover-nodes xbee-read-frame print-frame)
(#%require "device-r5rs.rkt")

(define SIM-DEVICES 
  (list
   (device-slip "ABCDEFGH" (vector 0 0 0 0 0 0 0 0) "addr" "Kitchen" "Kitchen sensor" 'multiSensor)
   (device-slip "IJKLMNOP" (vector 0 0 0 0 0 0 0 1) "addr" "KidsRoom" "kids room lights" 'smartPlug)))

(define ANSWERS '())

(define (generate-answer type mes adr)
  (cons 
   (cond 
     ((eq? type 'multiSensor)
      (cond ((eq? mes 'GET)
             (vector 
              ;message type
              144 
              ;64 bit address
              (vector-ref adr 0)  (vector-ref adr 1)  (vector-ref adr 2) (vector-ref adr 3)
              (vector-ref adr 4) (vector-ref adr 5) (vector-ref adr 6) (vector-ref adr 7)
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
              10 10))))) ANSWERS))

(define (xbee-initialise ign ore)
  SIM-DEVICES)

(define (xbee-write xb adr mes)
  (define (get-right lst)
    (if (null? lst)
        (display "could not write")
        (if (eq? ((car lst) 'get-address) adr)
            (generate-answer ((car lst) 'get-type) mes adr)
            (get-right (cdr lst)))))
  (get-right SIM-DEVICES))

(define (xbee-tick xb)
  (length SIM-DEVICES))

(define (xbee-list-nodes xb)
  (map 
   (lambda (device)
     (list 
      (device 'get-id)
      (device 'get-address)))
   xb))

(define (xbee-read-frame xb)
  (let ((ret (car ANSWERS)))
    (set! ANSWERS (cdr ANSWERS))
    ret))

(define (xbee-discover-nodes xb)
  #t)

(define print-frame display)
