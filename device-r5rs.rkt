#lang r5rs


(#%provide device-r5rs)

;Wraper for the devices 
(define (device-r5rs device-id com ser pla nam)
  
  (define (send-message mes)
    'iets)
  
  
  (define (dispatch mes . args)
    (cond  ((eq? mes 'send-message) (apply args send-message))
           (else '(Unknown message))))
  
  dispatch)