#lang r5rs

;(#%require "xbee-simulation.rkt")
(#%provide device-slip)

;Wraper for the devices 
(define (device-slip device-id com ser pla nam type)
   
  (define (serialize)
    (vector 'device device-id com ser pla nam type))
  
  (define (dispatch mes . args)
    (cond  ((eq? mes 'get-device-id) device-id)
           ((eq? mes 'get-address) com)
           ((eq? mes 'serialize) (serialize))
           ((eq? mes 'get-place) pla)
           ((eq? mes 'get-type) type)
           ((eq? mes 'get-ser) ser)
           ((eq? mes 'get-name) nam)
           ((eq? mes 'get-id) device-id)
           (else '(Unknown message))))
  
  dispatch)