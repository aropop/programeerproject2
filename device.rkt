#lang racket

;---------------------------------------------------------------------
;|
;|    Device.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Abstraction and simulation of the physical devices and sensors
;|
;---------------------------------------------------------------------


(require "database-saveable.rkt")
(provide device-wrapper% device-wrapper$)


(define 
  device-wrapper%
  (class*
      object% (database-saveable<%>)
    (super-new)
    
    (init-field
     id~
     place~
     com-adr~
     type~
     steward-wrapper~
     (is-stored?~ #f))
    
    (define/public (is-already-stored?)
      is-stored?~)
    
    
    (define/public (store-sql)
      (if (is-already-stored?)
          (string-append
           "UPDATE Device SET type='" (symbol->string type~) "', communication_address='" com-adr~ "'"
           "WHERE device_id='" id~ "'")
          (string-append
           "INSERT INTO Device (device_id, type, communication_address, steward_id) VALUES ('"
           id~ "', '" type~ "', '" com-adr~ "', " (get-field id~ steward-wrapper~) ")"))) 
           
    
    (define/public (get-sql)
      "SELECT device_id, communication_address, type FROM Device WHERE steward_id=")
      
    
    (define/public (create-lambda)
      (lambda (id com-adr type st place)
        (new device-wrapper%
             [id~ id]
             [place~ place]
             [com-adr~ com-adr]
             [type~ type]
             [steward-wrapper~ st]
             [is-stored~ #t])))))

(define device-wrapper$ (new device-wrapper%
                             [id~ 0]
                             [place~ "static"]
                             [com-adr~ "static"]
                             [type~ 'staticClass]
                             [steward-wrapper~ 0]))
                             

