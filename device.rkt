#lang racket

;---------------------------------------------------------------------
;|
;|    Device.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Data storing object, all the magic happens in steward, this is
;|    just used so we can easily print and list devices without 
;|    connecting to the steward
;|
;---------------------------------------------------------------------


(require "database-saveable.rkt"
         "macros.rkt")
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
     (last-status~ "Searching for device...")
     (is-found?~ #f)
     (is-stored?~ #f))
    
    
    (define* 
      (sep "."))
    
    
    (define/private (serialize-com-adr com-adr)
      (define ret "")
      (let 
          lp
        ((idx 0))
        (if (>= idx (vector-length com-adr))
            ret
            (begin
              (string-append ret sep (number->string (vector-ref com-adr idx)))
              (lp (+ idx 1))))))
    
    (define/private (deserialize-com-adr com-adr)
      (list->vector (string-split com-adr sep)))
    
    (define/public (is-already-stored?)
      is-stored?~)
    
    ;For printing in front-end
    (define/public (get-com-adr-as-string)
      (define string (make-string (vector-length com-adr~) #\a))
      (let lp
        ((idx 0))
        (if (>= idx (vector-length com-adr~))
            string
            (begin
              (string-set! string idx (integer->char (vector-ref com-adr~ idx)))
              (lp (+ idx 1))))))
    
    (define/public (store-sql)
      (if (is-already-stored?)
          (string-append
           "UPDATE Device SET type='" type~ "', communication_address='" (serialize-com-adr com-adr~) "'"
           "WHERE device_id='" id~ "'")
          (string-append
           "INSERT INTO Device (device_id, type, communication_address, steward_id) VALUES ('"
           id~ "', '" type~ "', '" (serialize-com-adr com-adr~) "', " (number->string (get-field 
                                                   steward-id~
                                                   steward-wrapper~)) ")"))) 
    
    
    (define/public (get-sql)
      "SELECT device_id, communication_address, type FROM Device WHERE steward_id=")
    
    
    (define/public (create-lambda)
      (lambda (id com-adr type st place)
        (new device-wrapper%
             [id~ id]
             [place~ place]
             [com-adr~ (deserialize-com-adr com-adr)]
             [type~ type]
             [steward-wrapper~ st]
             [is-stored?~ #t])))))

(define device-wrapper$ (new device-wrapper%
                             [id~ 0]
                             [place~ "static"]
                             [com-adr~ "static"]
                             [type~ 'staticClass]
                             [steward-wrapper~ 0]))


