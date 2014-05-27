#lang racket

;---------------------------------------------------------------------
;|
;|    Generic-Data.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Abstraction for diffrent forms of data
;|
;---------------------------------------------------------------------

(require "database-saveable.rkt")
(provide generic-data%
         typed-data%
         response-message%
         dated-data%)
(define generic-data%
  (class* object% (database-saveable<%>)
    (super-new)
    (init name value)
    
    (field [name~ name] [value~ value])
    
    (define/public (get-name)
      name~)
    
    (define/public (get-value)
      value~)

    (define/public (get-type)
      'generic-data)
    
    (define/public (set-value! val)
      (set! value~ val))
        
    
    (define/public (store-sql)
      (string-append
                    "INSERT INTO Data (type, value, device_id) VALUES ('" 
                    (get-name)
                    "', '"
                    (get-value)
                    "', "
                    0
                    ")"))
    
    (define/public (create-lambda)
      (lambda (name value) (new generic-data% [name name] [value value])))
    
    (define/public (get-sql)
      "SELECT type, value FROM Data WHERE data_id=")))


;Represents the return of a call
;Adds just the device id for storing 
(define response-message%
  (class generic-data%
    (inherit-field value~ name~)
    (init device-id)
    (field [device-id~ device-id])
    (super-new)
    
    (inherit get-name set-value! get-value)
    
    (define/public (get-device-id)
      device-id~)
    
    (define/public (set-device-id! id)
      (set! device-id~ id))
    
    (define/override (store-sql)
      (string-append
                    "INSERT INTO Data (type, value, device_id) VALUES ('" 
                    (get-name)
                    "', '"
                    (get-value)
                    "', '"
                    device-id~
                    "')"))
    
    (define/override (create-lambda)
      (lambda (name value device-id)
        (new response-message% [name name] [value value] [device-id device-id])))
    
    (define/override (get-sql)
      "SELECT type, value, device_id FROM Data WHERE data_id=")))

;New types of data should be added here
(define typed-data%
  (class response-message%
    (inherit-field value~ name~ device-id~)
    (super-new)
    (field
     (to-number-lambda string->number)
     (type~ ;example temperature
      (cond
        [(equal? (substring value~
                            (- (string-length value~) 2)
                            (string-length value~)) 
                 "°C")
         (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 2)))))
         "temperature"]
        [(equal? (substring value~
                            (- (string-length value~) 2)
                            (string-length value~))
                 "lx")
          (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 2)))))
         "brightness"]
        [(equal? (substring value~
                            (- (string-length value~) 2)
                            (string-length value~))          
                 "Hz")
         (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 2)))))
         "frequency"]
        [(equal? (substring value~
                            (- (string-length value~) 1)
                            (string-length value~))               
                 "V")
          (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 1)))))
         "voltage"]
        [(equal? (substring value~
                            (- (string-length value~) 1)
                            (string-length value~))
                 
                 "%")
         (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 1)))))
         "percentage"]
         [(equal? (substring value~
                            (- (string-length value~) 1)
                            (string-length value~))
                 "W")
         (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 1)))))
         "watt"]
        [(< (string-length value~) 3) 
         (set! to-number-lambda (lambda (s) 1))
         "unkown"]
        [(equal? (substring value~
                            (- (string-length value~) 3)
                            (string-length value~))
                 "hPa")
         (set! to-number-lambda (lambda (s) (display s) (string->number (substring s 0 (- (string-length value~) 3)))))
         "pressure"]
        [(equal? (substring value~
                            (- (string-length value~) 3)
                            (string-length value~))
                 "kWh")
         (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 3)))))
         "watthour"]
        [(equal? (substring value~
                            (- (string-length value~) 2)
                            (string-length value~))
                 "mA")
         (set! to-number-lambda (lambda (s) (string->number (substring s 0 (- (string-length value~) 2)))))
         "ampère"]
        [else
         (set! to-number-lambda (lambda (s) 1))
         "unknown"])))
    (inherit get-name set-value! get-value get-device-id set-device-id!)
    
    (define/public (get-full-string)
      (string-append
       name~ "=" value~))
    
    (define/public (get-nice-string)
      (string-append name~ ": " value~ "<br />"))
    
    (define/public (get-value-type)
      type~)
    
    (define/public (get-value-as-number)
      (to-number-lambda (get-value)))
    
    (define/override (create-lambda)
      (lambda (name value device-id)
        (new typed-data% [name name] [value value] [device-id device-id])))))


;has an extra date field and procedures to extract days and atc
;the date string has to be of the form "YYYY-MM-DD HH:MM:SS"
(define dated-data%
  (class typed-data%
    (init-field 
     (date ""))
    (super-new)
    (inherit-field value~ name~ device-id~)
    (inherit get-name set-value! get-value get-device-id
             set-device-id! get-full-string get-value-type get-value-as-number)
    
    (define/public (get-seconds)
      (substring date 17))
    
    (define/public (get-minutes)
      (substring date 14 16))
    
    (define/public (get-hours)
      (substring date 11 13))
    
    (define/public (get-days)
      (substring date 8 10))
    
    (define/public (get-month)
      (substring date 5 7))
    
    (define/public (get-year)
      (substring date 0 4))
    

    
    
    )
  )






