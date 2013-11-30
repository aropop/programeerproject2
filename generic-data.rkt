#lang racket

;---------------------------------------------------------------------
;|
;|    Generic-Data.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Abstraction for diffrent forms of data
;|
;---------------------------------------------------------------------


(provide generic-data%
         temperature-data%
         response-message%
         dated-data%)
(define generic-data%
  (class object%
    (super-new)
    (init name value)
    
    (field [name~ name] [value~ value])
    
    (define/public (get-name)
      name~)
    
    (define/public (get-value)
      value~)
    
    ;for queries
    (define/public (get-value-as-string)
      (cond [(number? value~) (number->string value~)]
            [(symbol? value~) (symbol->string value~)]
            [(string? value~) value~]
            [else (error "Generic data cannot convert this to string" value~)]))
    
    (define/public (get-value-as-number)
      (cond [(number? value~) value~]
            [(symbol? value~) (symbol->number value~)]
            [(string? value~) (my-string->number value~)]
            [else (error "Cannot put " value~ " to a number type")]))
    
    (define/private (symbol->number val)
      (cond [(eq? 'ON) 1]
            [(eq? 'OFF) 0]
            [(eq? 'celcius) 1]
            [(eq? 'fahrenheit) 0]
            [else 1]))
    
    (define/private (my-string->number val)
      (cond [(string->number val) (string->number val)]
            [(string=? "ON" val) 1]
            [(string=? "OFF" val) 0]
            [(string=? "celcius" val) 1]
            [(string=? "fahrenheit" val) 0]
            [else (error "Cannot put " value~ " to a number type")]))
    
    (define/public (get-type)
      'generic-data)
    
    (define/public (set-value! val)
      (set! value~ val))
    )
  )


;Represents the return of a call
;Adds just the device id for storing 
(define response-message%
  (class generic-data%
    (inherit-field value~ name~)
    (init device-id)
    (field [device-id~ device-id])
    (super-new)
    
    (inherit get-value-as-string get-name set-value! get-value)
    
    (define/public (get-device-id)
      device-id~)
    
    (define/public (set-device-id id)
      (set! device-id~ id))
    
    (define/override (get-type)
      'response-message)
    
    )
  )
;represents temperature
(define temperature-data%
  (class response-message%
    
    (init value)
    
    (define  celcius~ #t)
    
    
    (inherit set-value! get-value get-value-as-string get-name)
    
    (super-new [name 'temp] [value value])
    
    (define/public (to-celcius)
      (cond [(not celcius~)
             (begin 
               (set-value! (* (- (get-value) 32) (/ 5 9)))
               (set! celcius~ #t))]
            ))
    
    (define/public (to-fahrenheit)
      (cond  [celcius~
              (begin 
                (set-value! (* (+ (get-value) 32) (/ 9 5)))
                (set! celcius~ #f))]))
    
    (define/public (which-unit)
      (if celcius~
          'celcius
          'fahrenheit))
    
    (define/override (get-type)
      (if celcius~
          'temp-celcius
          'temp-fahrenheit)) 
    )
  )

;has an extra date field and procedures to extract days and atc
;the date string has to be of the form "YYYY-MM-DD HH:MM:SS"
(define dated-data%
  (class generic-data%
    (init-field 
     (date ""))
    (super-new)

    
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






