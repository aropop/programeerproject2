#lang racket

(define generic-data%
  (class object%
    (super-new)
    (init name value)
    
    (field [name~ name] [value~ value])

    
    
    (define/public (get-name)
      name~)
    
    (define/public (get-value)
      value~)
    
    (define/public (set-value! val)
      (set! value~ val))
    )
  )

(define temperature-data%
  (class generic-data%
    
    (init value)
    
    (define  celcius~ #t)
    
    (inherit/super set-value! get-value)
    
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
    )
  )