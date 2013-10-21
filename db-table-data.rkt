#lang racket

(define db-table-data%
  (class object%
    (init list-of-vectors) 
    (define* 
      [data~ list-of-vectors]
      [index~ 0]
      [amount-of-rows~ 'not-counted]) ;stored for performance
    
    (super-new)
    
    (define/public (get-next-row)
      (let ([ret  (list-ref data~ index~)])
        (set! index~ (+ index~ 1))
        ret))
    
    (define/public (number-rows)
      (if (eq? amount-of-rows~ 'not-counted)
          (begin
            (set! amount-of-rows~ (length data~))
            amount-of-rows~)
          amount-of-rows~))
    
    )
  )
