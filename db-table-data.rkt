#lang racket
(require "macros.rkt")

(provide db-table-data%
         )

(define db-table-data%
  (class object%
    (init query-result) 
    
    (define*
      [index~ 0]
      [data~ query-result]
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
    
    (define/public (get-next-row-colum colum-id)
      (let ([row (get-next-row)])
        (if (> colum-id (vector-length row))
               (error "colum id out of bounds")
               (vector-ref colum-id row))))
    
    (define/public (at-end?)
      (empty? data~))
    
    )
  )
(define x (new db-table-data% [query-result (list (vector 1 2 3))]))
(define y (new db-table-data% [query-result (list (vector 1 5 3))]))