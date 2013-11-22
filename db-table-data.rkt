#lang racket

;---------------------------------------------------------------------
;|
;|    db-table-data.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Abstraction for the rows returned by a query
;|
;---------------------------------------------------------------------


(require "macros.rkt")

(provide db-table-data%
         )

(define db-table-data%
  (class object%
    (init query-result) 
    
    (define*
      [index~ 0]
      [at-end~ (empty? query-result)] ; if no data is initialised this data is empty
      [data~ query-result]
      [amount-of-rows~ 'not-counted];stored for performance
      [current-row~ 'none]) 
    
    (super-new)
    
    ;returns next row
    (define/public (get-next-row)
      (if (>= index~ (number-rows))
          (set! at-end~ #t)
          (let ([ret  (list-ref data~ index~)])
            (set! current-row~ ret)
            (set! index~ (+ index~ 1))
            ret)))
    ;returns the current row
    (define/public (get-current-row-colum num)
      (vector-ref current-row~ num))
    
    ;maps a certain proc on each row
    (define/private (data-map proc)
      (map proc data~))
    
    ;returns a whole column as a list
    (define/public (get-colum index)
      (data-map (lambda (v) (vector-ref v index))))
    
    
    ;returns the number of rows
    (define/public (number-rows)
      (if (eq? amount-of-rows~ 'not-counted) ;store them for extra performens
          (begin
            (set! amount-of-rows~ (length data~))
            amount-of-rows~)
          amount-of-rows~))
    ;returns a colum of the next row
    (define/public (get-next-row-colum colum-id)
      (let ([row (get-next-row)])
        (if (> colum-id (vector-length row))
            (error "colum id out of bounds")
            (vector-ref colum-id row))))
    ;returns whether this data type is at its end
    (define/public (at-end?)
      (or
       (>= index~ (number-rows))
       (empty? current-row~)))
    
    )
  )