#lang racket

(define master%
  (class object%
    (field (stewards '()))
    (field (content-provider (new content-provider%)))
    (field (content-storer '()))
    (field (test 'arno))
    
    (define/public (get-data which)
      (send content-provider get-stored-data which)
      )
    
    (define/public (init)
      ;Get stewards from DB 
      (set! stewards (get-data 'stewards))
      ;initialize the stewards too
      (set! stewards 
            (map 
             (lambda (steward)
               (send steward init))
             stewards))
      )
    
    
    
    (define/private (store data)
      data)
    
    (super-new) ;constructor
    )
  )

(define ik (new master%))