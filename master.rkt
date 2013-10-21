#lang racket

(#%require "content-provider.rkt")
(#%require "content-storer.rkt")
(#%require "steward.rkt")

(define master%
  (class object%
    (super-new)
    
    (field (stewards '()))
    (field (content-provider (new content-provider%)))
    (field (content-storer (new content-storer%
                                [content-provider content-provider])))
    
    
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
    
    )
  )

(define ik (new master%))