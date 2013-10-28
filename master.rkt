#lang racket

(#%require "content-provider.rkt")
(#%require "content-storer.rkt")
(#%require "steward.rkt")

(define master%
  (class object%
    (super-new)
    
    (field (stewards~ '()))
    (field (content-provider~ (new content-provider%)))
    (field (content-storer~ (new content-storer%
                                [content-provider content-provider~])))
    
    
    (define/public (get-data which)
      (send content-provider~ get-stored-data which)
      )
    
    (define/public (init)
      
      )
    
    
    
    (define/private (store data)
      data)
    
    )
  )
