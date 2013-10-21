#lang racket

(#%provide content-storer%)

(define content-storer%
  (class object%
    (super-new)
    
    (init-field (content-strorer '()))
    
    (define/public (store something)
      'do-something)
    
    (define/private (build-store-query)
      'test)
    
    )
  )