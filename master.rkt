#lang racket


(require "content-provider.rkt"
         "content-storer.rkt"
         "steward.rkt"
         "macros.rkt"
         "database-manager.rkt")
(provide master%)
(define master%
  (class object%
    (super-new)
    
    (define*
      [stewards~ '()]
      ;procedure init initialises these objects to prevent 2 database connections
      [content-provider~ 'uninitialised] 
      [content-storer~ 'uninitialised]
      )
    
    (define/public (init)
      (let ([db-manager (new database-manager%)])
        (set! content-provider~ (new content-provider% [database-manager db-manager]))
        (set! content-storer~ (new content-storer% 
                                   [content-storer content-storer~]
                                   [database-manager db-manager])))
      )
                                   
    
    (define/public (get-data which)
      (send content-provider~ get-stored-data which)
      )
    
    
    
    
    (define/private (store data)
      data)
    
    )
  )
