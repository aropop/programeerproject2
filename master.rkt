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
    
    ;loads everything in place 
    (define/public (init)
      ;make a database manager object
      (let ([db-manager (new database-manager%)])
        ;initialise both content provider and content storer
        (set! content-provider~ (new content-provider% [database-manager db-manager]))
        (set! content-storer~ (new content-storer% 
                                   [content-storer content-storer~]
                                   [database-manager db-manager]))
        (set! stewards~ (send content-provider~ get-stewards this)) 
        )
      )
                                   
    
    (define/public (get-data which)
      (send content-provider~ get-stored-data which)
      )
    

    (define/private (store data)
      data)
    
    )
  )
