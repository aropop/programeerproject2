#lang racket

(require "macros.rkt")
(#%provide content-storer%)

(define content-storer%
  (class object%
    (super-new)
    
    (init content-provider
          database-manager
          )
    
    (define* 
      [content-provider~ content-provider]
      [database-manager~ database-manager])
    
    
    (define/public (store something)
      (let* ([type (send something get-type)]
            [is-type? (lambda (submitted-type)
                        (eq? type submitted-type))])
        (cond [(is-type? 'temp)
               (store-data something)]
              [else (error "Content-storer: Cannot store this" something)]
              )
        )
      )
    
    (define/private (store-data generic-data-object)
      (let ([query (string-append
                    "INSERT INTO Data (type, value) VALUES ('" 
                    (send generic-data-object get-type)
                    "', '"
                    (send generic-data-object get-value-as-string)
                    "')")])
        (send database-manager~ execute/no-return query)))
    
    
    )
  )