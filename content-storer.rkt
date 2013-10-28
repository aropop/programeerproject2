#lang racket

(#%provide content-storer%)

(define content-storer%
  (class object%
    (super-new)
    
    (init content-provider~
          database-manager~
          )
    
    
    
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
                    "
    
    (define/private (build-store-query)
      'test)
    
    )
  )