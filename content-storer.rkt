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
              [(is-type? 'device)
               (store-device something)]
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
    
    ;stores a device, when it is not already stored it expects an steward-id
    (define/private (store-device device . steward-id)
      (cond [(send device is-already-stored?)
             (let* ([device-id (get-field device-id device)]
                    [type (send device get-type)]
                    [query (string-append "UPDATE Device SET type='" 
                                          (symbol->string type)
                                          "' WHERE device_id="
                                          (number->string device-id))])
                    (send database-manager~ execute/no-return device))
             ]
            [else
             (let* ([device-type (send device get-device-type)]
                    [steward-id (if (empty? steward-id)
                                    (error "Store device expects a steward id when not stored")
                                    (car steward-id))]          
                    [query (string-append "INSERT INTO Device (type, steward_id) VALUES ('"
                                          (symbol->string device-type)
                                          "', '"
                                          (number->string steward-id)
                                          "')")])
               (send database-manager~ execute/no-return device)
               )]
            )
      )
    
    (define/private (store-steward steward)
      
      'todo
      )
    
    
    
    )
  )