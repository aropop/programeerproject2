#lang racket

;---------------------------------------------------------------------
;|
;|    Content-storer.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Stores objects and data to the database
;|
;---------------------------------------------------------------------


(require "macros.rkt")
(#%provide content-storer%)

(define content-storer%
  (class object%
    (super-new)
    
    (init content-provider
          database-manager)
    
    (define* 
      [content-provider~ content-provider]
      [database-manager~ database-manager])
    
    
    ;store dispatch which allows easy storing
    (define/public (store something)
      (let* ([type (with-handlers ((exn:fail? (lambda (e)
                                                'nothing)))
                     (send something get-type))]
             [is-type? (lambda (submitted-type)
                         (eq? type submitted-type))])
        (cond [(or 
                (is-type? 'temp-celcius)
                (is-type? 'temp-fahrenheit)
                (is-type? 'response-message))
               (store-data something)]
              [else
               (with-handlers 
                   ((exn:fail? (lambda ;display instead of error
                                   (e)
                                 (display "Cannot store (")
                                 (display e)
                                 (display "): ")
                                 (pretty-display something))))
                 (let 
                     ;May return a cons with insert query and update id lambda
                     ((possible-cons (send something store-sql)))
                   (cond [(cons? possible-cons)
                          (send database-manager~ execute/no-return (car possible-cons))
                          ((cdr possible-cons) (send database-manager~ last-inserted-id))]
                         [else
                          (send
                  database-manager~
                  execute/no-return
                  possible-cons)])))])))
    
    ;stores a generic data object
    (define/private (store-data response-message-object)
      (let ([query (string-append
                    "INSERT INTO Data (type, value, device_id) VALUES ('" 
                    (symbol->string (get-field name~ response-message-object))
                    "', '"
                    (send response-message-object get-value-as-string)
                    "', "
                    (number->string (get-field device-id~ response-message-object))
                    ")")])
        (send database-manager~ execute/no-return query)))))