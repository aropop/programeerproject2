#lang racket
;---------------------------------------------------------------------
;|
;|    Parser.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Parses messages to generic data types
;|
;---------------------------------------------------------------------

(require "generic-data.rkt")
(provide parser%)

(define parser%
  (class object%
    (super-new)
    
    ;Parses a message from a device
    ;returns a list of generic-data-types
    (define/public (parse-message x-expression)
      (let ([ack-nack (car x-expression)])
        (if (eq? ack-nack 'NACK)
            (new generic-data%
                 [name "Error"]
                 [value 'Unknown-message])
            (let loop
              ([result '()]
               [remaining-answers (cdr x-expression)])
              (if (empty? remaining-answers)
                  result
                  ;differentiate between diffrent types of data we already know
                  (let ([type (caar remaining-answers)]
                        [value (cadar remaining-answers)]
                        [data-type 'not-set])
                    (cond [(eq? type 'TEMP) ;temperature data
                           (set! 
                            data-type
                            (new temperature-data%
                                 [value value]))]
                          ;other data types should come here
                          [else ;else we put it in a generic data type
                           (set! 
                            data-type
                            (new generic-data%
                                 [name type]
                                 [value value]))])
                    (loop (cons data-type
                                result)
                          (cdr remaining-answers))))))))
    
    ;returns an x-expression for this specific generic-data object
    (define/public (unparse-generic-data generic-data-type)
      (list (get-field name~ generic-data-type)
            (get-field value~ generic-data-type)))
    
    ;Makes a get X-expression
    (define/public (unparse-get list-of-generic-data-types)
      (cons 'GET (map (lambda (x) ;lambda is required because in classes procedures should always be in an application
                        (unparse-generic-data x))
                      list-of-generic-data-types)))
    
    ;Makes a put X-expression
    (define/public (unparse-put list-of-generic-data-types)
      (cons 'PUT (map (lambda (x) 
                        (unparse-generic-data x))
                      list-of-generic-data-types)))
    
    
    )
  )