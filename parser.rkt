#lang racket
;---------------------------------------------------------------------
;|
;|    Parser.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Parses messages to generic data types
;|
;---------------------------------------------------------------------

(require "generic-data.rkt")
(provide parser% parser$)

(define parser%
  (class object%
    (super-new)
    
    ;Parses a message from a device
    ;returns a list of generic-data-types
    (define/public (parse-message string device-id)
      (let ([ack-nack (substring string 0 4)])
        (cond [(equal? ack-nack "nack")
               (new generic-data%
                    [name "Error"]
                    [value 'Unknown-message])]
              [(equal? (substring ack-nack 0 3) "ack")
               (new response-message%  
                    [name "ack"]
                    [value (substring 4 (string-length string))]
                    [device-id device-id])]
              [else
               (let*
                   ([tuples (string-split string ",")]
                    [splitted (map (lambda (s) (string-split s "=")) tuples)])
                 (map
                  (lambda (2-list)
                    (display (list-ref 2-list 0)) (display "---") (display (list-ref 2-list 1)) (newline)
                    (new typed-data%
                         [name (list-ref 2-list 0)]
                         [value (list-ref 2-list 1)]
                         [device-id device-id]))
                  splitted))])))
    
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
    
    ;unparses list of dated objects to json expressions
    (define/public (unparse-to-json list-of-dated-objects time-diff) ;time-diff is a symbol day, month, year
      (define (add-string-loop hash-table current-object get-proc) ;adds to the has the data of the current data object
        (let ([cur-str (if (hash-has-key? hash-table (send current-object get-name))
                           (hash-ref hash-table (send current-object get-name))
                           (let ([init-str (string-append 
                                            ; "\""
                                            ;(send current-object get-name)
                                            ;"\":"
                                            "{\n label:\""                                         
                                            (send current-object get-name)
                                            "\",\n data:[")])
                             (hash-set! hash-table ;initialise the string
                                        (send current-object get-name)
                                        init-str)
                             init-str))
                       ]
              [str (string-append "[" (get-proc current-object) ", "
                                  (number->string
                                   (send current-object get-value-as-number))
                                  "],")]) ;the comma at the end will cause a comma "too much" at the end
          
          (hash-set! hash-table (send  current-object get-name) 
                     (string-append
                      cur-str ;longest string first for performance
                      str))))
      (define built-hash-table (let 
                                   loop
                                 ([json-hash-table (make-hash)]
                                  [remaining-dated-objects list-of-dated-objects])
                                 ;cond to test if empty
                                 (cond 
                                   [(empty? remaining-dated-objects)
                                    json-hash-table]
                                   [else
                                    ;make new conditional to determine which proc to use for unparsing
                                    (let ([proc (lambda (obj) ;default is a day
                                                  (send obj get-days))])
                                      (cond
                                        [(eq? time-diff 'year)
                                         (set! proc (lambda (obj)
                                                      (send obj get-year)))]
                                        [(eq? time-diff 'month)
                                         (set! proc (lambda (obj)
                                                      (send obj get-month)))]
                                        [(eq? time-diff 'hour)
                                         (set! proc (lambda (obj)
                                                      (send obj get-hours)))]
                                        [(eq? time-diff 'minute)
                                         (set! proc (lambda (obj)
                                                      (send obj get-minutes)))] )
                                      (add-string-loop json-hash-table 
                                                       (car remaining-dated-objects) 
                                                       proc)
                                      (loop json-hash-table (cdr remaining-dated-objects)))])))
      (define return-string "")
      ;we have to map to remove excess "," and add "]"
      (hash-map
       built-hash-table
       (lambda (key string)
         (let*
             ([removed-komma-string 
               (if (> (string-length string) 0)
                   (substring string 0 
                              (- (string-length string) 1))
                   string)]
              [added-bracket (string-append removed-komma-string
                                            "]\n},\n")])
           (set! return-string (string-append return-string added-bracket)))))
      
      ;now there's still one excess ","
      (if (> (string-length return-string) 2)
          (substring return-string 0 (- (string-length return-string) 2)) ; 2 because we have the newline which apperently counts as a character aswell
          return-string))))

(define parser$ (new parser%))
