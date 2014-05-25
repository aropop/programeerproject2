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
      [database-manager~ database-manager])
    
    
    ;Stores object which implement database-saveable
    (define/public (store something)
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
                 ((cdr possible-cons) (send database-manager~ last-inserted-id)
                                      this)]
                [else
                 (send
                  database-manager~
                  execute/no-return
                  possible-cons)]))))))