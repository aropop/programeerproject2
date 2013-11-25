#lang racket

;---------------------------------------------------------------------
;|
;|    Content-provider.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Provides content from the database
;|
;---------------------------------------------------------------------


(#%provide SETTINGS)

(define settings%
  (class object%
    (field
      [db-path "./database/default"]
      [unknown-message '(Unknown Message)]
      [author "Arno De Witte"]
      [title "Control Your House"]
      [data-get-interval 60000] ; 1000 = 1 second 
      )
    (super-new)
    )
  )

(define SETTINGS (new settings%))
    
      