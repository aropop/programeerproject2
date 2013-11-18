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
      )
    (super-new)
    )
  )

(define SETTINGS (new settings%))
    
      