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
      )
    (super-new)
    )
  )

(define SETTINGS (new settings%))
    
      