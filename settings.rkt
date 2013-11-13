#lang racket

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
    
      