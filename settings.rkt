#lang racket

(#%provide SETTINGS)

(define settings%
  (class object%
    (field
      [db-path "./database/default"]
      
      )
    (super-new)
    )
  )

(define SETTINGS (new settings%))
    
      