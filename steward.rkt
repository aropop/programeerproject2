#lang racket

(define steward%
  (class object%
    (field (devices '()))
    (field (place-in-house "Unknown place"))
    
    (define/public (init)
      'info
      )
    )
  )