#lang racket
(#%provide database-manager%)

(define database-manager%
  (class object%
    (super-new)
    
    (field (con 123))
    
    (define/public (execute sql)
      'result)
    )
  )