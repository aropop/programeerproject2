#lang racket

(#%require "database-manager.rkt")

(#%provide content-provider%)

(define content-provider%
  (class object%
    (super-new)
    
    (field (db-manager (new database-manager%)))
    
    (define/public (get-stored-data which room)
      (cond ((eq? which 'avg-temp)
             (send db-manager execute (build-select-query which room)))
            (else
             (error "Unknown data stored, couldn't get"))))
    
    (define/private (build-select-query which room)
      (cond ((eq? which 'avg-temp)
             (string "SELECT AVG(temp) FROM sensor-data WHERE room='" room "'"))
            
            (else
             (error "Unknown data stored, couldn't get"))))
    
    
    
    )
  )