#lang racket

(require "database-manager.rkt"
         "steward.rkt")

(#%provide content-provider%)

(define content-provider%
  (class object%
    (super-new)
    
    (field (db-manager~ (new database-manager%)))
    
    (define/public (get-stored-data which room)
      (cond ((eq? which 'avg-temp)
             (send db-manager~ execute/return (build-select-query which room)))
            (else
             (error "Unknown data stored, couldn't get"))))
    
    (define/public (get-stewards master)
      (let* ([query "SELECT steward_id, room_name, name, serial_number, communication_adress FROM Steward"]
             [steward-data (send db-manager~ execute/return query)]
             [create-steward (lambda (id place name serial_number communication_adress)
                               (let ([devices (get-devices id)])
                                 (new steward% 
                                      [place place]
                                      [master master]
                                      [devices devices]
                                      [is-already-stored #t]
                                      [name~ name]
                                      [serial-number~ serial_number]
                                      [communication-adress communication_adress]
                                      [steward-id id])))])
        (send steward-data get-next-row)
        (let loop
          ([stewards-list '()])
          (send steward-data get-next-row)
          (if (send steward-data at-end?)            
              stewards-list
              (loop
               (cons (create-steward 
                      (send steward-data get-current-row-colum 0)
                      (send steward-data get-current-row-colum 1)
                      (send steward-data get-current-row-colum 2)
                      (send steward-data get-current-row-colum 3)
                      (send steward-data get-current-row-colum 4))
                     stewards-list))))
        )
      )
    
    
    (define/private (get-devices steward-id)
      (let* ([query (string-append "SELECT device_id, type FROM Device WHERE steward_id=" (number->string steward-id))])
        'nothing))
    
    
    
    (define/private (build-select-query which room)
      (cond [(eq? which 'avg-temp)
             (string-append "SELECT AVG(temp) FROM sensor-data WHERE room='" room "'")]
            
            [else
             (error "Unknown data stored, couldn't get")]))
    
    
    
    )
  )