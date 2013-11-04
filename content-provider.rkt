#lang racket

(require "database-manager.rkt"
         "steward.rkt"
         "device.rkt"
         "macros.rkt")

(#%provide content-provider%)

(define content-provider%
  (class object%
    (super-new)
    
    (init database-manager)
    
    (define*
      [db-manager~ database-manager])
    
    (define/public (get-stored-data which room)
      (cond ((eq? which 'avg-temp)
             (send db-manager~ execute/return (build-select-query which room)))
            (else
             (error "Unknown data stored, couldn't get"))))
    
    (define/public (get-stewards master)
      (let* ([query "SELECT steward_id, room_name, name, serial_number, communication_adress FROM Steward"]
             [steward-data (send db-manager~ execute/return query)]
             [create-steward (lambda (id place name serial_number communication_adress)
                               (let ([devices (get-devices id place)])
                                 (new steward% 
                                      [place~ place]
                                      [master master]
                                      [devices devices]
                                      [is-already-stored #t]
                                      [name~ name]
                                      [serial-number~ serial_number]
                                      [communication-adress~ communication_adress]
                                      [steward-id~ id])))])
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
    
    
    (define/private (get-devices steward-id place)
      (let* (;prepare the query
             [query (string-append "SELECT device_id, type FROM Device WHERE steward_id=" (number->string steward-id))]
             ;execute the query
             [query-result (send db-manager~ execute/return query)]
             ;create a procedure that creates the objects which represent the devices
             [create-device (lambda (type device-id) 
                              (cond [(eq? type 'switch) 
                                     (new switch% [device-id~ device-id] [place~ place])]
                                    [else
                                     (error "Cannot create device from type " type)]))])
             ;loop over the results
             (let loop 
               ([devices-list '()])
               (send query-result get-next-row)
               (if (send query-result at-end?) ;stop condition
                   devices-list ;return list of devices 
                   (loop
                    (cons (create-device ;create a device and put it in the list
                           (string->symbol (send query-result get-current-row-colum 1))
                           (send query-result get-current-row-colum 0)))))
               )                
             )
        )
      
      
      
      (define/private (build-select-query which room)
        (cond [(eq? which 'avg-temp)
               (string-append "SELECT AVG(temp) FROM sensor-data WHERE room='" room "'")]
              
              [else
               (error "Unknown data stored, couldn't get")]))
      
      
      
      )
    )