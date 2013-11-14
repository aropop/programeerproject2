#lang racket

;---------------------------------------------------------------------
;|
;|    Content-provider.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Provides content from the database
;|
;---------------------------------------------------------------------

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
      (let* ([query "SELECT steward_id, room_name FROM Steward"]
             [steward-data (send db-manager~ execute/return query)]
             [create-steward (lambda (id place)
                               (let ([devices (get-devices id place)])
                                 (new steward% 
                                      [place~ place]
                                      [master master]
                                      [devices devices]
                                      [is-already-stored #t]
                                      [steward-id~ id])))])
        (let loop
          ([stewards-list '()])          
          (if (send steward-data at-end?)            
              stewards-list
              (begin
                (send steward-data get-next-row)
                (loop
                 (cons (create-steward 
                        (send steward-data get-current-row-colum 0)
                        (send steward-data get-current-row-colum 1))
                       stewards-list)))))
        )
      )
    
    
    (define/private (get-devices steward-id place)
      (let* (;prepare the query
             [query (string-append "SELECT device_id, type , name, serial_number, communication_adress FROM Device WHERE steward_id=" (number->string steward-id))]
             ;execute the query
             [query-result (send db-manager~ execute/return query)]
             ;create a procedure that creates the objects which represent the devices
             [create-device (lambda (type device-id name serial-number com-adr) 
                              (cond [(eq? type 'switch) 
                                     (new switch%
                                          [device-id~ device-id]
                                          [place~ place]
                                          [communication-address~ com-adr]
                                          [name~ name]
                                          [serial-number~ serial-number])]
                                    [(eq? type 'thermometer)
                                     (new thermometer%
                                          [device-id~ device-id]
                                          [place~ place]
                                          [communication-address~ com-adr]
                                          [name~ name]
                                          [serial-number~ serial-number])]
                                    
                                    [else
                                     (error "Cannot create device from type " type)]))])
        ;loop over the results
        (let loop 
          ([devices-list '()])
          (if (send query-result at-end?) ;stop condition
              devices-list ;return list of devices 
              (begin 
                (send query-result get-next-row)
                (loop
                 (cons (create-device ;create a device and put it in the list
                        (string->symbol (send query-result get-current-row-colum 1))
                        (send query-result get-current-row-colum 0)
                        (send query-result get-current-row-colum 2)
                        (send query-result get-current-row-colum 3)
                        (send query-result get-current-row-colum 4))
                       devices-list))))
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