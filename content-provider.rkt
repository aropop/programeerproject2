#lang racket

;---------------------------------------------------------------------
;|
;|    Content-provider.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Provides content from the database
;|
;---------------------------------------------------------------------

(require "database-manager.rkt"
         "steward-wraper.rkt"
         "device.rkt"
         "macros.rkt"
         "generic-data.rkt")

(#%provide content-provider%)

(define content-provider%
  (class object%
    (super-new)
    
    (init database-manager)
    
    (define*
      [db-manager~ database-manager])
    
    ;returns the stored stewards as steward objects
    (define/public (get-stewards master)
      (let* ([query (send steward-wrapper$ get-sql)]
             [steward-data (send db-manager~ execute/return query)]
             [create-steward (send steward-wrapper$ create-lambda)])
        (let loop
          ([stewards-list '()])          
          (if (send steward-data at-end?)            
              stewards-list
              (begin
                (send steward-data get-next-row)
                (loop
                 (cons (create-steward 
                        (send steward-data get-current-row-colum 0)
                        (send steward-data get-current-row-colum 1)
                        (send steward-data get-current-row-colum 2)
                        (send steward-data get-current-row-colum 3)
                        (get-devices (send steward-data get-current-row-colum 0)
                                     (send steward-data get-current-row-colum 1))
                        master)
                       stewards-list)))))))
    
    
    ;returns the devices as device objects
    (define/private (get-devices steward-id place)
      (let* (;prepare the query
             [query 
              (string-append 
               (send device-wrapper$ get-sql)
               (number->string steward-id))]
             ;execute the query
             [query-result (send db-manager~ execute/return query)]
             ;create a procedure that creates the objects which represent the devices
             [create-device (send device-wrapper$ create-lambda)])
        ;loop over the results
        (let loop 
          ([devices-list '()])
          (if (send query-result at-end?) ;stop condition
              devices-list ;return list of devices 
              (begin 
                (send query-result get-next-row)
                (loop
                 (cons (create-device ;create a device and put it in the list
                        (send query-result get-current-row-colum 0)
                        (send query-result get-current-row-colum 1)
                        (send query-result get-current-row-colum 2)
                        steward-id
                        place)
                       devices-list)))))))
    
    ;returns all rooms
    (define/public (get-rooms)
      (let* ([query "SELECT name FROM Room"]
             [result (send db-manager~ execute/return query)])
        (send result get-colum 0)))
    
    ;Returns all data in the dated data type
    (define/public (get-all-data)
      (let* ([query "SELECT date, type, value FROM Data"]
             [result (send db-manager~ execute/return query)])
        (if (not(= (send result number-rows) 0))
            (handle-data-result result)
            '())))
    
    ;Returns data for specific type
    (define/public (get-data-for-type type)
      (let* ([query (string-append 
                     "SELECT date, type, value FROM Data WHERE type='" 
                     (symbol->string type) "'")]
             [result (send db-manager~ execute/return query)])
        (handle-data-result result)))
    
    ;This procedure handles the conversion of db-result to list of dated-data-objects
    (define/private (handle-data-result result)
      (let lp ([list-of-dated-data '()])
        (send result get-next-row)
        (if (send result at-end?)
            list-of-dated-data
            (lp (cons 
                 (new 
                  dated-data%
                  [date (send result get-current-row-colum 0)]
                  [name (send result get-current-row-colum 1)]
                  [value (send result get-current-row-colum 2)])
                 list-of-dated-data)))))
    
    ;returns the number of elements in the data table
    (define/public (get-amount-of-data)
      (let* ([query "SELECT COUNT(*) FROM Data"]
             [result (send db-manager~ execute/return query)]
             )
        (send result get-next-row)
        (send result get-current-row-colum 0)))
    
    ;Returns the lastest timestamp of the data
    (define/public (last-data-stored-timestamp)
      (let* ([query "SELECT MAX(date) FROM Data"]
             [result (send db-manager~ execute/return query)])
        (send result get-next-row)
        (send result get-current-row-colum 0)))
    
    
    
    )
  )
