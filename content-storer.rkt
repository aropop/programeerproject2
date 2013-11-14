#lang racket

;---------------------------------------------------------------------
;|
;|    Content-storer.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Stores objects and data to the database
;|
;---------------------------------------------------------------------


(require "macros.rkt")
(#%provide content-storer%)

(define content-storer%
  (class object%
    (super-new)
    
    (init content-provider
          database-manager
          )
    
    (define* 
      [content-provider~ content-provider]
      [database-manager~ database-manager])
    
    
    ;store dispatch which allows easy storing
    (define/public (store something)
      (let* ([type (send something get-type)]
             [is-type? (lambda (submitted-type)
                         (eq? type submitted-type))])
        (cond [(is-type? 'temp)
               (store-data something)]
              [(is-type? 'device)
               (store-device something)]
              [(is-type? 'steward)
               (store-steward something)]
              
              [else (error "Content-storer: Cannot store this" something)]
              )
        )
      )
    
    ;Help procedure that returns wether a room/place is stored
    (define/private (is-room-stored? room)
      (let* (
             [query (string-append "SELECT * FROM Room WHERE name='" room "'")]
             [result (send database-manager~ execute/return query)]
             )
        ;if there are no entries the data should be at end immidiatly
        (not (send result at-end?))))
    
    ;stores a place/room
    (define/private (store-room room-name)
      (send 
       database-manager~
       execute/no-return
       (string-append
        "INSERT INTO Room VALUES ('" room-name "')"
        )
       )
      )
    
    ;stores a generic data object
    (define/private (store-data generic-data-object)
      (let ([query (string-append
                    "INSERT INTO Data (type, value) VALUES ('" 
                    (send generic-data-object get-type)
                    "', '"
                    (send generic-data-object get-value-as-string)
                    "')")])
        (send database-manager~ execute/no-return query)))
    
    ;stores a device, when it is not already stored it expects an steward-id
    (define/private (store-device device . steward-id)
      (let ([type (send device get-device-type)]
            [name (get-field name~ device)]
            [com-adr (get-field communication-address~ device)]
            [ser-nbr (get-field serial-number~ device)])
        (cond [(send device is-already-stored?)
               (let* ([device-id (get-field device-id~ device)]
                      [query (string-append "UPDATE Device SET type='" 
                                            (symbol->string type) "', "
                                            "serial_number='" (number->string ser-nbr) "', "
                                            "communication_adress='" com-adr "', "
                                            "name='" name "', "
                                            "WHERE device_id="
                                            (number->string device-id))])
                 (send database-manager~ execute/no-return query))
               ]
              [else
               (let* ([steward-id (if (empty? steward-id)
                                      (error "Store device expects a steward id when not stored")
                                      (car steward-id))]          
                      [query (string-append "INSERT INTO Device (type, name, serial_number, communication_adress, steward_id) VALUES ('"
                                            (symbol->string type)
                                            "', '"
                                            name
                                            "', '"
                                            (number->string ser-nbr)
                                            "', '"
                                            com-adr
                                            "', '"
                                            (number->string steward-id)
                                            "')")])
                 (send database-manager~ execute/no-return query)
                 )]
              )
        )
      )
    
    ;stores a steward to the database
    (define/private (store-steward steward)
      ;get all the info 
      (let ([steward-id (get-field steward-id~ steward)]
            [room (get-field place~ steward)])
        ;update query or insert query
        (cond [(send steward is-already-stored?) ;device is already in the database so we need to update
               (let ([query (string-append "UPDATE Steward SET "
                                           "room_name='" room "' "
                                           "WHERE steward_id=" (number->string steward-id))]
                     )
                 ;store the query
                 (send database-manager~ execute/no-return query)
                 )
               ]
              [else ;steward is not stored already
               ;make sure the room is stored
               (cond [(not (is-room-stored? room))
                      ;if not stored store it
                      (store-room room)])
               ;build the query to store the steward
               (let ([query (string-append
                             "INSERT INTO Steward (room_name) VALUES ('"
                             room "')")])
                 ;execute query
                 (send database-manager~ execute/no-return query)
                 ;set the device id right so that the devices are stored with the correct id
                 (set! steward-id (send database-manager~ last-inserted-id))
                 )
               ]
              )
        
        ;store the devices too
        (map (lambda (device)
               (store-device device steward-id)) ;we do not know if it is already stored so we add the id
             (send steward get-device-list))           
        )
      )
    
    
    
    )
  )