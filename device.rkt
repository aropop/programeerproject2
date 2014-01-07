#lang racket

;---------------------------------------------------------------------
;|
;|    Device.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Abstraction and simulation of the physical devices and sensors
;|
;---------------------------------------------------------------------


(require "macros.rkt"
         "generic-data.rkt"
         "settings.rkt")
(provide switch% thermometer% supported-device-types)
(define supported-device-types '(switch thermometer))
(define device%
  (class object%
    (super-new)
    
    
    (init-field [place~ 'no-place]
                [device-id~ -1]
                [communication-address~ 'none]
                [name~ "No name"]
                [serial-number~ 0])
    
    (define*
      [input-pipe~ 'init]
      [output-pipe~ 'init]
      [input-port~ 'init]
      )
    
    (field
     [answer~ 'no-question-asked]
     [current-message~ 'no-message]
     [end-message~ '(END)]
     )
    
    ;abstract fields for the children of this object
    (abstract handle-message get-device-type get-status-message)
    
    
    
    ;content-storer dispatch
    (define/public (get-type)
      'device)
    
    ;is this device already in the database
    (define/public (is-already-stored?)
      (> device-id~ 0))
    
    ;provides the message which has to be responded when you do not know the message
    ;this is the same for every type of device
    (define/public (get-unknown)
      (get-field unknown-message SETTINGS)) ;store it in the settings so it's easily accesable
    
    ;returns the input port
    (define/public (get-input-port)
      (cond [(eq? input-pipe~ 'init)
             (get-output-port) ;instantiate the output port first, other wise we can't use the pipe
             (get-input-port)]
            [else
             ;We make a costum input port so that when a read is done we can handle the message
             ;without any steps between
             (let* ([read-lambda ;the lambda needed in the make-input-port put in a let for clean code
                     (lambda (skip) 
                       (begin 
                         (let-values ([(in out) (make-pipe)])
                           (set! current-message~ (read input-pipe~)) ;set the current message knowing it's not the last one                                
                           (handle-message) ;demand that the current message is handled
                           (write answer~ out)
                           in)))];return the answer
                    [input 
                     (make-input-port
                      'device-input-port
                      read-lambda 
                      #f
                      (lambda () 'closed)) ;close the port                     
                     ])
               (set! input-port~ input)
               input-port~)
             ]
            ))
    
    ;voor de writes
    (define/public (get-output-port)
      ;test if the pipe is already set
      (if (eq? output-pipe~ 'init)
          ;if not we need to set it
          (let-values ([(in out) (make-pipe)])
            (set! input-pipe~ in)
            (set! output-pipe~ out)
            output-pipe~)
          output-pipe~))
    
    
    )
  )

;Represents a switch
(define switch%
  (class device%
    (super-new)
    
    (field 
     [state~ 'OFF]
     )
    
    (inherit-field current-message~ answer~ name~ communication-address~ serial-number~)
    (inherit get-unknown)
    
    (define accepted-states '(ON OFF))
    
    (define/public (get-state)
      (list-ref accepted-states (random 2))) ;this is for simulation 
    
    (define/override (get-device-type)
      'switch)
    
    (define/override (get-status-message)
      '(GET POW))
    
    (define/private (get-state-datum)
      `(ACK (POW ,(get-state))))
    
    (define/public (set-state! new-state)
      (if (not (memq new-state accepted-states))
          (error "Unaccepted state in switch")
          (set! state~ new-state)))
    
    (define/override (handle-message)
      (let ([type (car current-message~)])
        (cond 
          ;Getters
          [(eq? type 'GET)
           
           (let ([command (cadr current-message~)])
             (cond [(or (eq? command 'POW) 
                        (eq? command 'ALL))
                    (set! answer~ (get-state-datum))]
                   [else 
                    (set! answer~ (get-unknown))]))]
          
          ;Setters
          [(eq? type 'PUT)
           (let* ([command-lst (cadr current-message~)]
                  [command (car command-lst)]
                  [parameter (cadr command-lst)])
             (cond [(eq? command 'POW)
                    (set-state! parameter)
                    (set! answer~ (get-state-datum))]
                   [else (get-unknown)]))]
          
          [else 
           (set! answer~ (get-unknown))]))
      answer~)
    
    )
  )

;Simulation of a thermometer device
(define thermometer%
  (class device%
    (super-new)
    
    (field 
     [temperature~ (new temperature-data% [value 32] [device-id -1])]
     )
    
    (inherit-field current-message~ answer~  name~ communication-address~ serial-number~)
    (inherit get-unknown)
    
    ;Private function returns a random temp
    (define/private (get-temperature) 
      (send temperature~ set-value! (+ (random 11) 15))
      (send temperature~ get-value))
    
    (define/override (get-device-type)
      'thermometer)
    
    (define/override (get-status-message)
      '(GET TEMP))
    ;Returns the temp data
    (define/private (get-temp-datum)
      (let ((t (get-temperature))
            (c-or-f (send temperature~ which-unit)))
        `(ACK (UNIT ,c-or-f) (TEMP ,t)))) ;Putting it last fixes the temperature
    
    ;Handle messages from input port
    (define/override (handle-message)
      (let ([type (car current-message~)])
        (cond 
          ;Getters
          [(eq? type 'GET)
           
           (let ([command (cadr current-message~)])
             (cond [(or (eq? command 'TEMP) 
                        (eq? command 'ALL))
                    (set! answer~ (get-temp-datum))]
                   [else 
                    (set! answer~ (get-unknown))]))]
          
          ;Setters
          ;no setters
          
          [else 
           (set! answer~ (get-unknown))]))
      answer~)
    
    )
  )



