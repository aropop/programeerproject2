#lang racket

(require "macros.rkt"
         "generic-data.rkt")
(provide switch% thermometer%)
(define supported-device-types '(Switch Thermometer))
(define device%
  (class object%
    (super-new)
   
    
    (init-field [place~ 'no-place]
                [device-id~ -1])
    
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
    
    (abstract handle-message get-device-type)
    
    (define/public (recieve-message message)
      (set! current-message~ message))
    
    ;content-storer dispatch
    (define/public (get-type)
      'device)
    
    ;is this device already in the database
    (define/public (is-already-stored?)
      (> device-id~ 0))
    
    (define/public (get-unknown)
      '(Unknown Message))
    
    ;voor de reads
    (define/public (get-input-port)
      (cond [(eq? input-pipe~ 'init)
             (get-output-port) ;instantiate the output port first, other wise we can't use any pipes
             (get-input-port)]
            [else
             (let* ([read-lambda
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

(define switch%
  (class device%
    (super-new)
    
    (field 
     [state~ 'OFF]
     )
    
    (inherit-field current-message~ answer~)
    (inherit get-unknown)
    
    (define accepted-states '(ON OFF))
    
    (define/public (get-state)
      state~)
    
    (define/override (get-device-type)
      'switch)
    
    (define/private (get-state-datum)
      `(ACK (POW ,state~)))
    
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

(define thermometer%
  (class device%
    (super-new)
    
    (field 
     [temperature~ (new temperature-data% [value 32])]
     )
    
    (inherit-field current-message~ answer~)
    (inherit get-unknown)
   
    
    (define/public (get-temperature)
      (send temperature~ get-value))
    
    (define/override (get-device-type)
      'thermometer)
    
    (define/private (get-temp-datum)
      (let ((t (get-temperature))
            (c-or-f (send temperature~ which-unit)))
      `(ACK (TEMP ,t) (UNIT ,c-or-f))))
    
    
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



