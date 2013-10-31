#lang racket

(define device%
  (class object%
    
    (init 
     (device-id -1) ;no device id yet 
          )
    
    (init-field (place~ 'no-place))
    
    (field
      [answer~ 'no-question-asked]
      [current-message~ 'no-message]
      )
    
    (define/public (recieve-message message)
      (set! current-message~ message))
    
    (define/public (get-type)
      'device)
    
    )
  )

(define switch%
  (class device%
    (super-new)
    
    (field 
     [state~ 'OFF]
     )
    
    (define accepted-states '(ON OFF))
    
    (define/public (get-state)
      state~)
    
    (define/public (set-state! new-state)
      (if (not (memq new-state accepted-states))
          (error "Unaccepted state in switch")
          (set! state~ new-state)))
    
    )
  )
      
    
    
      