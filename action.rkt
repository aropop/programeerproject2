#lang racket

(require "macros.rkt"
         "database-saveable.rkt")

(provide action% action$)

(define action%
  (class* object% (database-saveable<%>)
    (super-new)
    (init-field
     type~
     value~
     source-device-id~
     destination-device-id~
     command~
     (equality~ <)
     (action-id~ -1))
    
    
    (define*
      [supported-equalities (list (cons "<" <)
                                  (cons ">" >)
                                  (cons "<=" <=)
                                  (cons ">=" >=)
                                  (cons "=" =)
                                  (cons "eq" eq?))])
    
    
    (define/private (serialize-eq eq)
      (let lp
        ((rest supported-equalities))
        (if (eq? (cdar rest) eq)
            (caar rest)
            (lp (cdr rest)))))
    
    (define/private (deserialize-eq eq)
      (let lp
        ((rest supported-equalities))
        (if (equal? (caar rest) eq)
            (cdar rest)
            (lp (cdr rest)))))
    
    (define/private (already-stored?)
      (= -1 action-id~))
    
    (define/public (execute steward)
      (define (check&execute d-o)
        (define v (send d-o get-value-as-number))
        (when (equality~ v value~)
          (displayln (string-append "Executed action id:" (number->string action-id~)))
          (send steward send-message-to-remote destination-device-id~ command~)))
      (let get-right-data
        ((lst (send steward get-device-status source-device-id~)))
        (cond
          [(null? lst) 'do-nothing]
          [(equal? (send (car lst) get-name) type~) (check&execute (car lst))]
          [else (get-right-data (cdr lst))])))
    
    (define/public (get-equality-string)
      (serialize-eq  equality~))
    
    (define/public (get-equality-list)
      (map car supported-equalities))
    
    ;Returns a cons of a lambda to be executed after this
    ;Should only be stored once
    (define/public (store-sql)
      (let ([query (string-append
                    "INSERT INTO Action (type, value, destination_device_id, source_device_id,"
                    "command, equality) VALUES ('"
                    type~ "', '" value~"', '" destination-device-id~ "', '"
                    source-device-id~ "', '" command~
                    "', '" (serialize-eq equality~) "')")]
            [update-lambda (lambda (new-id content-storer)
                             (set! action-id~ new-id))])
        ;return query
        (cons query update-lambda)))
    
    ;Makes a steward-wrapper object
    (define/public (create-lambda)
      (lambda (type value d-device-id s-device-id command eq-string action-id) 
        (new action% ;make a wrapper will connect when needed
             [type~ type]
             [value~ value]
             [destination-device-id~ d-device-id]
             [source-device-id~ s-device-id]
             [command~ command]
             [equality~ (deserialize-eq eq-string)]
             [action-id~ action-id])))
    
    (define/public (get-sql)
      "SELECT type, value, destination_device_id, source_device_id, command, equality, action_id
 FROM Action")))

(define action$ (new action%
                     [type~ 'none]
                     [value~ 0]
                     [destination-device-id~ 0]
                     [source-device-id~ 0]
                     [command~ 0]))
