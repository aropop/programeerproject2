#lang racket
;---------------------------------------------------------------------
;|
;|    FrontEnd.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Defines the frond end
;|
;---------------------------------------------------------------------

(require web-server/servlet-env
         web-server/templates
         web-server/http/response-structs
         web-server/http/bindings
         "settings.rkt"
         "parser.rkt"
         "action.rkt")

(provide html-front-end%)

(define front-end%
  (class object%
    
    (super-new)
    
    (init-field
     [master~ 'none])
    
    (abstract start)
    
    (define/public (get-master)
      master~)))

(define html-front-end%
  (class front-end%
    
    
    (super-new)
    
    (inherit-field master~)
    
    (define menuitems
      (list '("home" . "Home") '("stewards" . "Stewards")
            '("devices" . "Devices") '("actions" . "Actions") '("data" ."Data")))
    (define home-page 'home)
    
    (define help-text
      (string-append
       "Welcome to control your house!<br />"
       "This is the interface where you can control the device stored in your home. <br />"
       "In the menu above your can see stewards, devices, actions and data.<br />"
       "You can add new stewards on the stewards page, these stewards will automatically detect any"
       " devices in the room. These are listed on the device page. Note that it can take some time"
       " before these will show up. You can see the loading stage for each steward on the steward page"
       "<br />You can also add actions. These will execute a command when a certain value meets a "
       "certain condition. For example when you want the electric stove to turn on when the "
       "temperature is under 20°C, you select the thermometer device as source device. Then you select"
       " the plug connected to the stove as destination device. The type should be TEM as you want "
       "to measure the temperature. Value should be 20 and equality <. As we want to turn the power on"
       " The command should be 'SET POW=ON'. When your press add this action"
       " will be stored in the system and reguarly checked. Only if the condition is met it will "
       "execute.<br />Enjoy Control Your House!"))
    
    ;dispatches between diffrent requests (read pages)
    ;you can optionally add any messages 
    (define (dispatcher requests . message)
      (define response 'null)
      (display (request-bindings requests)) (newline)
      (call-with-current-continuation
       (lambda (exit)
         (let get-inside-main-loop
           (;handle the "static" elements of the site
            [page (extract-page (request-bindings requests))]
            [head (render-head-template)]
            [heading (render-heading-template)]
            [message message]
            ;in here the actual content should be stored
            [inside-main 'temp]
            [answer-type 'normal-html]
            [data "ERROR"])
           (cond
             ;stewards page
             [(equal? page "stewards")
              (let* ([rooms (send master~ get-all-rooms)]
                     [stewards  (send master~ get-stewards)])
                (set! inside-main (include-template "templates/steward-table.html"))
                inside-main)]
             
             ;devices page
             [(equal? page "devices")
              (let* ([stewards  (send master~ get-stewards)]
                     [devices 
                      (map (lambda (steward)
                             (cons steward (send steward get-devices)))
                           stewards)])
                (set! inside-main (include-template "templates/devices.html")))]
             
             ;actions page
             [(equal? page "actions")
              (let* ([actions  (send master~ get-actions)]
                     [device-ids 
                      (foldl append '()
                             (map (lambda (steward)
                                    (map 
                                     (lambda (device)
                                       (get-field id~ device))
                                     (send steward get-devices)))
                                  (send master~ get-stewards)))]
                     [equalities (send action$ get-equality-list)])
                (set! inside-main (include-template "templates/actions.html")))]
             
             
             [(equal? page "deleteAction")
              (let* ([bindings (request-bindings requests)]
                     [action-id (string->number (extract-binding/single 'id bindings))])
                (send master~ delete-action action-id)
                (get-inside-main-loop "actions"
                                                     head
                                                     heading
                                                     '("Succesfully deleted action")
                                                     inside-main
                                                     answer-type
                                                     data))]
             
             [(equal? page "handleAddSteward")
              (let* ([bindings (request-bindings requests)]
                     [place (extract-binding/single 'stewardplace bindings)]
                     [port (extract-binding/single 'stewardport bindings)]
                     [ip (extract-binding/single 'stewardip bindings)])
                (send master~ add-steward ip (string->number port) place)
                (set! response (get-inside-main-loop "stewards"
                                                     head
                                                     heading
                                                     (list (string-append "Succesfully added steward on ip: '" 
                                                                    ip
                                                                    ":"
                                                                    port
                                                                    "'")) 
                                                     inside-main
                                                     answer-type
                                                     data)))]
             
             
             [(equal? page "handleAddAction")
              (let* ([bindings (request-bindings requests)]
                     [type (extract-binding/single 'actiontype bindings)]
                     [value (extract-binding/single 'actionvalue bindings)]
                     [command (extract-binding/single 'actioncommand bindings)]
                     [eq (extract-binding/single 'actionequality bindings)]
                     [ddevid (extract-binding/single 'actiondestinationdevice bindings)]
                     [sdevid (extract-binding/single 'actionsourcedevice bindings)]
                     [action-o ((send action$ create-lambda)
                                type value ddevid sdevid command eq -1)])     
                (send master~ add-action action-o)
                (get-inside-main-loop "actions"
                                                     head
                                                     heading
                                                     '("Succesfully added action")
                                                     inside-main
                                                     answer-type
                                                     data))]
             
             [(equal? page "data")
              (let* ([room_tuple (map 
                                  (lambda (steward)
                                    (cons 
                                     (get-field steward-id~ steward)
                                     (get-field place~ steward)))
                                  (send master~ get-stewards))]
                     [amount_data (send master~ get-facts 'amount-data)]
                     [number_stewards (length room_tuple)]
                     [number_devices (send master~ get-facts 'amount-devices)]
                     [last_stored_data (send master~ get-facts 'last-stored-data)]
                     )
                (set! inside-main (include-template "templates/data-start.html")))]
             
             [(equal? page "getDeviceStatus")
              (set! answer-type 'simple-data)
              (let* 
                  ([device-id (extract-binding/single 'id (request-bindings requests))]
                   [steward (with-handlers ((exn:fail? (lambda (e)
                                                         'off)))
                              (send master~ get-steward-for-device device-id))]
                   [data-t (if (eq? 'off steward)
                               "Steward offline"
                               (send steward get-device-status device-id))])
                (if (string? data-t)
                    (set! data data-t)
                    (set! data (foldl (lambda (s s2) (string-append s s2)) ""
                                      (map 
                                       (lambda (t-d) (send t-d get-nice-string))
                                       (send steward get-device-status device-id))))))]
             
             [(equal? page "getStewardDevices")
              (set! answer-type 'simple-data)
              (let* 
                  ([s-id (extract-binding/single 'id (request-bindings requests))]
                   [steward (send master~ get-steward (string->number s-id))]
                   [data-t (send steward get-devices-count)])
                (set! data (if (number? data-t) (number->string data-t) data-t)))]
             
             [(equal? page "sendMessageToDevice")
              (set! answer-type 'simple-data)
              (let* 
                  ([device-id (extract-binding/single 'id (request-bindings requests))]
                   [message (extract-binding/single 'mes (request-bindings requests))]
                   [steward (send master~ get-steward-for-device device-id)]
                   [data-t (send steward send-message-to-device device-id message)])
                (set! data data-t))]
             
             [(equal? page "data_whole_system") ; bug with minute data of diffrent hours is shown
              (let* ([unparsed-data (send master~ get-data 'all)]
                     [current-time-diff (string->symbol (extract-binding/single 'time_diff (request-bindings requests)))]
                     [json-data (if (null? unparsed-data) 
                                    ""
                                    (send parser$ unparse-to-json unparsed-data current-time-diff))]
                     [time-diffs '((minute . "Minutes") (hour . "Hours") (day . "Days") (month . "Months") (year . "Years"))]
                     [current-page page]
                     [jquery "/js/jquery.min.js"]
                     [flot-js "/js/jquery.flot.min.js"])
                (set! inside-main (include-template "templates/graph.html")))]
             
             
             ;standard page
             [else
              (set! inside-main help-text)])
           
           
           (exit (cond 
                   
                   [(eq? answer-type 'simple-data) 
                    (response/full
                     200 #"Okay"
                     (current-seconds) TEXT/HTML-MIME-TYPE
                     empty
                     (list (string->bytes/utf-8 data)))]
                   
                   
                   [else (let* ((main (include-template "templates/main.html"))
                                (footer (include-template "templates/footer.html"))
                                (menu (include-template "templates/menu.html"))
                                (body (include-template "templates/body.html")))
                           (response/full
                            200 #"Okay"
                            (current-seconds) TEXT/HTML-MIME-TYPE
                            empty
                            (list (string->bytes/utf-8 (include-template "templates/home.html")))))]))))))
      
      (define/private (render-head-template)
        (let (
              [title (get-field title SETTINGS)]
              [scripts '("/js/jquery.min.js")]
              [stylesheets '("/style.css")];must be in the same directory
              [favicon "/images/favicon.png"]
              [author (get-field author SETTINGS)]
              [description (list "Home " "Automation " "System " "beschrijving")])
          (include-template "templates/head.html")))
      
      (define/private (render-heading-template)
        (let ([head-text "Control your house"]
              [home-link "home"])
          (include-template "templates/heading.html")))
      
      ;the list of bindings is constructed as follows
      ;'( (binding-name . binding-value) (next-binding-name . next-binding-value) ... )
      ;eg: '((page . home) (
      (define/private (extract-page bindings)
        (let 
            loop-over-bindings
          ([remaining bindings])
          (cond [(empty? remaining)
                 home-page]
                [(eq? (caar remaining) 'page)
                 (cdar remaining)]
                [else (loop-over-bindings (cdr remaining))])))
      
      (define/override (start)
        (serve/servlet dispatcher
                       #:extra-files-paths (list (current-directory))))))
  
  
  
  