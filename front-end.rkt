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
         "parser.rkt")

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
      (list '("home" . "Home") '("stewards" . "Stewards") '("devices" . "Devices") '("data" ."Data")))
    (define home-page 'home)
    
    ;dispatches between diffrent requests (read pages)
    ;you can optionally add any messages 
    (define (dispatcher requests . message)
      (display (request-bindings requests)) (newline)
      (let get-inside-main-loop
        (;handle the "static" elements of the site
         [page (extract-page (request-bindings requests))]
         [head (render-head-template)]
         [heading (render-heading-template)]
         [message message]
         ;in here the actual content should be stored
         [inside-main 'temp]
         )
        (cond
          ;stewards page
          [(equal? page "stewards")
           (let* ([rooms (send master~ get-all-rooms)]
                  [stewards  (send master~ get-stewards)])
             (set! inside-main (include-template "templates/steward-table.html"))
             )
           ]
          ;devices page
          [(equal? page "devices")
           (let* ([stewards  (send master~ get-stewards)]
                  [devices 
                   (map (lambda (steward)
                          (cons steward (send steward get-devices)))
                        stewards)])
             (set! inside-main (include-template "templates/devices.html")))]
          
          [(equal? page "handleAddSteward")
             (let* ([bindings (request-bindings requests)]
                   [place (extract-binding/single 'stewardplace bindings)]
                   [port (extract-binding/single 'stewardport bindings)]
                   [ip (extract-binding/single 'stewardip bindings)])
               (send master~ add-steward ip (string->number port) place)
               (get-inside-main-loop "stewards"
                                     head
                                     heading
                                     (string-append "Succesfully added steward on ip: '" 
                                                    ip
                                                    ":"
                                                    port
                                                    "'") 
                                     inside-main))]
          
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
          
          [(equal? page "data_whole_system") ; bug with minute data of diffrent hours is shown
           (let* ([unparsed-data (send master~ get-data 'all)]
                  [current-time-diff (string->symbol (extract-binding/single 'time_diff (request-bindings requests)))]
                  [json-data (send (new parser%) unparse-to-json unparsed-data current-time-diff)]                  
                  [time-diffs '((minute . "Minutes") (hour . "Hours") (day . "Days") (month . "Months") (year . "Years"))]
                  [current-page page]
                  [jquery "/js/jquery.min.js"]
                  [flot-js "/js/jquery.flot.min.js"]
                  )
             (set! inside-main (include-template "templates/graph.html")))]
          
          
          ;standard page
          [else
           (set! inside-main "Welkom<br>Features to add: <ul><li>Device delete</li><li>graphs</li><li>Better interface</li></ul>")
           ]
          )
        (let* ((main (include-template "templates/main.html"))
               (body (include-template "templates/body.html")))
          (response/full
           200 #"Okay"
           (current-seconds) TEXT/HTML-MIME-TYPE
           empty
           (list (string->bytes/utf-8 (include-template "templates/home.html")))))
        )
      )
    
    (define/private (render-head-template)
      (let (
            [title (get-field title SETTINGS)]
            [scripts '()]
            [stylesheets '("/style.css")];must be in the same directory
            [favicon "/images/favicon.png"]
            [author (get-field author SETTINGS)]
            [description (list "Home " "Automation " "System " "beschrijving")]
            )
        (include-template "templates/head.html")
        )
      )
    
    (define/private (render-heading-template)
      (let (
            [head-text "Control your house"]
            [home-link "home"]
            [menu (include-template "templates/menu.html")] ;menu-items are defined as a private field of this class
            )
        (include-template "templates/heading.html")
        )
      )
    
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
                     #:extra-files-paths (list (current-directory)
                                              ))
      )
    
    
    
    )
  )



