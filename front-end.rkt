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
         (only-in "device.rkt" supported-device-types))

(provide html-front-end%)

(define front-end%
  (class object%
    
    (super-new)
    
    (init-field
     [master~ 'none]
     )
    
    (abstract start
              )
    
    (define/public (get-master)
      master~)
    )
  )

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
           (let* ([stewards  (send master~ get-stewards)])
             (set! inside-main (include-template "templates/steward-table.html"))
             )
           ]
          ;devices page
          [(equal? page "devices")
           (let* ([stewards (send master~ get-stewards)]
                  [rooms (send master~ get-all-rooms)]
                  [devices 
                            (map (lambda (steward)
                                   (cons steward (send steward get-device-list)))
                                 stewards)])
             (set! inside-main (include-template "templates/devices.html")))]
          ;do the add device action
          [(equal? page "handleAddDevice")
           (let* ([bindings (request-bindings requests)]
                  [type (extract-binding/single 'devicetypeinput bindings)]
                  [name (extract-binding/single 'devicenameinput bindings)]
                  [place (extract-binding/single 'deviceplaceinput bindings)]
                  [com-addr (extract-binding/single 'devicecommunicationaddressinput bindings)]
                  [ser-num (extract-binding/single 'deviceserialnumberinput bindings)])
             (send master~ add-device  type name place ser-num com-addr)
             (get-inside-main-loop "devices" head heading (string-append "Succesfully added device '" name "'") inside-main))
           ]
          
          ;standard page
          [else
           (set! inside-main "Welkom...")
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
                     #:extra-files-paths (list (current-directory)))
      )
    
    
    
    )
  )



