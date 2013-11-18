#lang racket
;---------------------------------------------------------------------
;|
;|    start.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    starts the whole thing
;|
;---------------------------------------------------------------------

(require "master.rkt"
         "front-end.rkt")

(define master (new master%))
(send master init)

(define front-end (new html-front-end% [master~ master]))
(send front-end start)