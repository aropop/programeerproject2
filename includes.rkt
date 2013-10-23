#lang racket

(#%provide (all-defined))
(require "macros.rkt"
         "master.rkt"
         )


(define racket-files 
  (find-files (lambda (x) 
                (let* ([str (path->string x)]
                       [l (string-length str)])
                  (and 
                   (> l 4)
                   (equal? ".rkt" (substring str (- l 4) l)))))
              (string->path "./")))


  
