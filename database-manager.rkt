#lang racket
(require db)
(require "settings.rkt")
(require "db-table-data.rkt")
(#%provide database-manager%)

(define database-manager%
  (class object%
    (super-new)
    
    (define con~ (sqlite3-connect #:database (get-field db-path SETTINGS)))
    
    (define/public (execute/no-return sql)
      (query-exec con~ sql))
    
    (define/public (execute/return sql)
      (query-rows con~ sql))
    )
  )