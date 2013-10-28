#lang racket
(require db)
(require "settings.rkt")
(require "db-table-data.rkt")
(require "macros.rkt")
(#%provide database-manager%)

(define database-manager%
  (class object%
    (super-new)
    
    (define*
      [con~ (sqlite3-connect #:database (get-field db-path SETTINGS))]
      [initialized~ #f]
      [install-query~ 
       (list 
"CREATE TABLE Room (
name VARCHAR(30) PRIMARY KEY
);"

"CREATE TABLE Data (
data_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
date TEXT DEFAULT CURRENT_TIMESTAMP,
type VARCHAR(20) NOT NULL,
value TEXT NOT NULL
);"

"CREATE TABLE Steward (
steward_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
room_name VARCHAR(30) REFERENCES Room(name) ON UPDATE CASCADE ON DELETE SET NULL
);"

"CREATE TABLE Device (
device_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
type VARCHAR(20) NOT NULL,
steward_id INT REFERENCES Steward(steward_id) ON UPDATE RESTRICT ON DELETE CASCADE
);" )]
      )
    
    
    (define/public (execute/no-return sql)
      (if initialized~
          (query-exec con~ sql)
          (begin (do-init-tests)
                 (execute/no-return sql))
          )
      )
    
    (define/public (execute/return sql)
      (if initialized~
          (new db-table-data% 
               [query-result (query-rows con~ sql)])
          (begin (do-init-tests)
                 (execute/return sql))
          )
      )
    
    (define/private (do-init-tests)
      (if (not (is-table-installed?))
          (begin
            (set! initialized~ #t)
            (map (lambda (install-query)
                   (execute/no-return install-query))
                 install-query~))
          (set! initialized~ #t))
      )
    
    (define/private (is-table-installed?)
      (if (and       
           (sqlite3-available?)
           (connection? con~)
           (connected? con~))
          (and
           (table-exists? con~ "Steward")
           (table-exists? con~ "Room")
           (table-exists? con~ "Device")
           (table-exists? con~ "Data")
           )
          (error "Database error, no connection!"))
      )
    
    
    )
  )