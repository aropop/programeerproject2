#lang racket

;---------------------------------------------------------------------
;|
;|    Database-manager.rkt 
;|    Arno De Witte - Programmeerproject 2
;|    Executes database queries
;|
;---------------------------------------------------------------------


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
      [last-query~ 'none]
      [install-query~ 
       (list 
"CREATE TABLE Room (
name VARCHAR(30) PRIMARY KEY
);"

"CREATE TABLE Data (
data_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
date TEXT DEFAULT CURRENT_TIMESTAMP,
type VARCHAR(20) NOT NULL,
value TEXT NOT NULL,
device_id INT REFERENCES Device(device_id) ON UPDATE CASCADE ON DELETE SET NULL
);"

"CREATE TABLE Steward (
steward_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
room_name VARCHAR(30) REFERENCES Room(name) ON UPDATE CASCADE ON DELETE SET NULL
);"

"CREATE TABLE Device (
device_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
type VARCHAR(20) NOT NULL,
name VARCHAR(35) NOT NULL,
communication_adress VARCHAR(50) NOT NULL,
serial_number INT NOT NULL,
steward_id INT REFERENCES Steward(steward_id) ON UPDATE RESTRICT ON DELETE CASCADE
);" )]
      )
    
    ;Insert, delet and update queries
    (define/public (execute/no-return sql)
      (if initialized~
          ;save last query so we can get additional info 
          (set! last-query~ (query con~ sql)) 
          (begin (do-init-tests)
                 (execute/no-return sql))
          )
      )
    
    ;select queries where you expect a return
    (define/public (execute/return sql)
      (if initialized~
          (new db-table-data% 
               [query-result (query-rows con~ sql)])
          (begin (do-init-tests)
                 (execute/return sql))
          )
      )
    
    ;Returns the last inserted id after an insert operation
    ;More info on simple restult structs http://docs.racket-lang.org/db/query-api.html#%28def._%28%28lib._db%2Fbase..rkt%29._simple-result%29%29
    (define/public (last-inserted-id)
      (if (eq? 'none last-query~)
          (error "No query executed to get last inserted id")
          (cdar (simple-result-info last-query~)))) 
    
    
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