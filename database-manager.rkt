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
date DATETIME DEFAULT CURRENT_TIMESTAMP,
type VARCHAR(20) NOT NULL,
value TEXT NOT NULL,
device_id VARCHAR(30) REFERENCES Device(device_id) ON UPDATE CASCADE ON DELETE SET NULL
);"
        
        "CREATE TABLE Steward (
steward_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
ip VARCHAR(15) NOT NULL,
port INT NOT NULL,
room_name VARCHAR(30) REFERENCES Room(name) ON UPDATE CASCADE ON DELETE SET NULL
);"
        
        "CREATE TABLE Action (
action_id INTEGER PRIMARY KEY ASC AUTOINCREMENT,
type VARCHAR(15) NOT NULL,
value VARCHAR(15) NOT NULL,
command VARCHAR(30) NOT NULL,
equality VARCHAR(4) NOT NULL,
destination_device_id VARCHAR(30) NOT NULL,
source_device_id VARCHAR(30) NOT NULL
);"
        
        "CREATE TABLE Device (
device_id VARCHAR(30) PRIMARY KEY,
type VARCHAR(20) NOT NULL,
communication_address VARCHAR(50),
steward_id INTEGER REFERENCES Steward(steward_id) ON UPDATE RESTRICT ON DELETE CASCADE
);" 
        (when (not (null? (get-field standard-rooms SETTINGS)))
          (let ((room-query "INSERT INTO Room")
                (bool #t))
            (for-each (lambda (room) 
                        (if bool
                            (begin 
                              (set! bool #f)
                              (set! room-query (string-append room-query " SELECT '" room "' AS name ")))
                            (set! room-query (string-append room-query  "UNION SELECT ('" room "') ")))) 
                      (get-field standard-rooms SETTINGS))
            (string-append (substring room-query 0 (- (string-length room-query) 1)) ";"))))])
    
    ;Insert, delet and update queries
    (define/public (execute/no-return sql)
      (if initialized~
          ;save last query so we can get additional info 
          (with-handlers ((exn:fail:sql? (lambda (e)
                                           (display "Failed to execute (with no return)")
                                           (newline)
                                           (display "SQL: '") (display sql) (display "'")
                                           (newline)
                                           (display "State: ") (display (exn:fail:sql-sqlstate e))
                                           (newline)
                                           (display "Info: ") (newline)
                                           (map displayln (exn:fail:sql-info e))
                                           (raise "Quit"))))
            (set! last-query~ (query con~ sql))) 
          (begin (do-init-tests)
                 (execute/no-return sql))))
    
    ;select queries where you expect a return
    (define/public (execute/return sql)
      (if initialized~
          (with-handlers ((exn:fail:sql? (lambda (e)
                                           (display "Failed to execute (with return)")
                                           (newline)
                                           (display "SQL: '") (display sql) (display "'")
                                           (newline)
                                           (display "State: ") (display (exn:fail:sql-sqlstate e))
                                           (newline)
                                           (display "Info: ") (newline)
                                           (map displayln (exn:fail:sql-info e))
                                           (raise "Quit"))))
            (new db-table-data% 
                 [query-result (query-rows con~ sql)]))
          (begin (do-init-tests)
                 (execute/return sql))))
    
    ;Returns the last inserted id after an insert operation
    ;More info on simple restult structs 
    ;http://docs.racket-lang.org/db/query-api.html#%28def._%28%28lib._db%2Fbase..rkt%29._simple-result%29%29
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
          (set! initialized~ #t)))
    
    (define/private (is-table-installed?)
      (if (and       
           (sqlite3-available?)
           (connection? con~)
           (connected? con~))
          (and
           (table-exists? con~ "Steward")
           (table-exists? con~ "Room")
           (table-exists? con~ "Device")
           (table-exists? con~ "Data"))
          (error "Database error, no connection!")))))