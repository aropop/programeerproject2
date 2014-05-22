#lang racket

(provide database-saveable<%>)

(define 
  database-saveable<%>
  (interface ()
    store-sql
    create-lambda
    get-sql))