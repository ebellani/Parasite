#lang racket 
 
(require "../model/cnpj.rkt"
         "../model/url.rkt" 
         "../lib/logger.rkt" 
         "../lib/persistence.rkt")

(provide mount-databases)

(define DATABASES '("crawler-development" "crawler-test"))

(define RACKET-VERSION "5.1")
 
;; system-tables :  -> (listof tables)
(define system-tables
  (list (cnpj-table) (url-table)))

;; mount the databases
;; mount-databases : (listof string) -> void
(define (mount-databases (databases DATABASES)) 
  (for-each (λ (db-name)
               (disconnect-from-db!)
               (set-database! db-name)
               (for-each (λ (a-table)
                            (log-when-enabled
                             (format "For database ~a.\nCreating table ~a"
                                     db-name
                                     a-table))
                            (maybe-create-table! a-table))
                         system-tables)) databases)) 

