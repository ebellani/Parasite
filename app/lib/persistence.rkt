#lang typed/racket/no-check

;; Used to talk to some sort of persistence layer. Most of the function here
;; cause side effects. 

(require (planet schematics/spgsql:2:3/spgsql) 
         "../lib/logger.rkt"
         unstable/struct
         "../config/environment.rkt")

(provide (all-from-out (planet schematics/spgsql:2:3/spgsql))
         disconnect-from-db!
         (struct-out db-table)
         get-current-db-connection
         representation->columns
         maybe-select-row-from
         exact-conditions
         maybe-delete-from!
         format-values
         set-database!
         maybe-drop-table!
         representation-values->column-value-pair
         maybe-create-table!
         random-select-from 
         maybe-insert-into!)

(define CURRENT-DB-NAME        (get-default-db)) 
(define CONNECTION             #f)

;; represents a table, There is a name and a list of pairs of strings
;; representing the columns. For example:
;; (table "stuff"
;;        '(("base" . "integer PRIMARY KEY")
;;          ("name" . "text")))
(struct db-table (name columns) #:transparent) 

;; get-current-db-connection :  -> connection
;; singleton for the db connection. 
(define (get-current-db-connection) 
  (unless (and  CONNECTION
                (send CONNECTION connected?)) 
    (set! CONNECTION
          (let ([connection-data (database-connection CURRENT-DB-NAME)])
            (connect #:server    (hash-ref connection-data 'server)
                     #:port      (hash-ref connection-data 'port)
                     #:database  (hash-ref connection-data 'database)
                     #:user      (hash-ref connection-data 'user)
                     #:password  (hash-ref connection-data 'password)))))
  CONNECTION) 



;; set-database! : string -> void
(define (set-database! table-name) 
  (set! CURRENT-DB-NAME table-name))

;; disconnect! :  -> void
(define (disconnect-from-db!)
  (when (get-current-db-connection)
    (send (get-current-db-connection) disconnect)))
 
;; maybe-create-table! : table -> void or false
;; build a database table from a table struct.
(define (maybe-create-table! a-table)
  (with-handlers
      ([exn? (λ (exception)
                (log-when-enabled
                 (format "Error creating the ~a table. -> ~a"
                         a-table
                         (exn-message exception))
                 #:level 'error)
                #f)])
    (send (get-current-db-connection)
          exec
          (format "CREATE TABLE ~a ~a" (db-table-name a-table) 
                  (format-values (db-table-columns a-table)
                                 ", "
                                 (λ (column-info)
                                    (string-append (car column-info) " "
                                                   (cdr column-info))))))))

;; maybe-drop-table! : table -> void or false
(define (maybe-drop-table! table) 
  (with-handlers
      ([exn? (λ (exception)
                (log-when-enabled
                 (format "Error dropping the ~a table -> ~a"
                         (db-table-name table)
                         (exn-message exception))
                 #:level 'error)
                #f)])
    (send (get-current-db-connection) exec 
          (format "DROP TABLE ~a" (db-table-name table))))) 

;; format-values : (list string) -> (list string)
(define (format-values the-values separator value-alteration-function) 
  (string-append "("
                 (string-join (map value-alteration-function the-values)
                              separator)
                 ")"))

;; maybe-insert-into! : table (listof X) -> void or false
;; tries to insert a list of values into a given table. Use the format-values
;; to format them for insertion. The list of values must be ordered in the same
;; way as the table.
(define (maybe-insert-into! table the-values) 
  (with-handlers
      ([exn? (λ (exception)
                (log-when-enabled
                 (format "Error inserting into table ~a -> ~a"
                         (db-table-name table)
                         (exn-message exception))
                 #:level 'debug)
                #f)])
    (send (get-current-db-connection) exec 
          (format "INSERT INTO ~a VALUES ~a"
                  (db-table-name table) 
                  (format-values the-values
                                 ", "
                                 (λ (val) (quote-value val)))))))


;; representation-values->column-value-pair : (listof (pairof string)) (listof X) -> (listof (pairof string X))
;; Takes the columns out of the representation and associates it with values
(define (representation-values->column-value-pair rep val) 
  (define (acc-column-value-pairs representation values acc)
    (if (empty? values)
        acc
        (acc-column-value-pairs (rest representation)
                                (rest values)
                                (cons (cons (car (car representation))
                                            (car values))
                                      acc))))
  (reverse (acc-column-value-pairs (db-table-columns rep) val empty))) 

;; representation->columns : (listof (pairof string . string)) -> (listof string)
;; returns the columns of a given table representation.
(define (representation->columns representation) 
  (map (λ (name-type-pair)
          (car name-type-pair))
       (db-table-columns representation)))


;; maybe-delete-from! : table (listof X) -> void or false
;; tries to delete everything that has the values passed.
(define (maybe-delete-from! table the-values) 
  (with-handlers
      ([exn? (λ (exception)
                (log-when-enabled
                 (format "Error deleting the values ~a from table ~a -> ~a"
                         the-values
                         (db-table-name table)
                         (exn-message exception))
                 #:level 'error)
                #f)]) 
    (send (get-current-db-connection) exec 
          (format "DELETE FROM ~a WHERE ~a"
                  (db-table-name table) 
                  (exact-conditions
                   (representation-values->column-value-pair table
                                                             the-values)))))) 

;; quote-value : string -> string
(define (quote-value the-value) 
  (format "'~a'" the-value))

;; exact-conditions : (listof (pairof string X)) -> string
(define (exact-conditions conditions) 
  (format-values conditions " AND "
                 (λ (name-value-pair)
                    (format "~a=~a"
                            (car name-value-pair)
                            (quote-value (cdr name-value-pair))))))

;; random-select-from : table integer -> (listof (vectorof X)) 
;; fetches a random sample of a table to be used as seed.
(define (random-select-from table limit) 
  (with-handlers ([exn? (λ (exception)
                           (log-when-enabled
                            (format "Error while fetching ~a seeds -> ~a"
                                    (db-table-name table)
                                    (exn-message exception))
                            #:level 'error))])
    (Recordset-data (send (get-current-db-connection) query 
                          (format "SELECT * FROM ~a ORDER BY RANDOM() LIMIT ~a"
                                  (db-table-name table) limit)))))


;; maybe-select-row-from : string (listof (pairof string string)) -> false or (vectorof X) 
;; tries to select a row from the DB. 
(define (maybe-select-row-from table the-values) 
  (with-handlers
      ([exn? (λ (exception)
                (log-when-enabled
                 (format "Error selecting the values ~a from table ~a -> ~a"
                         the-values
                         (db-table-name table)
                         (exn-message exception))
                 #:level 'error)
                #f)])
    (send (get-current-db-connection) query-maybe-row 
          (format "SELECT * FROM ~a WHERE ~a"
                  (db-table-name table)
                  (exact-conditions
                   (representation-values->column-value-pair table
                                                             the-values)))))) 

