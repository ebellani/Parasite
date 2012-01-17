#lang racket

(require "../auxiliary.rkt" (test me))



(define-test-suite sql-suite
  (let ([stuff-representation (db-table "stuff"
                                     '(("base" . "integer PRIMARY KEY")
                                       ("name" . "text")))]) 
    (check-equal? (representation->columns stuff-representation)
                  '("base" "name"))
    (check-equal? (representation-values->column-value-pair stuff-representation
                                                            '(999 "Chuck"))
                  '(("base" . 999)
                    ("name" . "Chuck")))
    (check-not-false (maybe-create-table! stuff-representation)) 

    (check-not-false (maybe-insert-into! stuff-representation '(123 "Earl")))
    (check-false     (maybe-insert-into!  stuff-representation'(123 "Chuck"))) 
    (check-equal?    (random-select-from stuff-representation 10)
                     '(#(123 "Earl")))
    (check-not-false (maybe-select-row-from stuff-representation '(123)))
    (check-equal?    (maybe-select-row-from stuff-representation '(123))
                     #(123 "Earl"))
    (check-false     (maybe-select-row-from stuff-representation '(999 "Earl")))
    (check-not-false (maybe-delete-from! stuff-representation '(123 "Earl"))) 
    (check-equal?    (format-values '("testing" "123" "something")
                                    ", " (λ (x) (string-append "'" x "'")))
                     "('testing', '123', 'something')")
 
    (check-not-false (maybe-drop-table! stuff-representation))
    (check-false (maybe-drop-table! (db-table "lakjdhf" empty)))))

;;(define-test-suite sync-suite
;;  (let ([test1 (a-test "fred" 51044170)]
;;        [test2 (a-test "fred" 51044171)]
;;        [test3 (a-test "fred" 51044172)])
;;    (around (begin 
;;              (set-database! "test.db")
;;              (with-handlers ([exn? (λ (x) (void))])
;;                (create-tables! (list (create-table-string "atest"
;;                                                           '(("name" "text")
;;                                                             ("code" "int")))
;;                                      (create-table-string "temp_atest"
;;                                                           '(("name" "text")
;;                                                             ("code" "int")))))))
;;            (check-equal? (synchronize-repository-and-return-intersection!
;;                           (list test1 test2) "atest")
;;                          (list #("fred" 51044170) #("fred" 51044171))) 
;;            (check-equal? (synchronize-repository-and-return-intersection!
;;                           (list test1 test2) "atest")
;;                          empty)
;;            (check-equal? (synchronize-repository-and-return-intersection!
;;                           (list test3) "atest")
;;                          (list #("fred" 51044172))) 
;;            (check-equal? (synchronize-repository-and-return-intersection!
;;                           (list test3) "atest")
;;                          empty)
;; 
;;            (check-equal?schematics/spgsql:2:3/spgsql
;;             (select (get-current-db-connection) "SELECT * FROM temp_atest")
;;             empty)
;; 
;;            (with-handlers ([exn? (λ (x) (void))])
;;              (drop-tables! '("atest" "temp_atest"))))))


(run-tests sql-suite) 

