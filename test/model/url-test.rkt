#lang racket

(require "../auxiliary.rkt"
         (test "lib" "persistence")
         (test me))

(define-test-suite generic-suite
  (check-equal? (url-table)
                (db-table "url" '(("address" . "text PRIMARY KEY"))))

  (check-true (url<? (url "I am smaller")
                     (url "ZZZZ"))))
 
(run-tests generic-suite)
