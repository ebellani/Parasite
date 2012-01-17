#lang racket

(require "../auxiliary.rkt"
         (test "lib" "persistence")
         (test me))

(define-test-suite generic-suite
  (let ([the-cnpj (cnpj "01123321123401")]
        [cnpj-string-representations (list "01123321/1234-01"  
                                           "01123321/123401"  
                                           "01.123.321/1234-01"
                                           "01.123.3211234-01" 
                                           "01.123.321123401"  
                                           "011233211234-01"   
                                           "01123321123401")])
    (for-each (λ (representation)
                 (check-equal? (string->cnpj representation)
                               the-cnpj))
              cnpj-string-representations)
    (check-equal? (cnpj->formatted-string the-cnpj) "01.123.321/1234-01"))

  (check-equal? (input-port->cnpjs (open-input-string "01123321123401Preg\365es2006"))
                (list (cnpj "01123321123401")))
  
  (check-true (cnpj<? (cnpj "1234")
                      (cnpj "5678")))

  (with-fixture-data (cnpj-table)
                     (check-equal? (cnpj-random-seeds 10)
                                   (list (cnpj "1234")))
                     (cnpj "1234")))

(run-tests generic-suite)
