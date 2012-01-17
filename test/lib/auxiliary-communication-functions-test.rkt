#lang racket

(require html
         "../auxiliary.rkt"
         net/url
         rackunit
         rackunit/text-ui
         (test me))

(define-test-suite enconding-tests
  (let ([query "madonna"]
        [we "are"]
        [empty-string ""])
    (check-equal? (list-for-url-enconded query)
                  '((query . "madonna")))
    (check-equal? (list-for-url-enconded query empty-string empty-string)
                  '((query . "madonna")))
    (check-equal? (list-for-url-enconded query we)
                  '((query . "madonna") (we . "are")))))

;;(run-tests fetching-tests) 
(run-tests enconding-tests)



