 #lang racket

(require "../auxiliary.rkt"
         racket/sandbox
         (test "lib" "persistence")
         (test me))

(define test-table (db-table "testurl" '(("address" . "text"))))
 

(define-test-suite resource-suite
  (call-with-killing-threads
   (λ () 
      (with-test-table
       test-table
       (let ([test-url1 '("http://www.1.com")]
             [test-url2 '("http://www.2.com")]
             [test-url3 '("http://www.3.com")]
             [test-url4 '("http://www.4.com")]
             [url-resource-controller
              (make-resource-controller-thread (λ (x y)
                                                  (string<? (car x)
                                                            (car y)))
                                               test-table
                                               (λ (x) x)
                                               #:max-length 2
                                               #:timeout    1)]) 
         ;; send the manager as a message. Used because while integrating the crawler
         ;; we can have mutually recursive agents and letrec does not deal well with this
         (thread-send url-resource-controller (current-thread))

         (thread-send url-resource-controller (list test-url1
                                                    test-url1
                                                    test-url2))   
         (sleep 0.1)
         (check-false (thread-try-receive))
 
         (thread-send url-resource-controller (list test-url3))
         (sleep 0.1)
         (check-equal? (thread-try-receive)
                       (list test-url2
                             test-url1
                             test-url3))
         (thread-send url-resource-controller (list test-url4))
         (sleep 2)
         (check-equal? (thread-try-receive) (list test-url4)))))))
 
(run-tests resource-suite) 
