#lang racket

(require "../auxiliary.rkt"
         (test me))

(define-test-suite resolver-suite
  (clean-message-box)
  (let ([ct (current-thread)])
    (check-false (thread-try-receive))
    (call-in-nested-thread
     (λ () (resolve-name "digg.com" ct)))
    (resolve-name "slashdot.org" ct)
    (sleep 0.2)
    (check-equal? (thread-try-receive)
                  "64.191.203.30")
    (check-equal? (thread-try-receive)
                  "216.34.181.45")
    (resolve-name "asdkljh.oc1" ct)
    (check-true (exn:fail?
                 (thread-try-receive)))
    (check-false (thread-try-receive))))

(run-tests resolver-suite)
