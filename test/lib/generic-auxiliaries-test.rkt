#lang racket

(require "../auxiliary.rkt"
         (test me))

(define-test-suite manager-thread-suite
  (let ([the-manager-thread #f])
    
    (thread-send (current-thread)
                 (thread (λ ()
                            (check-equal? (thread-try-receive)
                                          "test"))))
    (with-a-manager-thread
     the-manager-thread
     (thread-send the-manager-thread "test"))
    (sleep 0.1)))

(run-tests manager-thread-suite)
