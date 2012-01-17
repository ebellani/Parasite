#lang racket

(require "../auxiliary.rkt"
         (test me))

(define-test-suite thread-suite
  (let* ([test-thread (new-worker-thread (current-thread)
                                         (λ (result) (add1 result))
                                         0.01)])
    (clean-message-box)
    (thread-send test-thread (current-thread))
    (thread-send test-thread 1)
    (thread-send test-thread 2)
    (thread-send test-thread 3)
    (sleep 0.5)
    (kill-thread test-thread)

    (check-equal? (thread-try-receive) 2)
    (check-equal? (thread-try-receive) 3)
    (check-equal? (thread-try-receive) 4)
    (check-false (thread-try-receive))))

(define-test-suite exception-suite
  (let* ([test-thread (new-worker-thread (current-thread)
                                         (λ (result) (add1 result))
                                         0.01)])
    (clean-message-box) 
    (thread-send test-thread (current-thread))
    (thread-send test-thread 1)
    (thread-send test-thread "error")
    (sleep 0.5)
    (kill-thread test-thread)

    (check-equal? (thread-try-receive) 2)
    (check-true (exn? (thread-try-receive)))
    (check-false (thread-try-receive))
    ))


(run-tests thread-suite)
(run-tests exception-suite) 
