#lang racket

(require "../lib/generic-auxiliaries.rkt")

(provide new-worker-thread)

;; new-worker-thread : thread (thread X -> ) (X -> X) number -> thread
;; generic function to build threads that loop fetching something
;; from its message queue, processing that something and sending the result of
;; this operation to a target thread. If it encounters any exception with
;; its action, sends the exception to the manager thread.
(define (new-worker-thread target-thread action (nap-interval 0.2))
  (thread
   (λ ()
      (let ([manager-thread (thread-receive)])
        (with-a-manager-thread
         manager-thread
         (while #t
           (let ([received-message (thread-receive)]
                 [result           #f])
             (with-handlers
                 ([exn? (λ (exception)
                           (thread-send manager-thread exception))])
               (set! result (action received-message))
               (thread-send target-thread result)
               (sleep nap-interval)))))))))

