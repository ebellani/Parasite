#lang racket

(provide into-queue! next-from-queue!)

(define-syntax into-queue!
  (syntax-rules ()
    [(_ queue element)
     (set! queue (if (list? element)
                     (append element queue)
                     (cons element queue)))]
    [(_ queue element ...)
     (set! queue (append (list element ...) queue))]))

;; next! : -> string
;; removes and returns the first element from the queue.
(define-syntax next-from-queue!
  (syntax-rules ()
    [(_ queue)
     (if (empty? queue)
         empty
         (let ([next (first queue)])
           (set! queue (rest queue))
           next))]))
