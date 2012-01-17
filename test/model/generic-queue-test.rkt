#lang racket

(require rackunit
         rackunit/text-ui
         "../../app/model/generic-queue.rkt")

(define-test-suite macros-suite
  (let ([a-queue '()])
    (check-equal?
     (begin 
       (into-queue! a-queue (list 1))
       a-queue)
     '(1))
    (check-equal?
     (begin 
       (into-queue! a-queue 'x)
       a-queue)
     '(x 1))
    (check-equal? (next-from-queue! a-queue)
                  'x)
    (check-equal? (next-from-queue! a-queue)
                  '1)
    (check-equal? (next-from-queue! a-queue)
                  empty)
    (check-equal? (next-from-queue! a-queue)
                  empty)))


(run-tests macros-suite)

