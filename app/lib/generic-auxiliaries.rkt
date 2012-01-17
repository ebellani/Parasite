#lang racket

(require mzlib/defmacro)

(provide while
         increment!
         with-a-manager-thread)
 
(define-syntax with-a-manager-thread
  (syntax-rules ()
    [(with-a-manager-thread manager-thread body ...)
     (begin
       (while (not (thread? manager-thread))
         (set! manager-thread (thread-receive)))
       body ...)]))

(define-syntax while
  (syntax-rules ()
    [(while predicate expr ...)
     (let loop ()
       (when predicate
         expr ... (loop)))]))

(define-syntax increment!
  (syntax-rules ()
    [(increment variable)
     (set! variable (add1 variable))]
    [(increment variable amount)
     (set! variable (+ variable amount))])) 

