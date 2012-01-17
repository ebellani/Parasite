#lang racket

(require "../auxiliary.rkt"
         racket/sandbox
         (test me))
 
(define-test-suite logger-suite
  (let ([string-out (open-output-string "")])
    (install-log-printer 'info string-out)
    (log-when-enabled "ERROR!!!!" #:level 'debug  #:data 'WEEEE)
    (log-when-enabled "a test message for the logger" #:data  '(a b c))
   
    (sleep 0.1)
    ;; does not show the lower level log
    (check-true  (string? (get-output-string string-out)))))
 
(define-test-suite instrumentation-suite
  (call-with-killing-threads
   (λ ()
      (let ([string-out (open-output-string "")])
        (register-instrumentation-data (list 'already-seen-urls
                                             (λ (seen-urls) (+ seen-urls 2))
                                             0))
        (register-instrumentation-data (list 'already-seen-urls
                                             (λ (seen-urls) (+ seen-urls 20))
                                             0)) 
        
        (register-named-time-of "time spent" (sleep 1))
        (print-instrumentation-data string-out)
        (sleep 1)
        (check-true (string? (get-output-string string-out))))))) 

(run-tests logger-suite) 
(run-tests instrumentation-suite) 
