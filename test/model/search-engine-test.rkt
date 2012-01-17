#lang racket

(require "../auxiliary.rkt"
         (test me))

(define-test-suite se-suite
  ;; clean input port
  (let ([test-ip (open-input-string "\r\n {\"responseData\"")])
    (check-equal? (port->string (clean-input-port test-ip))
                  "{\"responseData\""))

  ;; generic parser
  
  )
