#lang racket

(require "../auxiliary.rkt"
         (test "model" "cnpj")
         (test "model" "url")
         racket/sandbox
         (test me)
         (test "lib" "logger"))

(define-test-suite search-engine-group-suite
  (call-with-killing-threads
   (λ ()
      (let ([group-manager
             (search-engine-group-start (current-thread))])
        (thread-send group-manager (list (cnpj "20060829112541"))) 
        (sleep 4)
        (check-not-false (thread-try-receive))))))


(define-test-suite document-group-suite
  (group-test
   document-group-start
   (list (url "http://www.mte.gov.br/delegacias/se/se_sind_lista.pdf"))))

(define (group-test starting-function seeds)
  (call-with-killing-threads
   (λ ()
      (check-false (thread-try-receive))
      (let ([group-manager
             (starting-function (current-thread))])
        (thread-send group-manager seeds) 
        (sleep 4)
        (check-not-false (thread-try-receive))))))


;;(install-log-printer 'info)
(run-tests search-engine-group-suite)
(run-tests document-group-suite) 
