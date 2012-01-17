#lang racket

(require "../auxiliary.rkt"
         (test me)
         net/ftp
         net/url)

;; these tests will use the network for now
(define-test-suite timeout-suite 
  (check-false
   (download
    "http://intranet.itajai.sc.gov.br/arquivos/compras_de_agosto_de_2008.txt" 
    0))
  

  (check-true
   (input-port?
    (download
     "http://intranet.itajai.sc.gov.br/arquivos/compras_de_agosto_de_2008.txt")))) 

(define-test-suite error-suite 
  (check-false (download "garbagesdasdf")) 
  (check-false (download "http://www.garbage123128fasdff.com/")) 
  ;; should return false for anything but a 200 OK . 
  (check-false (download "http://www.101zenstories.com/404"))

  (thread-send (current-thread) "testing for things in the thread pool")
  (check-not-equal? (download "garbagesdasdf")
                    "testing for things in the thread pool")) 

(define-test-suite ftp-suite
;;  ftp://ftp.rncan.gc.ca/ess/geochem/files/raw_data/pkg_0075a.xls
  (let ([conn (ftp-establish-connection "ftp.rncan.gc.ca/" 21
                                         "anonymous" "")])

    (ftp-cd conn "/ess/geochem/files/raw_data/")

    (ftp-download-file conn current-fixture-path "pkg_0075a.xls")

    
    
    ))


;;(run-tests error-suite) 
;;(run-tests timeout-suite)
(run-tests ftp-suite)
