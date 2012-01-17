#lang racket

(require "../auxiliary.rkt"
         (test me)
         (planet dherman/json:3:0)
         (test "model" "url")
         (test "model" "search-engine")
         (test "lib" "logger"))

(define-test-suite url-suite
  (let ([google-se (new google%)])
    
    (check-equal? (send google-se build-request "with key"
                        #:key "MY KEY"
                        #:results-per-request "1")
                  "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=with+key&key=MY+KEY&hl=pt-BR&gl=BR&start=0&rsz=1")
    (check-equal? (send google-se build-request"for a filetype"
                        #:file-types '("txt"))
                  "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=for+a+filetype+filetype%3Atxt&hl=pt-BR&gl=BR&start=0&rsz=large")
    (check-equal? (send google-se build-request "for several filetypes"
                        #:file-types '("txt" "xls"))
                  "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=for+several+filetypes+filetype%3Atxt+OR+filetype%3Axls&hl=pt-BR&gl=BR&start=0&rsz=large")
    ;; %3A means ':' 
    (check-equal?  (send google-se build-request "paris hilton"
                         #:results-per-request "1")
                   "http://ajax.googleapis.com/ajax/services/search/web?v=1.0&q=paris+hilton&hl=pt-BR&gl=BR&start=0&rsz=1"))) 

(define-test-suite search-engine-suite
  (let ([test-ip (open-input-string "\r\n {\"responseData\"")]
        [test-google-engine (new google%)]
        [input-sample
         (open-input-file (build-path  current-fixture-path  
                                       "google-response.json"))])

    (check-equal? (port->string (clean-input-port test-ip))
                  "{\"responseData\"")

    ;; parsing google's response.
    (check-equal? ((search-engine-parser test-google-engine)
                   (list (read-json input-sample)))
                  (list (url "http://en.wikipedia.org/wiki/Paris_Hilton")))


    (check-equal? (length ((search-engine-downloader test-google-engine 16)
                            "Um teste"))
                  2)))



(run-tests url-suite) 
(run-tests search-engine-suite) 
