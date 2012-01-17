#lang racket

(require "../auxiliary.rkt"
         (test me)
         (planet dherman/json:3:0)
         (test "model" "url")
         (test "model" "search-engine")
         (test "lib" "logger"))

(define-test-suite url-suite
  (let ([bing-se (new bing%)])
    
    (check-equal? (send bing-se build-request "with key"
                        #:key "MY KEY"
                        #:results-per-request "1")
                  "http://api.bing.net/json.aspx?Sources=Web&Query=with+key&AppId=MY+KEY&Market=pt-BR&WebRequest.Offset=0&Version=2.2&WebRequest.Count=1")
    (check-equal? (send bing-se build-request"for a filetype"
                        #:file-types '("txt"))
                  "http://api.bing.net/json.aspx?Sources=Web&Query=for+a+filetype&AppId=43D11764368B4993229310204823335D926243B5&Market=pt-BR&WebRequest.Offset=0&Version=2.2&WebRequest.Count=100&WebRequest.FileType=txt")
    (check-equal? (send bing-se build-request "for several filetypes"
                        #:file-types '("txt" "xls"))
                  "http://api.bing.net/json.aspx?Sources=Web&Query=for+several+filetypes&AppId=43D11764368B4993229310204823335D926243B5&Market=pt-BR&WebRequest.Offset=0&Version=2.2&WebRequest.Count=100&WebRequest.FileType=txt+xls")))


(define-test-suite search-engine-suite
  (let ([test-bing-engine (new bing%)]
        [input-sample
         (open-input-file (build-path  current-fixture-path  
                                       "bing-response.json"))])

    ;; parsing bing's response.
    (check-equal? (first ((search-engine-parser test-bing-engine)
                          (list (read-json input-sample))))
                  (url "http://pt.wikipedia.org/wiki/Paris_Hilton"))

    (check-equal?
     ((search-engine-parser test-bing-engine)
      (list
       '#hasheq((SearchResponse
                 .
                 #hasheq((Query . #hasheq((SearchTerms . 69.636.131/1630-14)))
                         (Version . 2.2)
                         (Web . #hasheq((Total . 0) (Offset . 0))))))))
     empty)
    
    (check-equal? (length ((search-engine-downloader test-bing-engine 40)
                            "Um teste"))
                  1)
 
    (check-equal? (length ((search-engine-downloader test-bing-engine 160)
                            "Um teste"))
                  2)))

;;(run-tests url-suite)
(run-tests search-engine-suite)
