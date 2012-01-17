#lang racket 

;; Library that implements Microsoft Bing Seach API 
;; Based on the php library by Cal Evans at
;; http://bingphp.codeplex.com and documentation from
;; http://msdn.microsoft.com/en-us/library/dd250846.aspx

(require net/uri-codec
         "../model/search-engine.rkt"
         "../lib/auxiliary-communication-functions.rkt"
         "url.rkt" 
         "../lib/logger.rkt")

(provide bing%)

(define bing%
  (class* search-engine% ()
    (super-new)

    (define/override (maximum-results)
      100)
    
    (define/override (base-url)
      "http://api.bing.net/json.aspx?Sources=Web&")
    
    (define/override (name)
      "Bing")

    (define/override (build-request
                      Query 
                      #:key
                      [AppId "43D11764368B4993229310204823335D926243B5"]
                      #:language            [Market               "pt-BR"]
                      #:start-in            [WebRequest.Offset    "0"]
                      #:version             [Version              "2.2"]
                      #:results-per-request [WebRequest.Count     "100"]
                      #:file-types          [file-types           empty])
      (let ([WebRequest.FileType
             (if (not (empty? file-types)) 
                 (string-join file-types " ")
                   "")])
        (string-append (base-url) 
                       (alist->form-urlencoded
                        (list-for-url-enconded Query
                                               AppId
                                               Market
                                               WebRequest.Offset
                                               Version
                                               WebRequest.Count
                                               WebRequest.FileType)))))

    (define/override (valid-response? a-jsexpr)
      (not (false?
            (hash-ref (hash-ref a-jsexpr 'SearchResponse) 'Errors (λ () #t)))))
    
    (define/override (json->urls a-jsexpr)
      (with-handlers
          ([exn?
            (λ (exception) 
               (log-when-enabled
                (format "~a error for response ~a \n ~a"
                        (name) a-jsexpr (exn-message exception))
                #:level 'error)
               empty)])
        (let ([results (hash-ref (hash-ref (hash-ref a-jsexpr 'SearchResponse)
                                           'Web)
                                 'Results
                                 empty)])
          (map (λ (a-result)
                  (url (hash-ref a-result 'Url)))
               results))))))
