#lang racket

;; Wrapper for Google's RESTful API
;; reference found in
;; http://code.google.com/apis/websearch/docs/reference.html#_intro_fonje

(require net/uri-codec
         "../model/search-engine.rkt"
         "../lib/auxiliary-communication-functions.rkt"
         "url.rkt" 
         "../lib/logger.rkt")

(provide google%)


(define google% 
  (class* search-engine% ()
    (super-new)

    (define/override (maximum-results)
      8)
    
    (define/override (base-url)
      "http://ajax.googleapis.com/ajax/services/search/web")
    
    (define/override (name)
      "Google")

    (define/override (build-request query 
                                    #:key                 [key         ""]
                                    #:language            [hl          "pt-BR"]
                                    #:country             [gl          "BR"]
                                    #:start-in            [start       "0"]
                                    #:version             [v           "1.0"]
                                    #:results-per-request [rsz         "large"]
                                    #:file-types          [file-types  empty])
      (let ([q (if (not (empty? file-types)) 
                   (string-append
                    query " "
                    (string-join (map (λ (file-type)
                                         (string-append "filetype:" file-type))
                                      file-types)
                                 " OR "))
                   query)])
        (string-append (base-url) "?"
                       (alist->form-urlencoded
                        (list-for-url-enconded v q key hl gl start rsz)))))

    (define/override (valid-response? a-json) 
      (equal? (hash-ref a-json 'responseStatus) 200))
    
    (define/override (json->urls a-jsexpr)
      (with-handlers
          ([exn?
            (λ (exception) 
               (log-when-enabled
                (format "~a error for response ~a."
                        (name) a-jsexpr)
                #:level 'error)
               empty)])
        (let ([results (hash-ref (hash-ref a-jsexpr 'responseData)
                                 'results
                                 empty)])
          (map (λ (a-result)
                  (url (hash-ref a-result 'unescapedUrl)))
               results)))))) 
