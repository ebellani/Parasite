#lang racket

(require (planet dherman/json:3:0)
         "../lib/logger.rkt"
         "downloader.rkt")

(provide search-engine-parser
         search-engine-downloader
         search-engine%
         clean-input-port)

;; clean-input-port : input-port -> input-port
;; removes anomalies from the input port. Basically it removes all chars
;; before the '{' that is the first element of a json hash.
(define (clean-input-port the-input-port) 
  (let loop ([ip the-input-port]
             [first-peeked-char (peek-char the-input-port)])
    (if (not (equal? first-peeked-char
                     #\{))
        (loop (begin (read-char ip) ip)
              (peek-char ip))
        ip)))

;; search-engine-parser : search-engine% -> ((list jexpr) -> (listof string))
;; Returns a function that receives an list of json
;; and returns the found urls.
(define (search-engine-parser a-search-engine) 
  (λ (jsons)
     (flatten (map (λ (json)
                      (let ([results (send a-search-engine json->urls json)]) 
                        (log-when-enabled
                         (format "the ~a parser found ~a urls."
                                 (send a-search-engine name)
                                 (length results))) 
                        results))
                   jsons))))

;; search-engine-downloader : search-engine% integer (listof string)
;;   -> (string -> (listof jexpr))
;; Returns a function that receives a query, builds a url for the engine with
;; a custom API builder and downloads the results from the web. This function
;; does a small parsing in the result, to see if the search engine returned
;; a success.
(define (search-engine-downloader a-search-engine 
                                  (maximum-results 100)
                                  (file-types '("pdf" "html" "txt" "doc" "xls"))) 
  (λ (search-query)
     ;; download-until-error : (listof string) -> (listof jexpr)
     ;; stops the downloading of search results if the API signals an error.
     (define (download-until-error requests) 
       (if (empty? requests)
           empty 
           (let* ([response
                   (register-named-time-of
                    (format  "~a engine download time"
                             (send a-search-engine name))
                    (download (first requests)))]
                  [response-json (if (input-port? response)
                                     (read-json (clean-input-port response))
                                     #f)])
             
             (if (and response-json
                      (send a-search-engine valid-response? response-json))
                 (cons response-json (download-until-error (rest requests)))
                 (download-until-error (rest requests))))))
     
     (log-when-enabled
      (format "~a API will receive the query ~a."
              (send a-search-engine name)
              search-query))
     (download-until-error (send a-search-engine
                                 requests-for-query
                                 search-query
                                 maximum-results
                                 file-types))))

(define search-engine-interface
  (interface ()
    name
    requests-for-query
    build-request
    base-url
    json->urls
    valid-response?
    maximum-results))

;; a superclass that has to have some of its methods overriden. They are indicated
;; by the "Override me! return type."
(define search-engine% 
  (class* object% (search-engine-interface)
    (super-new)

    ;; base-url :  -> string
    (define/public (base-url) 
      "Override me!")

    ;; valid-response? : json -> boolean
    ;; verifies if a given json represents an error from the API or not.
    (define/public (valid-response? a-json) 
      "Override me!")

    
    ;; maximum-results : -> integer
    ;; used to indicate the maximum number of urls that a given API can
    ;; return.
    (define/public (maximum-results)
      "Override me!")

    ;; name : -> string
    ;; used mostly for debbuging and logging.
    (define/public (name)
      "Override me!")

    ;; requests-for-query : string integer (listof string) -> (listof string)
    ;; figures out the maximum desired and builds a list of requests that represent
    ;; those results.
    (define/public (requests-for-query query desired-urls (file-types empty)) 
      (for/list ([i (in-range desired-urls)]
                 #:when (zero? (modulo i (maximum-results))))
                (build-request query
                               #:file-types file-types
                               #:start-in   (number->string i))))

    ;; build-request : string . -> string
    ;; builds a string capable of being consumed by a search engine API.
    (define/public (build-request query)
     "Override me!")
    
    
    
    ;; input-port->urls : input-port -> (listof url)
    ;; given an input port that is the response of a search engine API, returns
    ;; the URLs found. 
;;    (define/public (input-port->urls response-input-port)
;;      (json->urls (read-json (clean-input-port response-input-port))))

    ;; json->urls : jsexpr -> (listof url)
    ;; takes the search engine response in a jsexpr representation and parses it
    ;; to build url structs.  
    (define/public (json->urls a-jsexpr)
      "Override me!")))

