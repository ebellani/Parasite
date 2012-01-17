#lang racket

;; Craw manager.

;; The crawl manager is responsible for receiving the
;; URL input stream from the applications and forwarding it to
;; the available downloaders and DNS resolvers while enforc-
;; ing rules about robot exclusion and crawl speed. A down-
;; loader is a high-performance asynchronous HTTP client ca-
;; pable of downloading hundreds of web pages in parallel,
;; while a DNS resolver is an optimized stub DNS resolver
;; that forwards queries to local DNS servers. [1]

;; [1] Vladislav Shkapenyuk, Torsten Suel, "Design and Implementation
;; of a High-Performance Distributed Web Crawler," Data Engineering,
;; International Conference on, pp. 0357, 18th International Conference
;; on Data Engineering (ICDE'02), 2002.

(require racket/sandbox
         unstable/struct
         "resources.rkt"
         "../model/google.rkt"
         "../model/bing.rkt"
         "../model/downloader.rkt"
         "../model/document.rkt"
         "../model/search-engine.rkt"
         "../model/cnpj.rkt"
         "../model/url.rkt"
         "../model/generic-thread.rkt"
         "../lib/auxiliary-communication-functions.rkt"
         "../lib/logger.rkt"
         "../lib/persistence.rkt"
         "../lib/generic-auxiliaries.rkt")

(provide create-crawler
         search-engine-group-start
         document-group-start
         crawl!
         (all-from-out "../lib/generic-auxiliaries.rkt"))
 
;; create-crawler : (listof string) integer number -> void
;; creates a manager for the crawler. This manager watches the repositories and
;; does something with each of them when they reach their limit. It also connects
;; the document and the search groups, setting their nap time.
(define (create-crawler seeds
                        (repository-max-length 500)
                        (repository-timeout    120))  
  (letrec ([url-resource-controller
            (make-resource-controller-thread url<?
                                             (url-table)
                                             struct->list
                                             #:max-length repository-max-length
                                             #:timeout    repository-timeout)]
           [doc-resource-controller
            (make-resource-controller-thread cnpj<?
                                             (cnpj-table)
                                             struct->list
                                             #:max-length repository-max-length
                                             #:timeout    repository-timeout)]
           [doc-group-manager (document-group-start doc-resource-controller)]
           [se-groups-manager  (search-engine-group-start url-resource-controller)])
    (thread-send url-resource-controller  doc-group-manager)
    (thread-send doc-resource-controller se-groups-manager)
    (thread-send se-groups-manager seeds)))

;; generic-group-start : (X -> any) (X -> (listof X)) (X -> X) (X -> string) number -> thread
;; generic worker group. Check the specific implementations below for a better
;; documentation.
(define (generic-group-start resource-manager
                             parser-function
                             downloader-function
                             manager-message->string
                             #:manager-nap-time    (manager-nap-time    0.01)
                             #:downloader-nap-time (downloader-nap-tipe 0.2)
                             #:parser-nap-time     (parser-nap-tipe     0.2)
                             #:group-name          (group-name          "generic"))
  (let* ([parser     (new-worker-thread resource-manager
                                        parser-function
                                        parser-nap-tipe)]
         [downloader (new-worker-thread parser
                                        downloader-function
                                        downloader-nap-tipe)])
    (thread 
     (λ () 
        (thread-send parser (current-thread))
        (thread-send downloader (current-thread))
        (while #t 
          (sleep manager-nap-time)
          (let ([enqueued-message (thread-receive)])
            (log-when-enabled  (format
                                "The manager of the group ~a received ~a."
                                 group-name enqueued-message)) 
            (when (list? enqueued-message)
              (for-each (λ (message)
                           (thread-send downloader
                                        (manager-message->string message)))
                        enqueued-message)))))))) 

;; document-group-start : thread -> thread 
(define (document-group-start url-resource-controller) 
  (generic-group-start url-resource-controller 
                       (document-parser input-port->cnpjs "cnpj")
                       (document-downloader "cnpj")
                       url-address 
                       #:group-name "document"))

;; search-engine-group-start : thread hash -> thread
;; creates a a group for each search engine passed. The search engines hash has
;; the format -> #hash(search-engine-class . (sleeptime . number of groups))
;;(define-syntax search-engines-start
;;  (syntax-rules ()
;;    [(search-engines-start resource-controller search-engines)
;;     (thread
;;      (λ () ))]))

(define (search-engine-group-start document-resource-controller)
  (thread
   (λ () 
      (let* ([google-se (new google%)]
             [bing-se   (new bing%)]
             [google-group
              (generic-group-start document-resource-controller
                                   (search-engine-parser     google-se)
                                   (search-engine-downloader google-se)
                                   (λ (a-cnpj)
                                      (if (string? a-cnpj)
                                          a-cnpj
                                          (cnpj->formatted-string a-cnpj))) 
                                   #:downloader-nap-time 0.8
                                   #:group-name "Google Search Engine")] 
             [bing-group
              (generic-group-start document-resource-controller
                                   (search-engine-parser     bing-se)
                                   (search-engine-downloader bing-se)
                                   (λ (a-cnpj)
                                      (if (string? a-cnpj)
                                          a-cnpj
                                          (cnpj->formatted-string a-cnpj))) 
                                   #:downloader-nap-time 0.8
                                   #:group-name "Bing Search Engine")])
        (let loop ([message (thread-receive)])
          (for-each (λ (message-piece) 
                       (thread-send bing-group (list message-piece))
                       (thread-send google-group (list message-piece)))
                    message)
          (loop (thread-receive)))))))




;; crawl! : ( -> (listof cnpj)) . -> void
;; executes the crawler for some time with the given parameters. Main function
;; to execute the crawler.
(define (crawl! seeds-getter
                #:duration            (duration            120)
                #:log?                (log?                #f)
                #:report?             (report?             #f) 
                #:repository-max-size (repository-max-size 100)
                #:repository-timeout  (repository-timeout  120)) 
  (call-with-killing-threads
   (λ ()
      (when log? (install-log-printer 'info))
      (create-crawler (seeds-getter)
                      repository-max-size
                      repository-timeout) 
      (sleep duration)
      (when report? (print-instrumentation-data))))) 
