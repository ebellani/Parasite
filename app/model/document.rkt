#lang racket

(require "../lib/logger.rkt"
         "downloader.rkt")

(provide document-parser
         document-downloader)

;; document-parser : (input-port -> (listof X)) string -> (input-port -> (listof X))
;; returns a function that receives a input-port representing a document from the
;; web and uses the parser function to return a list of elements found in that doc.
(define (document-parser parser-function (audit-name "generic doc.")) 
  (λ (result-input-port) 
     (if (input-port? result-input-port)
         (let ([results (parser-function result-input-port)])
           (log-when-enabled
            (format "the ~a parser found ~a hits."
                    audit-name (length results)))
           (register-instrumentation-data (list
                                           (format 
                                            "total ~a parsed." audit-name)
                                           (λ (total) (+ total (length results)))
                                           0))
           (register-instrumentation-data (list
                                           (format
                                            "total web documents searched for ~a."
                                            audit-name)
                                           (λ (total) (add1 total))
                                           0))
           (register-instrumentation-data (list
                                           (format
                                            "Mean of ~a in documents."
                                            audit-name)
                                           (λ (total)
                                              (exact->inexact
                                               (/ (+ total (length results)) 2)))
                                           0))
           
           results)
         empty)))


;; document-downloader : string -> (string -> input-port)
;; Returns a function that receives an url and downloads it.
(define (document-downloader audit-name) 
  (λ (the-url)
     (log-when-enabled
      (format "the ~a downloader received the URL ~a"
              audit-name the-url))
     (register-named-time-of "Download time of web documents."
                             (download the-url 10)))) 

