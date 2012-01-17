#!/usr/bin/racket
#lang racket

(require racket/cmdline
         "../app/lib/logger.rkt"
         "../app/config/boot.rkt"
         "../app/config/environment.rkt" 
         "../app/model/cnpj.rkt"
         "../app/controller/crawlers.rkt")

(define log-mode           #f)
(define report-mode        #f) 
(define duration           "120")
(define seed-function      generate-cnpj-samples)

;;(define search-engine-downloder-number   (make-parameter 1))
;;(define search-engine-parser-number      (make-parameter 1))
;;(define document-parser-number           (make-parameter 1))
;;(define document-parser-number           (make-parameter 1))

 
(define begin-crawl
  (command-line
   #:program "parasite crawler"
   #:once-each
   [("-l" "--log-mode")
    "Run crawler with verbose logging."
    (set! log-mode #t)]
   [("-d" "--duration") duration-time
    "Number of seconds of the duration of the crawl."
    (set! duration duration-time)]
   [("-r" "--report-mode")
    "Ends the crawl printing a report of the internal state of the crawler."
    (set! report-mode #t)]
   [("-c" "--create-default-tables") "Create the system's default tables."
    (mount-databases)]
   [("--with-cnpjs-in-file") seed-file
    "Pass a file containing an initial cnpj dump to use as a starting point."
    (load-cnpjs-from-file! seed-file)]
   #:once-any 
   [("--cnpj-seeds-number") how-many-seeds
    "Number of random documents fetched from the DB to search the web."
    (set! seed-function (λ () (cnpj-random-seeds how-many-seeds)))]
   [("--auto-cnpj-seed")
    "Produces a list of simple cnpj seeds from 0001-00 ~ 0001-99 to use as a start"
    (set! seed-function generate-cnpj-samples)]
   #:args () 
   (begin
     (register-datum
      (format "Command line arguments: ~a"
              (string-join
               (vector->list (current-command-line-arguments)) " "))) 
     (crawl! seed-function
             #:duration    (string->number duration)
             #:log?        log-mode
             #:report?     report-mode))))
