#lang racket

;; Generic downloader

(require "../lib/auxiliary-communication-functions.rkt"
         unstable/port
         (planet bzlib/http:1:0)
         "../lib/generic-auxiliaries.rkt"
         "../lib/logger.rkt"
         net/url
         net/ftp)

(provide download)


;; download : string number -> input-port
;; This sends to a target thread a document from the web, or sends
;; false if there is a  timeout. Uses 2 thread to achieve the timeout
;; process.
;; 2000 pings of 0.001 secs each = 2 seconds default time.
;; There is a difference between the number of pings, that influences how
;; many times it verifies if the download is complete, and the time amount
;; of each ping, that is how much it will sleep before verifing again.
;; This can throw all the usual exceptions from get-pure-port. 
;; Makes the HTTP connection, retrieves the HTTP-header and if it is 200 OK
;; proceeds to download the document. 
(define (download an-url (timeout 2) (nap-interval 0.01))
  (let* ([header-white-list (regexp-quote "200 ok" #f)]
         [fetcher
          (thread
           (λ ()
              (let ([watcher-thread (thread-receive)])
                (with-handlers
                    ([exn?
                      (λ (exception) 
                         (log-when-enabled
                          (format
                           "the downloader found this exception while trying to fetch the document in this URL ~a." 
                           exception)
                          #:level 'error)
                         (thread-send watcher-thread #f))]) 
                  (let* ([response (http-get an-url)]
                         [response-input-port
                          (http-client-response-input response)])
                    (thread-send watcher-thread 
                                 (if (equal?
                                      (http-client-response-code response) 404)
                                     #f
                                     response-input-port)))))))]
         [message-for-watcher #f]
         [timeout-pings       0]) 
    (call-in-nested-thread
     (λ ()
        (thread-send fetcher (current-thread))
        (let loop ([cs (current-seconds)]
                   [message-for-watcher (thread-try-receive)])
          (if (and (< (- (current-seconds) cs) timeout)
                     (false? message-for-watcher))
              (begin
                (sleep nap-interval)
                (loop cs (thread-try-receive)))
              (begin
                (kill-thread fetcher)
                message-for-watcher)))))))


