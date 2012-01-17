#lang racket

(require racket/date
         "generic-auxiliaries.rkt"
         "../config/environment.rkt")

(provide install-log-printer
         register-datum
         register-named-time-of
         log-when-enabled
         instrumentation
         print-instrumentation-data
         register-instrumentation-data)


(date-display-format 'iso-8601)

;; install-log-printer : symbol string -> thread
;; makes a log receiver that captures each log made to the current logger and prints
;; it to some output-port, by default the current one.
(define (install-log-printer level (output-file-path
                                    (base-output-file-path "log-"))) 
  (let* ([out (open-output-file output-file-path #:exists 'append)]
         [r (make-log-receiver (current-logger) level)])
    (thread
     (λ ()
        (while #t
          (match (sync r)
            [(vector l m v)
             (fprintf out ";; [~a] ~a ~a\n" l m (if v v ""))
             (flush-output out)]))))))

;; log-when-enabled : string symbol any -> void
;; logs a message and a data if logging is enabled. 
(define (log-when-enabled message
                          #:level (level 'info)
                          #:data  (data #f)) 
  (when (LOGGING-ON)
    (log-message (current-logger)
                 level
                 (format "At ~a ~a" (date->string (current-date) #t) message)
                 data)))

;; base-output-file-path : string -> string
(define (base-output-file-path file-prefix) 
  (string-append (path->string logs-directory-path)
                 file-prefix
                 (get-default-db)
                 "-"
                 (date->string (current-date) #t)))


(define INSTRUMENTATION #f)

;; instrumentation :  -> thread
;; a singleton pattern for the crawl instrumentation data. Messages should be sent
;; to the thread in a list like this -> (key (any . -> . any) any).
(define (instrumentation) 
  (unless (and (thread? INSTRUMENTATION)
               (not (thread-dead? INSTRUMENTATION)))
    (set! INSTRUMENTATION
          (thread
           (λ ()
              (let ([instrumentation-data (make-hash)])
                (let loop ([message (thread-receive)])
                  (if (output-port? message)
                      (hash-for-each instrumentation-data
                                     (print-key-value-pair message))
                      (hash-update! instrumentation-data
                                    (first  message)
                                    (second message)
                                    (third  message)))
                  (loop (thread-receive))))))))
  INSTRUMENTATION)

;; register-instrumentation-data : (key (any . -> . any) any) -> void 
(define (register-instrumentation-data datum) 
  (thread-send (instrumentation) datum))

;; register-datum : string -> void
;; used when you just want to register a single entry into the report.
(define (register-datum datum) 
  (register-instrumentation-data (list datum (λ _ empty) empty)))
 
;; print-key-value-pair : output-port -> void
(define (print-key-value-pair out) 
  (λ (key value)
     (fprintf out "-- [~a] ~a\n" key value)
     (flush-output out)))

;; print-instrumentation-data : output-port -> void
(define (print-instrumentation-data (output-file-path
                                     (base-output-file-path "report-")))
  (let ([out (open-output-file output-file-path #:exists 'append)]) 
    (thread-send (instrumentation) out)))

(define-syntax register-named-time-of
  (syntax-rules ()
    [(register-named-time-of key expr)
     (let ([old-time (current-seconds)]
           [result   expr])
       (register-instrumentation-data
        (list key
              (λ (total)
                 (+ total (- (current-seconds) old-time)))
              0))
       result)]))
