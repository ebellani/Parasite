#lang racket

(require racket/runtime-path)

(provide LOGGING-ON
         database-connection
         get-default-db 
         logs-directory-path
         toogle-logging!)

(define LOGGING-ON           (make-parameter #t)) ;; more messages 
(define INSTRUMENTATION-ON   (make-parameter #f)) ;; starts logging 
(define PROFILING-ON         (make-parameter #f)) ;; should start 

;; toogle-logging! :  -> void
(define (toogle-logging!)
  (toogle LOGGING-ON))


(define-runtime-path logs-directory-path "../../logs/")

;; toogle-instrumenting! :  -> void
(define (toogle-instrumenting!)
  (toogle INSTRUMENTATION-ON)) 

;; toogle : parameter -> void
;; toogles between true and false a given parameter
(define (toogle a-param) 
  (if (a-param)
      (a-param #f)
      (a-param #t)))
 
(define (get-default-db) "crawler-development") 

;; database-connection : string -> hash
(define (database-connection base-name)
  (make-immutable-hash `((server   . "localhost")
                         (port     . 5432)
                         (database . ,base-name)
                         (user     . "b-man")
                         (password . " "))))
