#lang racket

(require ffi/unsafe
         "logger.rkt")

(provide resolve-name)

(define libtadns (ffi-lib "../lib/tadns/libtadns.so")) 

;; a function type for the callback that will be passed to the tadns library
(define _tadns-callback
  (_fun _pointer -> _void))

;; functions from the TADSN library 
(define callback_data_error_code
  (get-ffi-obj "callback_data_error_code"
               libtadns (_fun _pointer -> _int))) 
(define callback_data_name
  (get-ffi-obj "callback_data_name"
               libtadns (_fun _pointer -> _string))) 
(define dns-queue
  (get-ffi-obj "resolve"
               libtadns (_fun _string _pointer -> _int))) 
(define callback_data_address
  (get-ffi-obj "callback_data_address"
               libtadns (_fun _pointer _string -> _string)))

 ;; get-resolver-callback : thread -> (cpointer -> void)
;; returns a function that will be passed as a callback to the TADNS lib.
;; the function will in turn check the callback data for its contents and
;; send them to the original target thread.
(define (get-resolver-callback target-thread)
  (λ (cb-data)
     (case (callback_data_error_code cb-data)
       [(0) (thread-send target-thread
                         (callback_data_address cb-data ""))]
       [(1) (thread-send  target-thread
                          (exn:fail
                           (format "Query timeout for ~a"
                                   (callback_data_name cb-data))
                           (current-continuation-marks)))]  
       [(2) (thread-send target-thread
                         (exn:fail
                          (format "No such address ~a"
                                  (callback_data_name cb-data))
                          (current-continuation-marks)))]  
       [(3) (thread-send
             target-thread
             (exn:fail (format "System error occurred while resolving a name"
                               (callback_data_name cb-data))
                       (current-continuation-marks)))])))

;; resolve-name : string thread -> void
;; tries to resolve a name. Sends the result to the target thread.
(define (resolve-name a-name target-thread)
  (call-in-nested-thread
   (λ ()
      (dns-queue a-name
                 (function-ptr (get-resolver-callback target-thread)
                               _tadns-callback)))))

