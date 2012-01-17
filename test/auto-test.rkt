#lang racket

(provide main)

;; run all the tests on the system 
(define (enter-all-from-root (pause-time 2))
  (define test-dirs-white-list '("controller.*" "model.*" "lib.*"))
  (define module-white-list    (pregexp ".*\\.rkt$"))
  (define testing-output-port  (current-error-port))
  (define printing-black-list  '(".*usr.*"
                                 ".*planet.*"))

;;  (current-error-port  (open-output-string ""))
  (for ((module-path (in-directory)))
       (with-handlers ([exn? (lambda (any-exn)
                               (fprintf testing-output-port
                                        "~a: ~a\n"
                                        module-path
                                        (exn-message any-exn)))])
         (when (and (ormap (λ (dir-pattern)
                              (regexp-match? dir-pattern module-path))
                           test-dirs-white-list)
                    (regexp-match? module-white-list (path->string module-path)))
           (eval `(enter! ,(path->string module-path))) 
;;           (for-each (λ (line) 
;;                        (unless (or (equal? line "")
;;                                    (ormap (λ (pattern)
;;                                              (regexp-match? pattern line))
;;                                           printing-black-list))
;;                         (fprintf testing-output-port line)))
;;                     (port->lines (open-input-string
;;                                   (get-output-string (current-error-port)))))
;;           
           (sleep pause-time)))))
 
;; start :  ->
(define (main (nap-interval 10))
  (printf "Starting tests.\n")
  (enter-all-from-root)
  (sleep nap-interval)
  (main)) 
