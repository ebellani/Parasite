#lang racket

;; (require rackunit
;;          rackunit/text-ui
;;          "../../app/model/document.rkt")
;;  
;; (define-test-suite cpf-tests
;;   (test-suite "pattern recognition"
;;               (check-true (not (empty? (match-cpf "000.000.000-22"))))
;;  
;;               (check-true (not (empty? (match-cpf "000000.000-22"))))
;;  
;;               (check-true (not (empty? (match-cpf "000.000000-22"))))
;;  
;;               (check-true (not (empty? (match-cpf "000.000.00022"))))
;;  
;;               (check-true (not (empty? (match-cpf "00000000022"))))
;;               
;;               ;; a letter instead of a zero
;;               (check-true (empty? (match-cpf "000.000.00O-22")))
;;  
;;               (check-true
;;                (not (empty? (match-cpf "000.000.000-22asdfasdf")))))
;;  
;;   (test-suite "struct manipulation"
;;               (let ([mock-cpf (cpf "04838407" "9" "3" "9")])
;;                 (check-equal? (cpf->punctuated-string mock-cpf)
;;                               "048.384.079-39")
;;                 (check-equal? (cpf->string-representations mock-cpf)
;;                               '("048.384.079-39"
;;                                 "04838407939"))
;;                 (check-equal? (raw-string->cpf "04838407939")
;;                               mock-cpf)
;;                 (check-equal? (string->dotted-string "04838407")
;;                               "048.384.07"))))
;;  
;; (run-tests cpf-tests)

