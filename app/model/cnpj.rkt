#lang racket

;; The CNPJ has the configuration XX.XXX.XXX/XXXX-XX, where the first 7 digits 
;; are the base, the 8th is the verification digit base 10 of the base,
;; the following 4 digits the number of the order of
;; affiliated societies of the company the 13th is the verification digit
;; of last 12 digits and the 14th is the verification digit of the last
;; 13 digits.

;; TODO : REMOVE THE PARSING AND DOWNLOADING TO A DOCUMENT MODULE.

(require "../lib/persistence.rkt"
         "../lib/generic-auxiliaries.rkt"
         "../lib/logger.rkt"
         unstable/struct)

(provide (struct-out cnpj)
         load-cnpjs-from-file!
         cnpj->formatted-string
         cnpj-random-seeds
         input-port->cnpjs
         generate-cnpj-samples
         cnpj<?
         string->cnpj
         cnpj-table)


(struct cnpj (code) #:transparent)

(define PATTERN
  (pregexp "\\d{2}\\.?\\d{3}\\.?\\d{3}/?\\d{4}-?\\d{2}"))

(define PATTERN-FOR-EXTRANEOUS-CHARS
  (pregexp "\\.|\\/|\\-"))

;; input-port->cnpjs : input-port pregexp -> (listof string)
;; returns all occurences of the pattern inside a text.
(define (input-port->cnpjs web-doc (pattern PATTERN)) 
  (map (λ (a-cnpj)
          (string->cnpj (bytes->string/utf-8 a-cnpj)))
       (regexp-match* pattern web-doc)))


;; string->cnpj : string -> cnpj
(define (string->cnpj string-representation)
  (cnpj (regexp-replace* PATTERN-FOR-EXTRANEOUS-CHARS
                                      string-representation
                                      "")))

;; cnpj->formatted-string : cnpj -> string
(define (cnpj->formatted-string a-cnpj)
  (let ([code (cnpj-code a-cnpj)])
    (format "~a.~a.~a/~a-~a"
            (substring code 0 2)
            (substring code 2 5)
            (substring code 5 8)
            (substring code 8 12)
            (substring code 12 14))))

;; vector->cnpj : vector -> cnpj
(define (vector->cnpj cnpj-vector) 
  (cnpj (vector-ref cnpj-vector 0)))

;; columns :  -> (listof string)
;; columns in the DB.
(define (cnpj-table)
  (db-table "cnpj" '(("code" . "text PRIMARY KEY"))))

;; generate-cnpj-samples :  -> (listof string)
;; returns the strings from 0001-00 to 0001-99
(define (generate-cnpj-samples)
  (let acc-samples ([i 0]
                    [return empty])
    (let ([stringified-number (number->string i)])
      (if (<= i 99)
          (acc-samples (add1 i)
                       (cons (string-append "0001-"
                                            (if (<= i 9)
                                                (string-append "0"
                                                               stringified-number)
                                                stringified-number))
                             return))
          return))))

;; cnpj<? : cnpj -> boolean
(define (cnpj<? a-cnpj another-cnpj)
  (string<? (cnpj-code a-cnpj)
            (cnpj-code another-cnpj)))

;; load-cnpjs-from-file! : string -> void
;; takes a file containing a cnpj by line and dumps it into the DB.
(define (load-cnpjs-from-file! file)
  (let ([file-input-port (open-input-file file)])
    (let loop ([line (read-line file-input-port)]) 
      (unless (eof-object? line)
        (maybe-insert-into! (cnpj-table) (list line))
        (loop (read-line file-input-port))))))
 
;; cnpj-random-seeds : integer -> (listof cnpj)
(define (cnpj-random-seeds how-many) 
  (map (λ (a-cnpj-vector)
          (string->cnpj (vector-ref a-cnpj-vector 0))) 
       (random-select-from (cnpj-table) how-many)))

