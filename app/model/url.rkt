#lang racket


;; TODO: Research the possibility of saving only the hash of each url and using it
;; in the synchronization process. Also saving the last seen date so we can revisit
;; some urls after a period.

(require "../lib/persistence.rkt")

(provide (struct-out url)
         url-table
         url<?)

(struct url (address) #:transparent) 

;; columns :  -> (listof string)
;; columns in the DB.
(define (url-table)
  (db-table "url" '(("address" . "text PRIMARY KEY"))))
 
;; vector->url : vector -> url
(define (vector->url url-vector) 
  (url (vector-ref url-vector 0)))

;; url<? : url url -> boolean
(define (url<? a-url another-url) 
  (string<? (url-address a-url)
            (url-address another-url)))

