#lang racket

(require rackunit
         "../app/config/environment.rkt"
         "../app/lib/persistence.rkt"
         "../app/lib/generic-auxiliaries.rkt"
         unstable/struct
         racket/runtime-path
         racket/require-syntax
         rackunit/text-ui)

(provide (all-from-out rackunit)
         (all-from-out rackunit/text-ui)
         clean-message-box
         with-fixture-data
         current-fixture-path
         with-test-table
         test)


;; work with the temporary database.
(set-database! "crawler-test") 

(define-runtime-path current-fixture-path "data/")

(define-syntax with-test-table
  (syntax-rules ()
    [(with-test-table table expr ...)
     (begin
       (maybe-create-table! table)
       expr ...
       (maybe-drop-table!   table))]))

(define-syntax with-fixture-data
  (syntax-rules ()
    [(with-fixture-data table body datum ...)
     (begin
       (for-each (λ (a-datum) 
                    (maybe-insert-into! table
                                        (struct->list a-datum)))
                 (list datum ...))
       body
       (for-each (λ (a-datum)
                    (maybe-delete-from! table (struct->list a-datum)))
                 (list datum ...)))]))

(define (clean-message-box (a-thread (current-thread)))
  (while (not (false? (thread-try-receive)))))


;; new-test-tables! : string (listof string) -> 
;; Freshes up tables to test persistence in some models.
;;(define (new-test-tables table-name representations) 
;;  (with-handlers ([exn? (λ (x)
;;                           (void))])
;;    (drop-tables!  (list table-name (get-temporary-table-for table-name))))
;;  (with-handlers ([exn? (λ (x) (void))])
;;    (create-tables! representations)))

(define-require-syntax (test stx)
  (let*-values ([(base filename must-be-dir?)
                 (split-path
                  (resolved-module-path-name
                   (current-module-declare-name)))]
                [(type-base type-path must-be-dir?) (split-path base)] 
                [(resolved-name type)
                 (values (path->string filename)
                         (path->string type-path))]
                [(cleaned-name)
                 (substring resolved-name
                            0 (- (string-length resolved-name) 9))]
                [(syntax-for-name)
                 (λ (name (the-type type))
                    (datum->syntax stx
                                   (string-append "../../app/"
                                                  the-type
                                                  "/"
                                                  name 
                                                  ".rkt")))])
    (syntax-case stx (me) 
      [(test me) (syntax-for-name cleaned-name)]
      [(test relpath) (syntax-for-name (syntax-e #'relpath))]
      [(test alternative-type relpath)
       (syntax-for-name (syntax-e #'relpath)
                        (syntax-e #'alternative-type))])))

;; First, you need to use define-require-syntax:
;;  
;;   (define-require-syntax from-model ___)
;;  
;; Next, since you want to compute the new path at compile time, you can't use syntax-rules. If you do, then you'll return the string-append expression as if it were a require form; but it isn't, and you'll get an error.
;;  

;;  
;; Finally, you want to append the given string to a common prefix:
;;  
;;   (string-append "../app/model/" (syntax-e #'relpath))
;;  
;; But that's a string, and your require macro must return a syntax object. And since require is essentially a non-hygienic binding form that binds names based on the lexical context of the require subforms, you get the lexical context right. The right lexical context in this case is the same as the original require form, stx:
;;  
;;   (datum->syntax stx
;;                  (string-append "../app/model/" (syntax-e #'relpath)))
;;  
;; Then use it thus:
;;  
;;   (require (from-model "some-file.rkt")
;;            (from-model "another-file.rkt"))
;;  
;; Ryan 
