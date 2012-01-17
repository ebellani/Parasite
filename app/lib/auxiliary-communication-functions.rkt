#lang racket

;; Some of the symbols in html and xml conflict with
;; each other and with racket/base language, so we prefix
;; to avoid namespace conflict.
(require (prefix-in h: html)
         (prefix-in x: xml)
         net/uri-codec
         net/url)

(provide input-port->html
         list-for-url-enconded)


;; a search-result is a structure that contains all info about a search
;; hit fetched from a search engine. Not very useful as it is, but
;; it is here on the hunch that all that info could be useful in the future.
;; (struct search-result (visible-url url title sample-content)
;;         #:transparent)

;; input-port->html : input-port -> html
(define (input-port->html input-port) 
  (h:read-html (get-pure-port input-port)))

;; extract-pcdata: html-content -> string
;; Pulls out the pcdata strings from some-content.
(define (extract-pcdata some-content)
  (cond [(x:pcdata? some-content) (x:pcdata-string some-content)]
        [(x:entity? some-content) ""]
        [else (extract-pcdata-from-element some-content)]))

;; extract-pcdata-from-element: html-element -> string
;; Pulls out the pcdata strings from an-html-element.
(define (extract-pcdata-from-element an-html-element)
  (match an-html-element
    [(struct h:html-full (attributes content))
     (apply string-append (map extract-pcdata content))]
    [(struct h:html-element (attributes)) ""]))


(define-syntax list-for-url-enconded
  (syntax-rules ()
    [(_ first-symbol ...)
     (remove* (list (void))
              `(,(when (not (zero? (string-length first-symbol)))
                    `(first-symbol . ,first-symbol))
               ...))]))
