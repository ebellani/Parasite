#lang racket

(require "../lib/generic-auxiliaries.rkt" 
         (prefix-in rbt: (planet krhari/pfds:1:5/redblacktrees))
         "../lib/logger.rkt"
         "../lib/persistence.rkt")

(provide make-resource-controller-thread)

;; make-resource-controller-thread : (X X -> boolean) table (any -> (listof any)) integer number string -> thread
;; creates a thread that watches over a redblacktree of resources and
;; syncs it somewhere when a given size limit or timer is reached.
;; This function returns a thread. The first element you send to it should be
;; a target thread to where it should respond to. All the other elements
;; should be lists of elements that can be compared using the comparer passed
;; to it. 
(define (make-resource-controller-thread comparer
                                         a-table
                                         resource->list
                                         #:max-length (max-length 100)
                                         #:timeout    (timeout    100)
                                         #:debug-name (debug-name "generic")) 
  (thread
   (λ ()
      (define target-thread (thread-receive))
      (let start-fresh-tree ([resource-tree    (rbt:redblacktree comparer)]
                             [elements-in-tree 0]
                             [timer            (current-seconds)]) 
        (with-a-manager-thread
         target-thread
         (let receive-message ([resources (thread-try-receive)]) 
           (when resources
             (set! resource-tree
                   (foldl (λ (resource tree)
                             (if (rbt:member? resource tree)
                                 tree
                                 (begin
                                   (increment! elements-in-tree)
                                   (rbt:insert resource tree))))
                          resource-tree resources))) 
           (if (or (> elements-in-tree max-length)
                   (>= (- (current-seconds) timer) timeout)) 
               (begin
                 (thread-send target-thread
                                   (intersect-tree-with-db! resource-tree
                                                            a-table
                                                            resource->list)) 
                      (start-fresh-tree (rbt:redblacktree comparer)
                                        0 (current-seconds)))
               (receive-message (thread-try-receive))))))))) 

;; intersect-tree-with-db! : redblacktree table (any -> (listof any)) -> (listof X)
;; checks each element of the tree for existence in the DB. If they are remove
;; from the tree. If they are not, they are a new element so they are inserted
;; into the DB and returned as a list.
(define (intersect-tree-with-db! resource-tree a-table resource->list) 
  (rbt:redblacktree->list
   (rbt:filter
    (λ (resource)
       (with-handlers ([exn?
                        (λ (exception)
                           (log-when-enabled
                            (format "Error saving the resource ~a into table ~a -> ~a"
                                    resource
                                    (db-table-name a-table) 
                                    (exn-message exception))
                            #:level 'error)
                           #f)])
         (register-named-time-of
          (format "Seconds spent intersecting and persisting ~a"
                  (db-table-name a-table)) 
          (not (false? (maybe-insert-into! a-table
                                           (resource->list resource)))))))
    resource-tree)))
