#lang racket/base
(require racket/match 
         "../../define-named-remote-server.rkt")

(define-named-remote-server                                                                                         
 tuple-server
                                                                                                              
  (define-state h (make-hash))                                                                              
  (define-rpc (set k v)
    (hash-set! h k v)
    v)
  (define-rpc (get k)
    (hash-ref h k #f)))
