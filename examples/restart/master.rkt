#lang racket/base
(require (except-in "../../racloud.rkt" main)
         racket/class
         racket/place)

(provide wait-place-thunk)
(provide main)

(define (wait-place-thunk)
  (place ch
    (printf "BEGINING SLEEP\n")
    (sleep 5)
    (printf "SLEEP DONE\n")))

(define (main)
  (master-event-loop
    (supervise-place-thunk-at "localhost" #:listen-port 6345 (get-current-module-path) 'wait-place-thunk #:restart-on-exit #t)))
