#lang racket/base
(require (except-in "../../racloud.rkt" main)
         racket/class
         racket/runtime-path
         "bank.rkt")

(define-runtime-path bank-path "bank.rkt")
(define-runtime-path place-worker-path "place-worker.rkt")
(define-runtime-path process-worker-path "process-worker.rkt")

(define (spawn-place-worker-at port message)
  (supervise-dynamic-place-at "localhost" #:listen-port port place-worker-path 'place-worker #:initial-message message #:restart-on-exit #t))

(define bank-vm (supervise-place-thunk-at "localhost" #:listen-port 6344 bank-path 'make-bank))
(define bank-place (send bank-vm get-first-place))

(master-event-loop
  ;(spawn-place-worker-at 6341 "ONE")
  ;(spawn-place-worker-at 6342 "TWO")
  ;(spawn-place-worker-at 6343 "THREE")
  bank-vm
  ;(every-seconds 3.3 (printf "Hello from every-seconds\n")(flush-output))
  (after-seconds 2
         (displayln (bank-new-account bank-place 'kevin))                                                                  
         (displayln (bank-add bank-place 'kevin 10))                                                                       
         (displayln (bank-removeM bank-place 'kevin 5)))
  #;(supervise-process-at "localhost" #:listen-port 6344 process-worker-path #:restart-on-exit #t))
