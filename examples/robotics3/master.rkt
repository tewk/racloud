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

(define bank-instance (supervise-place-thunk-at "localhost" #:listen-port 6344 bank-path 'make-bank))

(master-event-loop
  ;(spawn-place-worker-at 6341 "ONE")
  ;(spawn-place-worker-at 6342 "TWO")
  ;(spawn-place-worker-at 6343 "THREE")
  bank-instance
  ;(every-seconds 3.3 (printf "Hello from every-seconds\n")(flush-output))
  (after-seconds 2
         (define bi (send bank-instance get-first-place))
         (printf "HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH\n")
         (displayln (bank-new-account bi 'kevin))                                                                  
         (displayln (bank-add bi 'kevin 10))                                                                       
         (displayln (bank-removeM bi 'kevin 5)))
  #;(supervise-process-at "localhost" #:listen-port 6344 process-worker-path #:restart-on-exit #t))
