#lang racket/base
(require (except-in "../../racloud.rkt" main)
         racket/runtime-path)

(define-runtime-path place-worker-path "place-worker.rkt")
(define-runtime-path process-worker-path "process-worker.rkt")

(define (build-supervised-place-worker port message)
  (supervise-place-at "localhost" #:port port place-worker-path 'place-worker #:initial-message message #:restart-on-exit #t))

(master-event-loop
  (build-supervised-place-worker 6341 "ONE")
  (build-supervised-place-worker 6342 "TWO")
  (build-supervised-place-worker 6343 "THREE")
  #;(supervise-process-at "localhost" #:port 6344 process-worker-path #:restart-on-exit #t))
