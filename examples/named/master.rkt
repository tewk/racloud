#lang racket/base
(require "../../racloud.rkt"
         racket/class
         racket/place
         racket/runtime-path
         "bank.rkt"
         "tuple.rkt")
(define-runtime-path bank-path "bank.rkt")                                                                    
(define-runtime-path tuple-path "tuple.rkt")

(provide main)

(define (main)
  (define remote-vm   (spawn-remote-racket-vm "localhost" #:listen-port 6344))
  (define tuple-place (supervise-named-place-thunk-at remote-vm 'tuple-server tuple-path 'make-tuple-server))
  (define bank-place  (remote-place remote-vm bank-path 'make-bank))

  (master-event-loop
    remote-vm
    (after-seconds 4
      (displayln (bank-new-account bank-place 'kevin))
      (displayln (bank-add bank-place 'kevin 10))
      (displayln (bank-removeM bank-place 'kevin 5)))

    (after-seconds 2
      (define c (connect-to-named-place remote-vm 'tuple-server))
      (define d (connect-to-named-place remote-vm 'tuple-server))
      (displayln (tuple-server-set c "Kevin" 100))
      (displayln (tuple-server-set d "Kyle" 200))
      (displayln (tuple-server-get c "Kevin"))
      (displayln (tuple-server-get d "Kyle"))
      (displayln (tuple-server-get d "Kevin"))
      (displayln (tuple-server-get c "Kyle")))
    (after-seconds 6
      (vm-send-exit remote-vm))
    (after-seconds 8
      (exit 0))))
