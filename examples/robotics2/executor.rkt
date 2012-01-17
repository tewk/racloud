#lang racket/base
(require racket/place
         racket/match
         "../../racloud.rkt")

(provide executor-node)

(define CONTROL-ID 0)
(define EXECUTOR-ID 1)

(define (executor-node ch)
  (define cg (dcg-get-cg ch))

  (define d-ch1 (dcg-recv cg))
  (define d-ch2 (dcg-recv cg))
  (define d-ch3 (dcg-recv cg))
  (define d-ch4 (dcg-recv cg))
    
  (displayln (dchannel-get d-ch4))
  (displayln (dchannel-get d-ch2))
  (displayln (dchannel-get d-ch3))
  (displayln (dchannel-get d-ch1))
  (flush-output)

  (let loop ()
    (displayln (dcg-recv cg))
    (flush-output)
    (loop)))
