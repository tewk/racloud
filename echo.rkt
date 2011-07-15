#lang racket/base
(require racket/place
         racket/match
         "socket-channel.rkt")

(provide echo-node)

(define (echo-node ch)
  (define c (apply dcg ch (place-channel-get ch)))
  (match (dcg-id c)
    [0
      (for ([i (in-range (dcg-n c))])
        (dcg-send c i (format "Hello ~a" i)))
      (sleep 2)
      (for ([i (in-range 1 (dcg-n c))])
                    (dcg-send c i (format "Hello ~a" i)))]
    [else
      (displayln (dcg-recv c))]))


