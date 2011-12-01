#lang racket/base
(require racket/place
         racket/match
         (except-in "racloud.rkt" main))

(provide hello)

(define (hello ch)
  (let loop ()
    (match (place-channel-get ch)
      [x (printf "Hello from: ~a\n" x) (flush-output)])
    (loop)))
