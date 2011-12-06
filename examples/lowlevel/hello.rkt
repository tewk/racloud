#lang racket/base
(require racket/place)

(require (except-in "../../racloud.rkt" main))

(provide hello1
         hello2)

(define (echo ch)
  (define msg (place-channel-get ch))
  (displayln msg)
  (place-channel-put ch (format "Hello ~a" msg)))

(define hello1 echo)
(define hello2 echo)
