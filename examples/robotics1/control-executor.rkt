#lang racket/base
(require racket/place
         racket/match
         racket/runtime-path
         racket/pretty
         (except-in "../../racloud.rkt" main))

(provide control-node
         executor-node
         main
         config)

(define CONTROL-ID 0)
(define EXECUTOR-ID 1)

(define (control-node ch)
  (define cg (dcg-get-cg ch))

  (define direct-ch1 (dcg-send-new-dchannel cg EXECUTOR-ID))
  (define direct-ch2 (dcg-send-new-dchannel cg EXECUTOR-ID))
  (define direct-ch3 (dcg-send-new-dchannel cg EXECUTOR-ID))
  (define direct-ch4 (dcg-send-new-dchannel cg EXECUTOR-ID))

  (dchannel-put direct-ch1 "Hello on dchannel 1")
  (dchannel-put direct-ch2 "Hello on dchannel 2")
  (dchannel-put direct-ch3 "Hello on dchannel 3")
  (dchannel-put direct-ch4 "Hello on dchannel 4")

  (let loop ([a 0])
    (cond
      [(a . < . 6)
        (dcg-send cg EXECUTOR-ID (format "Hello ~a" a))
        (sleep 5)
        (loop (add1 a))]
      [else (void)])))

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

(define ssh-path (ssh-bin-path))
(define racketpath (path->string (racket-path)))
(define racloudpath (racloud-path))
(define controlpath (get-current-module-path))
(define config 
  (list
    (node-config "nan" "6431" 1 ssh-path racketpath racloudpath controlpath 'control-node controlpath 'config)
    (node-config "nan2" "6432" 1 ssh-path racketpath racloudpath controlpath 'executor-node controlpath 'config)))

(define (main)
  (pretty-print config)
  (launch-config config))

