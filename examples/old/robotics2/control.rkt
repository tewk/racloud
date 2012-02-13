#lang racket/base
(require racket/place
         racket/match
         racket/runtime-path
         "../../racloud.rkt")

(provide control-node
         main
         config)

(define CONTROL-ID 0)
(define EXECUTOR-ID 1)

(define-runtime-path executorpath "executor.rkt")

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
    (dcg-send cg EXECUTOR-ID (format "Hello ~a" a))
    (sleep 5)
    (loop (add1 a))))

(define ssh-path (path->string (ssh-bin-path)))
(define racketpath (path->string (racket-path)))
(define racloudpath (racloud-path))
(define controlpath (get-current-module-path))
(define config 
  (list
    (node-config "localhost" "6431" 1 ssh-path racketpath racloudpath controlpath 'control-node controlpath 'config)
    (node-config "localhost" "6432" 1 ssh-path racketpath racloudpath (path->string executorpath) 'executor-node controlpath 'config)))

(define (main)
  (launch-config config))

