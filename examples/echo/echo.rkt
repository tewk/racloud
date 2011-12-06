#lang racket/base
(require racket/place
         racket/match
         racket/runtime-path
         (except-in "../../racloud.rkt" main))

(provide echo-node
         main
         config)

(define-runtime-path hello-path "hello.rkt")

(define (echo-node ch)
  (define c (dcg-get-cg ch))
  (match (dcg-id c)
    [0
      (for ([i (in-range 1 (dcg-n c))]) (dcg-send c i (format "Hello ~a" i)))
      (sleep 1)
      (for ([i (in-range 1 (dcg-n c))]) (dcg-send c i (format "Hello Again ~a" i)))
      (sleep 1)
      (define new-channels
        (for/list ([i (in-range 1 (dcg-n c))])
          (dcg-send-new-dchannel c i)))
      (for ([i (in-range 1 (dcg-n c))]
            [ch new-channels])
        (dchannel-put ch (format "Hello on new chanel ~a" i)))
      (sleep 1)
      (define hch (dcg-spawn-remote-dplace c "nan4" #:listen-port 6434 (path->string hello-path) 'hello))
      (dchannel-put hch "Hello new node!")
      (sleep 1)
      (sleep 1)
      (sleep 1)
      (sleep 1)
      (sleep 1)
      ;(for ([i (in-range 1 (dcg-n c))]) (dcg-kill c i))
      ]

    [else
      (displayln (dcg-recv c))
      (displayln (dcg-recv c))
      (define dch (dcg-recv c))
      (printf "dch ~a\n" dch)
      (displayln (dchannel-get dch))]))
(define ssh-path (ssh-bin-path))
(define racketpath (path->string (racket-path)))
(define racloudpath (racloud-path))
(define echopath (get-current-module-path))
(define config 
  (list
    (node-config "localhost" "6431" 2 ssh-path racketpath racloudpath echopath 'echo-node echopath 'config)
    (node-config "localhost" "6432" 2 ssh-path racketpath racloudpath echopath 'echo-node echopath 'config)
    (node-config "localhost" "6433" 2 ssh-path racketpath racloudpath echopath 'echo-node echopath 'config)))

(define (main)
  (launch-config config))

