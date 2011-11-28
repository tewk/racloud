#lang racket/base
(require racket/place
         racket/match
         (except-in "racloud.rkt" main))

(provide echo-node
         main
         config)

(define (echo-node ch)
  (define c (apply dcg ch (place-channel-get ch)))
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
      (define hch (dcg-spawn-remote-process c "nan4" #:port 6434 "hello.rkt" 'hello))
      (place-channel-put hch "Hello new node!")
      ;(for ([i (in-range 1 (dcg-n c))]) (dcg-kill c i))
      ]

    [else
      (displayln (dcg-recv c))
      (displayln (dcg-recv c))
      (define dch (dcg-recv c))
      (printf "dch ~a\n" dch)
      (displayln (dchannel-get dch))]))

(define racketpath (path->string (racket-path)))
(define racloudpath (racloud-path))
(define echopath (get-current-module-path))
(define config 
  (list
    (ext-config "nan"  "6431" 2 racketpath racloudpath echopath 'echo-node 'config)
    (ext-config "nan2" "6432" 2 racketpath racloudpath echopath 'echo-node 'config)
    (ext-config "nan3" "6433" 2 racketpath racloudpath echopath 'echo-node 'config)))

(define (main)
  (spawn-config config))

