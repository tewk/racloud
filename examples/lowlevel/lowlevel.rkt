#lang racket/base

(require "../../racloud.rkt"
         racket/runtime-path)

(define-runtime-path hello-path "hello.rkt")

(define vm1 (spawn-remote-racket-vm "localhost" #:listen-port 6431))
(define vm2 (spawn-remote-racket-vm "localhost" #:listen-port 6433))

(define llch1 (remote-dynamic-place vm1 hello-path 'hello1))
(define llch2 (remote-dynamic-place vm2 hello-path 'hello2))

(ll-channel-put llch1 "Hello 1")
(ll-channel-put llch2 "Hello 2")

(displayln (ll-channel-get llch1))
(displayln (ll-channel-get llch2))
