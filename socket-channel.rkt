#lang racket/base
(require racket/list
         racket/match
         racket/tcp
         racket/place)

(provide main
         (struct-out dcg)
         dcg-send
         dcg-recv)

(define (write/f msg [p current-output-port])
  (write msg p)
  (flush-output p))

(struct socket-pair (in out))
(struct dcg (ch id n))
(struct dcgm (src dest msg) #:prefab)

(define (dcg-send c dest msg)
  (place-channel-put (dcg-ch c) (dcgm (dcg-id c) dest msg)))

(define (dcg-send-router-die c dest msg)
  (place-channel-put (dcg-ch c) (dcgm (dcg-id c) dest msg)))

(define (dcg-recv c)
  (place-channel-get (dcg-ch c)))



(define (total-node-count conf)
  (for/fold ([sum 0]) ([l conf]) (+ sum (second l))))

(define (start-node-router chan-vec)
  (define (handle-mesg m)
    (match m
      ;[(dcgm srcs "DIE" msg) (exit 1)]
      [(dcgm srcs dest msg)
        (define d (vector-ref chan-vec dest))
        (cond
          [(socket-pair? d)
            (write/f m (socket-pair-out d))]
          [(or (place-channel? d) (place? d))
            (place-channel-put d m)])]))

  (define (ch->evt x)
    (cond
      [(socket-pair? x)
       (handle-evt (socket-pair-in x) (lambda (e)
         (handle-mesg (read (socket-pair-in x)))))]
      [(or (place-channel? x) (place? x))
       (handle-evt x (lambda (e)
         (handle-mesg e)))]))

  (define evt-lst
    (for/list ([x (in-vector chan-vec)])
      (ch->evt x)))

  (let loop ()
    (apply sync evt-lst)
    (loop)))

(define (startup [conf #f] [node-name #f] [node-id #f] [modpath #f] [funcname #f])
  (start-node-router 
    (cond 
    [conf
      (define t-n-c (total-node-count conf))
      (define cv (make-vector t-n-c null))
      (build-net 6342 cv 0 (caar conf) (cadar conf) conf modpath funcname)
      cv]
    [else
      (listen/init-channels 6342)])))


(define (build-net port cv node-id node-name node-cnt conf modpath funcname)
  (define t-n-c (total-node-count conf))
  (define (isself? host-info) (equal? (first host-info) node-name))

  (let loop ([conf-rest conf]
             [idx 0]
             [seen-self #f]
             [nn 0]
             [rl null])
    (match conf-rest
      [(list) (reverse rl)]
      [(cons (and h (list* rname rcnt rst)) t)
        (define (remote-spawn)
          (define-values (in out) (tcp-connect rname port))
          (write/f (list node-id node-name node-cnt idx nn rname rcnt conf modpath funcname) out)
          (define sp (socket-pair in out))
          (for ([i (in-range rcnt)])
            (vector-set! cv (+ nn i) sp)))
        (define (local-spawn)
          (for ([i (in-range rcnt)])
            (define sp (dynamic-place modpath funcname))
            (vector-set! cv (+ nn i) sp)
            (place-channel-put sp (list (+ nn i) t-n-c))))

       (cond 
         [seen-self       (loop t (add1 idx) #t (+ nn rcnt) (cons (remote-spawn) rl))]
         [(isself? h)     (loop t (add1 idx) #t (+ nn rcnt) (cons (local-spawn) rl))]
         [(not seen-self) (loop t (add1 idx) #f (+ nn rcnt) (cons null rl))])])))

(define (listen/init-channels port)
  (define listener (tcp-listen port))
  (let loop ([cnt #f]
             [thr #f]
             [cv #f])
    (define-values (in out) (tcp-accept listener))
    (define sp (socket-pair in out))
    (match-define (list sid sname scnt myidx myid myname mycnt conf modpath funcname) (read in))
    (when (not cv)
      (set! cnt (- myidx 1))
      (set! thr (thread (lambda () (build-net port cv myid myname mycnt conf modpath funcname))))
      (set! cv (make-vector (total-node-count conf) null)))

    (for ([i (in-range scnt)])
      (vector-set! cv (+ sid i) sp))

    (if (= 0 cnt)
      (begin (thread-wait thr) cv)
      (loop #t (sub1 cnt) thr cv))))

(define (main . args)
  (define nl (list (list "nan"  2 #t "echo.rkt" 'echo-node)
                   (list "nan2" 2 #t "echo.rkt" 'echo-node)))

  (match args
    [(list "1") (startup nl tan 0 "echo.rkt" 'echo-node)]
    [(list "2") (startup)]))

