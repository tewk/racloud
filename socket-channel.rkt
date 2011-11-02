#lang racket/base
(require racket/list
         racket/match
         racket/tcp
         racket/place)

(provide main
         (struct-out dcg)
         dcg-send
         dcg-recv)

(define (write-flush msg [p current-output-port])
  (write msg p)
  (flush-output p))

(struct socket-pair (in out))
;distributed communication group
(struct dcg (ch id n))
;distributed communication group message
(struct dcgm (src dest msg) #:prefab)

(define (dcg-send c dest msg)
  (place-channel-put (dcg-ch c) (dcgm (dcg-id c) dest msg)))

(define (dcg-send-router-die c dest msg)
  (dcg-send c dest msg))

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
            (write-flush m (socket-pair-out d))]
          [(or (place-channel? d) (place? d))
            (place-channel-put d m)])]
      [eof 
        (printf "connection died\n")
        (exit 1)]))

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

(define (startup [port 6342] [conf #f] [node-name #f] [node-id #f] [modpath #f] [funcname #f])
  (start-node-router 
    (cond 
      ;master
      [conf
        (define t-n-c (total-node-count conf))
        (define cv (make-vector t-n-c null))
        (build-net port cv 0 (caar conf) (cadar conf) conf modpath funcname)
        cv]
      ;slave
      [else
        (listen/init-channels port)])))


(define (build-net port cv node-id node-name node-cnt conf modpath funcname)
  (define t-n-c (total-node-count conf))
  (define (isself? host-info) (equal? (first host-info) node-name))

  (let loop ([conf-rest conf]
             [idx 0]
             [seen-self #f]
             [next-node-id 0]
             [rl null])
    (match conf-rest
      [(list) (reverse rl)]
      [(cons (and h (list* rname rcnt rport rst)) t)
        (define (remote-spawn)
          (define-values (in out) (tcp-connect rname rport))
          (write-flush (list node-id node-name node-cnt idx next-node-id rname rcnt conf modpath funcname) out)
;          (printf "Sending ~a\n" (list node-id node-name node-cnt idx next-node-id rname rcnt conf modpath funcname))
          (define sp (socket-pair in out))
          (for ([i (in-range rcnt)])
            (vector-set! cv (+ next-node-id i) sp)))
        (define (local-spawn)
          (set! seen-self #t)
          (for ([i (in-range rcnt)])
            (define sp (dynamic-place modpath funcname))
            (vector-set! cv (+ next-node-id i) sp)
            (place-channel-put sp (list (+ next-node-id i) t-n-c))))

       (define node-result 
         (cond 
           [seen-self       (remote-spawn)]
           [(isself? h)     (local-spawn)]
           [(not seen-self) null]))
       
       (loop t (add1 idx) seen-self (+ next-node-id rcnt) (cons node-result rl))])))
;; Node 1  Node 2  Node 3
;; 1 2     3 4     5 6
;;
;;
(define (listen/init-channels port)
  (define listener (tcp-listen port))
  (let loop ([cnt #f]
             [thr #f]
             [cv #f])
    (define-values (in out) (tcp-accept listener))
    (define sp (socket-pair in out))
    (match-define (list sid sname scnt myidx myid myname mycnt conf modpath funcname) (read in))
;    (printf "Listen ~a\n" (list sid sname scnt myidx myid myname mycnt conf modpath funcname))
    (when (not cv)
      (set! cnt (- myidx 1))
      (set! thr (thread (lambda () (build-net port cv myid myname mycnt conf modpath funcname))))
      (set! cv (make-vector (total-node-count conf) null)))

    (for ([i (in-range scnt)])
      (vector-set! cv (+ sid i) sp))

    (if (= 0 cnt)
      (begin (thread-wait thr) cv)
      (loop (sub1 cnt) thr cv))))

(define (main . args)
  (define nl (list (list "nan"  2 6341 #t "echo.rkt" 'echo-node)
                   (list "nan2" 2 6342 #t "echo.rkt" 'echo-node)
                   (list "nan3" 2 6343 #t "echo.rkt" 'echo-node)))

  (match args
    [(list "1") (startup 6341 nl tan 0 "echo.rkt" 'echo-node)]
    [(list "2") (startup 6342)]
    [(list "3") (startup 6343)]
    [(list "p" p) (startup (string->number p))]))

