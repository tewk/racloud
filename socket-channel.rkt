#lang racket/base
(require racket/list
         racket/match
         racket/tcp
         racket/place)

(provide main
         (struct-out dcg)
         dcg-send
         dcg-send-type
         dcg-recv
         dcg-kill
         dchannel-put
         dchannel-get)

(define (write-flush msg [p current-output-port])
  (write msg p)
  (flush-output p))

(struct socket-pair (in out))
;distributed communication group
(struct dcg (ch id n))
;distributed communication group message
(struct dcgm (type src dest msg) #:prefab)

(define DCGM-TYPE-NORMAL 0)
(define DCGM-TYPE-DIE    1)
(define DCGM-TYPE-NEW-DCHANNEL    2)


(define dchannel-put place-channel-put)
(define dchannel-get place-channel-get)

(define (dcg-send-type c type dest msg)
  (place-channel-put (dcg-ch c) (dcgm type (dcg-id c) dest msg)))

(define (dcg-send c dest msg)
  (dcg-send-type c DCGM-TYPE-NORMAL dest msg))

(define (dcg-kill c dest)
  (place-channel-put (dcg-ch c) (dcgm DCMG-TYPE-DIE (dcg-id c) dest "DIE")))

(define (dcg-send-new-dchannel c dest)
  (define-values (e1 e2) (place-channel))
  (dcg-send-type c DCGM-TYPE-NEW-DCHANNEL dest e2))

(define (dcg-recv c)
  (place-channel-get (dcg-ch c)))

(define-syntax-rule (reduce-sum seq item body ...)
  (for/fold ([sum 0]) ([item seq]) (+ sum (begin body ...))))

(define (total-node-count conf) (reduce-sum conf item (second item)))

(define (start-node-router chan-vec)
  (define (forward-mesg m src-channel)
    (match m
      [(dcgm ,DCMG-TYPE-DIE "DIE" dest msg) (exit 1)]
      [(dcgm ,DCMG-TYPE-NEW-DCHANNEL src dest msg)
        (define d (vector-ref chan-vec dest))
        (cond
          [(socket-pair? d)
            (write-flush m (socket-pair-out d))]
          [(or (place-channel? d) (place? d))
            (place-channel-put d msg)])]
       
      [(dcgm mtype srcs dest msg)
        (define d (vector-ref chan-vec dest))
        (cond
          [(socket-pair? d)
            (write-flush m (socket-pair-out d))]
          [(or (place-channel? d) (place? d))
            (place-channel-put d m)])]
      [(? eof-object?)
        (printf "connection died\n")
        (exit 1)]))

  (define (ch->evt x)
    (cond
      [(socket-pair? x)
       (define in (socket-pair-in x))
       (handle-evt in (lambda (e) (forward-mesg (read in) x)))]
      [(or (place-channel? x) (place? x))
       (handle-evt x (lambda (e) (forward-mesg e x)))]))

  (define evt-lst
    (for/list ([x (in-vector chan-vec)])
      (ch->evt x)))

  (let loop ()
    (apply sync evt-lst)
    (loop)))

(define (startup [port 6342] [conf #f])
  (start-node-router 
    (cond 
      ;master
      [conf
        (define t-n-c (total-node-count conf))
        (define cv (make-vector t-n-c null))
        (build-down port cv conf 0)
        cv]
      ;slave
      [else
        (listen/init-channels port)])))

;; Contract: build-down : port channel-vector conf conf-idx -> (void)
;;
;; Purpose: build up channel-vector by connecting to nodes greater than my-id
;;
;; Example: (build-down 6432 channel-vector conf 0)
;;
(define (build-down port cv conf conf-idx)
  (define t-n-c (total-node-count conf))
  (match-define (list* node-name node-cnt _ _ modpath funcname rst) (list-ref conf conf-idx))
  (define (isself? rname) (equal? rname node-name))

  (for/fold ([my-id #f]
             [next-node-id 0])
            ([item conf]
             [conf-idx (in-naturals)])
    (match-define (and h (list* rname rcnt rport rst)) item)

    (define (loopit my-id)
      (values my-id (+ next-node-id rcnt)))
    (define (remote-spawn)
      (define-values (in out) (tcp-connect rname rport))
      (define msg (list my-id node-name node-cnt conf-idx next-node-id rname rcnt conf))
      (printf "Sending ~a\n" msg)
      (write-flush msg  out)
      (define sp (socket-pair in out))
      (for ([i (in-range rcnt)])
        (vector-set! cv (+ next-node-id i) sp))
      (loopit my-id))
    (define (local-spawn)
      (for ([i (in-range rcnt)])
        (define sp (dynamic-place modpath funcname))
        (vector-set! cv (+ next-node-id i) sp)
        (place-channel-put sp (list (+ next-node-id i) t-n-c)))
      (loopit next-node-id))

   (cond 
     [my-id  (remote-spawn)]
     [(isself? rname) (local-spawn)]
     [(not my-id) (loopit my-id)])))
       

;; Contract: listen/init-channels : port -> VectorOf[ socket-pair]
;;
;; Purpose: build up channel-vector by listening for connect requests for nodes less than
;; myid. Spawn thread to build channel-vector by connecting to nodes greater than myid.
;;
;; Example: (listen/init-channels 6432)
;;
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
    (match-define (list sid sname scnt myidx myid myname mycnt conf) (read in))
    (printf "Listen ~a\n" (list sid sname scnt myidx myid myname mycnt conf))
    (let*
      ([cnt (or cnt (- myidx 1))]
       [cv  (or cv  (make-vector (total-node-count conf) null))]
       [thr (or thr (thread (lambda () (build-down port cv conf myidx))))])

      (for ([i (in-range scnt)])
        (vector-set! cv (+ sid i) sp))

      (if (= 0 cnt)
        (begin (thread-wait thr) cv)
        (loop (sub1 cnt) thr cv)))))

(define (main . args)
  (define conf (list (list "nan"  2 6341 #t "echo.rkt" 'echo-node)
                   (list "nan2" 2 6342 #t "echo.rkt" 'echo-node)
                   (list "nan3" 2 6343 #t "echo.rkt" 'echo-node)))

  (match args
    [(list "1") (startup 6341 conf)]
    [(list "2") (startup 6342)]
    [(list "3") (startup 6343)]
    [(list "p" p) (startup (string->number p))]))

