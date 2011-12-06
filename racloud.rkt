#lang racket/base
(require racket/list
         racket/match
         racket/tcp
         racket/place
         racket/class
         racket/trait)

(provide main
         (struct-out node-config)
         (struct-out dcg)
         ssh-bin-path
         racket-path
         racloud-path
         get-current-module-path

         dcg-get-cg
         dcg-send
         dcg-send-type
         dcg-recv
         dcg-kill

         dcg-send-new-dchannel
         dcg-spawn-remote-dplace

         dchannel-put
         dchannel-get

         launch-config

         master-event-loop
         supervise-place-at
         supervise-process-at


         ;; low-level API
         spawn-remote-racket-vm
         remote-place 
         )

(define DEFAULT-ROUTER-PORT 6342)

(define (->path x)
  (cond [(path? x) x]
        [(string? x) (string->path x)]))

(define (->number x)
  (cond [(number? x) x]
        [(string? x) (string->number x)]))

(define (->string x)
  (cond [(string? x) x]
        [(number? x) (number->string x)]
        [(symbol? x) (symbol->string x)]
        [(path? x) (path->string x)]
        ))

(define (->length x)
  (cond [(string? x) (string-length x)]
        [(bytes? x) (bytes-length x)]
        [(list?  x) (length x)]))

(define-syntax-rule (get-current-module-path)
  (path->string (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference)))))

; returns the path to the racket executable on the current machine.
(define (racket-path)
  (parameterize ([current-directory (find-system-path 'orig-dir)])                                             
    (find-executable-path (find-system-path 'exec-file) #f)))

; returns the path to the racloud.rkt file on the current machine.
(define (racloud-path)
  (path->string (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference)))))

; hard-coded default for ssh binary
(define (ssh-bin-path) "/usr/bin/ssh")

(define (write-flush msg [p (current-output-port)])
  (write msg p)
  (flush-output p))

(define (tcp-connect/backoff rname rport #:times [times 4] #:start-seconds [start-seconds 1])
  (let loop ([t 0]
             [wait-time start-seconds])
    (with-handlers ([exn? (lambda (e) 
                  (cond [(t . < . times) 
                         (printf "backing off ~a sec to ~a:~a\n" (expt 2 t) rname rport)
                         (sleep wait-time) 
                         (loop (add1 t) (* 2 wait-time))]
                        [else (raise e)]))])
      (tcp-connect rname (->number rport)))))

(define (tcp-connect/retry rname rport #:times [times 10] #:delay [delay 1])
  (let loop ([t 0])
    (with-handlers ([exn? (lambda (e) 
                  (cond [(t . < . times) 
                         (printf "waiting ~a sec to retry connection to ~a:~a\n" delay rname rport)
                         (sleep delay) 
                         (loop (add1 t))]
                        [else (raise e)]))])
      (tcp-connect rname (->number rport)))))

;node configuration
(struct node-config (node-name node-port proc-count ssh-path racket-path racloud-path mod-path func-name conf-path conf-name) #:prefab)

;socket channel
(struct socket-channel (in out [subchannels #:mutable]))
(define (socket-channel-add-subchannel s ch-id ch)
  (set-socket-channel-subchannels! s (append (socket-channel-subchannels s) (list (cons ch-id ch)))))
(define (socket-channel-lookup-subchannel s ch-id)
  (cdr (assoc ch-id (socket-channel-subchannels s))))

;distributed communication group
(struct dcg (ch id n))
;distributed communication group message
(struct dcgm (type src dest msg) #:prefab)

(struct dchannel (ch) #:prefab)

;dcg types
(define DCGM-TYPE-NORMAL               0)
(define DCGM-TYPE-DIE                  1)
(define DCGM-TYPE-NEW-DCHANNEL         2)
(define DCGM-TYPE-NEW-INTER-DCHANNEL   3)
(define DCGM-TYPE-INTER-DCHANNEL       4)
(define DCGM-TYPE-KILL-DPLACE          5)
(define DCGM-TYPE-SPAWN-REMOTE-PROCESS 6)
(define DCGM-DPLACE-DIED               7)


(define (dchannel-put ch msg)
  (unless (or (dchannel? ch) (place-channel? ch))
    (raise-mismatch-error 'dchannel-get "expected dchannel?, got " ch))
  (if (dchannel? ch)
    (place-channel-put (dchannel-ch ch) msg)
    (place-channel-put ch msg)))

(define (dchannel-get ch)
  (unless (or (dchannel? ch) (place-channel? ch))
    (raise-mismatch-error 'dchannel-get "expected dchannel?, got " ch))
  (if (dchannel? ch)
    (place-channel-get (dchannel-ch ch))
    (place-channel-get ch)))

(define (dcg-send-type c type dest msg)
  (place-channel-put (dcg-ch c) (dcgm type (dcg-id c) dest msg)))

(define (dcg-send c dest msg)
  (dcg-send-type c DCGM-TYPE-NORMAL dest msg))

(define (dcg-get-cg ch) (apply dcg ch (place-channel-get ch)))

(define (dcg-kill c dest)
  (place-channel-put (dcg-ch c) (dcgm DCGM-TYPE-DIE (dcg-id c) dest "DIE")))

(define (dcg-send-new-dchannel c dest)
  (define-values (e1 e2) (place-channel))
  (dcg-send-type c DCGM-TYPE-NEW-DCHANNEL dest (dchannel e2))
  (dchannel e1))

;; Contract: start-node-router : VectorOf[ (or/c place-channel socket-channel)] -> (void)
;; Purpose: Forward messages between channels and build new point-to-point subchannels
;; Example:
(define (dcg-spawn-remote-dplace c hostname modpath funcname #:listen-port [listen-port 6432])
  (define-values (e1 e2) (place-channel))
  (dcg-send-type c DCGM-TYPE-SPAWN-REMOTE-PROCESS (list hostname listen-port modpath funcname) e2)
  e1)

(define (dcg-recv c)
  (dcgm-msg (place-channel-get (dcg-ch c))))

(define-syntax-rule (reduce-sum seq item body ...)
  (for/fold ([sum 0]) ([item seq]) (+ sum (begin body ...))))

(define (total-node-count conf) (reduce-sum conf item (node-config-proc-count item)))

;; Contract: start-node-router : VectorOf[ (or/c place-channel socket-channel)] -> (void)
;;
;; Purpose: Forward messages between channels and build new point-to-point subchannels
;;
;; Example:

(define (start-spawned-node-router listener)
  (define er (new event-router%))
  (define mv (new mesh-vector% [listen-port listener]))
  (send er add-ec mv)
  (send er sync-events)) 
   

(define (start-node-router chan-vec)
  (define er (new event-router%))
  (define mv (new mesh-vector% [chan-vec chan-vec]))
  (send er add-ec mv)
  (send er sync-events)) 

(define backlink 
  (trait->mixin 
    (trait
      (field [router #f])
      (define/public (remove-from-router)
        (send router remove-ec this))
      (define/public (add-to-router x)
        (send router add-ec x))
      (define/public (recompute-sync-list)
        (send router recompute-sync-list))
      (define/public (backlink _router)
        (set! router _router))
      (define/public (get-next-id)
        (send router nextid)))))
                   
(define event-container<%>
  (interface ()
;    backlink
    register
  ))

(define-syntax-rule (for/filter/fold/cons tail xs body ...)
  (for/fold ([n tail]) xs
    (define r (let () body ...))
    (if r (cons r n) n)))

(define (filter/list* . lst)
  (define rl (reverse lst))
  (for/filter/fold/cons (car rl) ([x (cdr rl)]) x))

(define spawned-process%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field cmdline-list)
      (field [s #f]
             [i #f]
             [o #f]
             [e #f]
             [pid #f])

      (printf "SPAWNING-PROCESS: ~a\n" cmdline-list)

      (let-values ([(_s _o _i _e) (apply subprocess #f #f #f cmdline-list)])
        (set! pid (subprocess-pid _s)) 
        (set! s _s)
        (set! o _o)
        (set! i _i)
        (set! e _e))

      (define (mk-handler port desc)
        (if port
          (handle-evt port (lambda (e) 
            (define (print-out x) (printf "SPAWNED-PROCESS ~a:~a:~a ~a\n" pid desc (->length x) x)
              (flush-output)) 
            (cond 
              [(not port) (print-out "IS #F")]
              [else
                (define bb (make-bytes 4096))
                (define bbl (read-bytes-avail!* bb port))                                                        
                (cond 
                  [(eof-object? bbl) 
                   (print-out "EOF")                                                                      
                   (set! port #f)]
                  [else                                                                                   
                   (print-out (subbytes bb 0 bbl))])])))
          #f))

      (define/public (register nes)
        (for/filter/fold/cons nes ([x (list s (list o "OUT") (list e "ERR"))])
          (cond 
            [(subprocess? x) (handle-evt s (lambda (e) (send this remove-from-router)))]
            [(list? x) (apply mk-handler x)]
            [else #f])))
      (super-new)
  )))

(define place-socket-bridge%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field
        pch sch id)
      (define/public (register nes)
        (cons 
          (handle-evt 
            (if (dchannel? pch) (dchannel-ch pch) pch) (lambda (e)
            (write-flush (dcgm DCGM-TYPE-INTER-DCHANNEL id id e) (socket-channel-out sch))))
          nes))
      (super-new)
  )))

(define mesh-vector%
  (backlink
    (class*
      object% (event-container<%>)
      (init-field [chan-vec #f])
      (init-field [listen-port #f])
      (init-field [socket-ports null])
      (init-field [sub-ecs null])
      (define (add-socket-port pair)
        (set! socket-ports (append socket-ports (list pair)))
        (send this recompute-sync-list))
      (define (add-sub-ec ec)
        (set! sub-ecs (append sub-ecs (list ec)))
        (send this recompute-sync-list))
      (define (add-place-channel-socket-bridge pch sch id)
        (send this add-to-router (new place-socket-bridge% [pch pch] [sch sch] [id id])))
(define DCGM-TYPE-NORMAL               0)
(define DCGM-TYPE-DIE                  1)
(define DCGM-TYPE-NEW-DCHANNEL         2)
(define DCGM-TYPE-NEW-INTER-DCHANNEL   3)
(define DCGM-TYPE-INTER-DCHANNEL       4)
(define DCGM-TYPE-KILL-DPLACE          5)
(define DCGM-TYPE-SPAWN-REMOTE-PROCESS 6)
(define DCGM-DPLACE-DIED               7)
      (define (forward-mesg m src-channel)
        (match m
          [(dcgm 1 #;(== DCGM-TYPE-DIE) src dest "DIE") (exit 1)]
          [(dcgm 2 #;(== DCGM-TYPE-NEW-DCHANNEL) src dest pch)
            (define d (vector-ref chan-vec dest))
            (cond
              [(socket-channel? d)
               (define ch-id (send this get-next-id))
               (socket-channel-add-subchannel d ch-id pch)
               (add-place-channel-socket-bridge pch d ch-id)
               (write-flush (dcgm DCGM-TYPE-NEW-INTER-DCHANNEL src dest ch-id)
                            (socket-channel-out d))]
              [(or (place-channel? d) (place? d))
                (place-channel-put d m)])]
          [(dcgm 3 #;(== DCGM-TYPE-NEW-INTER-DCHANNEL) -1 (list place-path place-func) ch-id)
           (add-sub-ec (new supervised-place%
                            [place-path place-path]
                            [place-func place-func]
                            [ch-id ch-id]
                            [sc src-channel]))]
          [(dcgm 3 #;(== DCGM-TYPE-NEW-INTER-DCHANNEL) src dest ch-id)
            (define s src-channel)
            (define d (vector-ref chan-vec dest))
            (define-values (pch1 pch2) (place-channel))
            (socket-channel-add-subchannel s ch-id pch1)
            (add-place-channel-socket-bridge pch1 s ch-id)
            (place-channel-put d (dcgm DCGM-TYPE-NEW-DCHANNEL src dest pch2))]
          [(dcgm 4 #;(== DCGM-TYPE-INTER-DCHANNEL) _ ch-id msg)
           (define pch (socket-channel-lookup-subchannel src-channel ch-id))
           (place-channel-put pch msg)]
          [(dcgm 6 #;(== DCGM-TYPE-SPAWN-REMOTE-PROCESS) src (list node-name node-port mod-path funcname) ch1)
           (send this add-to-router (new spawned-process% [cmdline-list
              (list (ssh-bin-path)  node-name (racket-path) "-tm" (racloud-path) "spawn" (->string node-port))]))
           (define-values (in out) (tcp-connect/backoff node-name node-port))
           (define sp (socket-channel in out null))
           (define ch-id (send this get-next-id))
           (socket-channel-add-subchannel sp ch-id ch1)
           (add-place-channel-socket-bridge ch1 sp ch-id)
           (add-socket-port sp)
           (write-flush (dcgm DCGM-TYPE-NEW-INTER-DCHANNEL -1 (list mod-path funcname) ch-id)
                            (socket-channel-out sp))]
          [(dcgm 7 #;(== DCGM-DPLACE-DIED) -1 -1 ch-id)
            (printf "PLACE ~a died\n" ch-id)]
          [(dcgm mtype srcs dest msg)
            (define d (vector-ref chan-vec dest))
            (cond
              [(socket-channel? d)
                (write-flush m (socket-channel-out d))]
              [(or (place-channel? d) (place? d))
                (place-channel-put d m)])]
          [(? eof-object?)
            (printf "connection died\n")
            (flush-output)
            (exit 1)
            ]))

      (define/public (register nes)
        (let*
          ([nes
            (if chan-vec
              (for/fold ([n nes]) ([x (in-vector chan-vec)])
                (cons
                  (cond
                    [(socket-channel? x)
                     (define in (socket-channel-in x))
                     (handle-evt in (lambda (e) (forward-mesg (read in) x)))]
                    [(or (place-channel? x) (place? x))
                     (handle-evt x (lambda (e) (forward-mesg e x)))])
                  n))
              nes)]
           [nes 
            (if listen-port
              (cons
                (handle-evt listen-port (lambda (e)
                  (define-values (in out) (tcp-accept listen-port))
                  (define sp (socket-channel in out null))
                  (add-socket-port sp)))
                nes)
              nes)]
           [nes
            (if socket-ports
              (for/fold ([n nes]) ([x socket-ports])
                (cons
                  (cond
                    [(socket-channel? x)
                     (define in (socket-channel-in x))
                     (handle-evt in (lambda (e) (forward-mesg (read in) x)))]
                    [(or (place-channel? x) (place? x))
                     (handle-evt x (lambda (e) (forward-mesg e x)))])
                  n))
              nes)]
           [nes
            (if sub-ecs
              (for/fold ([n nes]) ([x sub-ecs])
                (send x register n))
              nes)])
          nes))


      (super-new)
  )))

(define socket-connection%
  (backlink
    (class* object% (event-container<%>)
      (init-field [in #f]
                  [out #f])
      (define (forward-mesg x) (void))
      (define/public (register nes)
        (cons 
          (handle-evt in (lambda (e) 
            (forward-mesg (read in))))
          nes))
      (super-new)
  )))

(define remote-vm%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field host-name)
      (init-field listen-port)
      (init-field [cmdline-list #f])
      (init-field [sc #f]) ;socket-channel
      (field [sp #f]) ;spawned-process
      (field [id 0])

      (define/public (nextid)
        (set! id (add1 id))
        id)

      (field [remote-places null])
      (define (add-remote-place rp)
        (set! remote-places (append remote-places(list rp)))
        ;(send this recompute-sync-list)
        )

      (when (and cmdline-list (not sc))
        (set! sp (new spawned-process% [cmdline-list cmdline-list])))
      (unless sc
        (define-values (in out) (tcp-connect/retry host-name listen-port))
        (set! sc (socket-channel in out null)))

      (define (on-socket-event e)
        (define it (read e))
        (match it
          [(dcgm 7 #;(== DCGM-DPLACE-DIED) -1 -1 ch-id)
            (printf "PLACE ~a:~a:~a died\n" host-name listen-port ch-id)]
          [else (printf "recveived message ~a\n" it)]))

      (define/public (get-log-prefix) (format "PLACE ~a:~a" host-name listen-port))

      (define/public (launch-place place-path place-func)
        (define rp (new remote-place% [vm this] [place-path place-path] [place-func place-func]))
        (add-remote-place rp))

      (define/public (register-dchannel-with-router place-path place-func dch)
        (define ch-id (nextid))
        (socket-channel-add-subchannel sc ch-id dch)
        (write-flush (dcgm DCGM-TYPE-NEW-INTER-DCHANNEL -1 (list (->string place-path) place-func) ch-id) (socket-channel-out sc))
        (new place-socket-bridge% [pch dch] [sch sc] [id ch-id]))

      (define/public (register es)
        (let* ([es (if sp (send sp register es) es)]
               [es (if sc (cons (handle-evt (socket-channel-in sc) on-socket-event) es) es)])
          es))

      (super-new)
      )))

(define remote-place%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field vm)
      (init-field [place-path #f])
      (init-field [place-func #f])
      (field [psb #f])
      (field [pc #f])
      (field [running #f])

      (define-values (pch1 pch2) (place-channel))
      (set! psb (send vm register-dchannel-with-router place-path place-func pch1))
      (set! pc pch2)

      (define/public (stop)
        (void))
      (define (on-channel-event e)
        (printf "~a ~a\n" (send vm get-log-prefix) e))
      (define/public (register es)
        (let* ([es (if pc (cons (handle-evt pc on-channel-event) es) es)])
          es))

      (super-new)
      )))


(define supervised-place%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field place-path)
      (init-field place-func)
      (init-field ch-id)
      (init-field sc)
      (field [pd #f])
      (field [psb #f])
      (field [running #f])
      (define (default-on-place-dead e)
        (set! pd #f)
        (printf "PLACE DIED ~a\n" e)
        (flush-output)
        (write-flush (dcgm DCGM-DPLACE-DIED -1 -1 ch-id) (socket-channel-out sc)))

      (init-field [on-place-dead default-on-place-dead])

      (set! pd (dynamic-place (->path place-path) place-func))
      (socket-channel-add-subchannel sc ch-id pd)
      (set! psb (new place-socket-bridge% [pch pd] [sch sc] [id ch-id]))

      (define/public (stop)
        (cond
          [pd 
            (place-kill pd)
            (set! pd #f)]
          [else (void)])) ;send place not running message

      (define/public (register es)
        (let* ([es (if pd (cons (handle-evt (place-dead-evt pd) on-place-dead) es) es)]
               [es (if psb (send psb register es) es)])
          es))
      (super-new)
      )))



(define event-router%
  (class* object% ()
    (field [ecs null])
    (field [es null])
    (field [build-list #t])
    (field [id 0])
    (field [quit #f])

    (define/public (nextid)
      (set! id (add1 id))
      id)
    (define/public (add-ec ec)
      (send ec backlink this)
      (set! ecs (cons ec ecs))
      (set! build-list #t))
    (define/public (remove-ec ec)
      (set! ecs (remove ec ecs))
      (set! build-list #t))
    (define/public (recompute-sync-list)
      (set! build-list #t))

    (define/public (build-es-list)
      (set! es 
        ;(list*
          #;(handle-evt (current-output-port)
            (lambda (e)
              (flush-output)))
          #;(handle-evt (current-error-port)
            (lambda (e)
              (flush-output (current-error-port))))
          (for/fold ([nes null]) ([ec ecs])
            (send ec register nes)))
       es) ;)

    (define/public (sync-events)
      (let loop ()
        (when build-list (build-es-list)
          (set! build-list #f))
        (apply sync (build-es-list))
        (unless quit
          (loop))))
    (super-new)
  ))

(define (startup conf conf-idx)
  (start-node-router 
    (cond 
      ;master
      [(= 0 conf-idx)
        (define t-n-c (total-node-count conf))
        (define cv (make-vector t-n-c null))
        (build-down (node-config-node-port (first conf)) cv conf 0)
        cv]
      ;slave
      [else
        (listen/init-channels (node-config-node-port (list-ref conf conf-idx)))])))

;; Contract: build-down : port channel-vector conf conf-idx -> (void)
;;
;; Purpose: build up channel-vector by connecting to nodes greater than my-id
;;
;; Example: (build-down 6432 channel-vector conf 0)
;;
(define (build-down port cv conf conf-idx)
  (define t-n-c (total-node-count conf))
  (match-define (node-config node-name _ node-cnt _ _ _ modpath funcname config-path confname) (list-ref conf conf-idx))
  (define (isself? rid) (equal? rid conf-idx))

  (for/fold ([my-id #f]
             [next-node-id 0])
            ([item conf]
             [curr-conf-idx (in-naturals)])
    (match-define (node-config rname rport rcnt _ _ _ modpath funcname conf-path confname) item)

    (define (loopit my-id)
      (values my-id (+ next-node-id rcnt)))
    (define (remote-spawn)
      (define-values (in out) (tcp-connect/backoff rname rport))
      (define msg (list my-id node-name node-cnt curr-conf-idx next-node-id rname rcnt conf))
      ;(printf "Sending ~v\n" msg)
      (write-flush msg out)
      (define sp (socket-channel in out null))
      (for ([i (in-range rcnt)])
        (vector-set! cv (+ next-node-id i) sp))
      (loopit my-id))
    (define (local-spawn)
      (for ([i (in-range rcnt)])
        (define sp (dynamic-place (->path modpath) funcname))
        (vector-set! cv (+ next-node-id i) sp)
        (place-channel-put sp (list (+ next-node-id i) t-n-c)))
      (loopit next-node-id))

   (cond 
     [my-id  (remote-spawn)]
     [(isself? curr-conf-idx) (local-spawn)]
     [(not my-id) (loopit my-id)])))
       

;; Contract: listen/init-channels : port -> VectorOf[ socket-channel]
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
  (define listener (tcp-listen (->number port) 4 #t))
  (let loop ([cnt #f]
             [thr #f]
             [cv #f])
    (define-values (in out) (tcp-accept listener))
    (define sp (socket-channel in out null))
    (match-define (list sid sname scnt myidx myid myname mycnt conf) (read in))
    ;(printf "Listen ~a\n" (list sid sname scnt myidx myid myname mycnt conf))
    (let*
      ([cnt (or cnt (- myidx 1))]
       [cv  (or cv  (make-vector (total-node-count conf) null))]
       [thr (or thr (thread (lambda () (build-down port cv conf myidx))))])

      (for ([i (in-range scnt)])
        (vector-set! cv (+ sid i) sp))

      (if (= 0 cnt)
        (begin (thread-wait thr) cv)
        (loop (sub1 cnt) thr cv)))))

(define (supervise-process-at host #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:restart-on-exit [restart-on-exit #f]
                            . command-line-list)
  (void)
  )

(define (supervise-place-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:racloud-path [racloudpath (racloud-path)]
                            #:restart-on-exit [restart-on-exit #f])
  (define vm (spawn-remote-racket-vm host
                                     #:listen-port listen-port
                                     #:racket-path racketpath
                                     #:ssh-bin-path sshpath
                                     #:racloud-path racloudpath))
  (define dp 
    (send vm launch-place 
        place-path 
        place-func
        ;#:initial-message initial-message
        ;#:restart-on-exit restart-on-exit
        ))

  vm)

(define (master-event-loop . event-containers)
  (define er (new event-router%))
  (for ([ec event-containers])
    (send er add-ec ec))
  (send er sync-events)) 


(define (spawn-remote-racket-vm host #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                                     #:racket-path [racketpath (racket-path)]
                                     #:ssh-bin-path [sshpath (ssh-bin-path)]
                                     #:racloud-path [racloudpath (racloud-path)])
  (new remote-vm%
       [host-name host]
       [listen-port listen-port]
       [cmdline-list (list sshpath host racketpath "-tm" racloudpath "spawn" (->string listen-port))]))

(define (remote-place remote-vm place-path place-func)
  (send remote-vm launch-place 
      place-path 
      place-func))




;; Contract: node-config -> (void)
;;
;; Purpose: use ssh to launch remote nodes of racloud
;;
;; Example:
(define (launch-config config)
  ;FIXME kill safety
  (define nodes
    (for/list ([c config]
               [i (in-naturals)])
      (list
        (call-with-values 
          (lambda ()
            (match-define (node-config node-name node-port _ ssh-path racket-path racloud-path mod-path func-name config-path conf-name) c)
            (subprocess #f #f #f (ssh-bin-path) node-name racket-path "-tm" 
                        racloud-path
                        "launch" 
                        config-path
                        (symbol->string conf-name)
                        (number->string i)))
          list)
        c)))

  (define bb (make-bytes 4096))
  (define handlers
    (let ()
      (define (mkhandler port config)
        (let ()
          (define self
            (handle-evt port
              (lambda (x)
                (define bbl (read-bytes-avail!* bb x))
                (define (print-out x)
                  (printf "~a:~a:~a ~a\n" (node-config-node-name config) (node-config-node-port config) bbl x)
                  (flush-output))
                (cond [(eof-object? bbl)
                       (print-out "EOF")
                       (set! handlers (remove self handlers))]
                      [else 
                       (print-out (subbytes bb 0 bbl))]))))
          self))

        (for/fold ([r null]) ([n nodes])
          (list* (mkhandler (second (first n)) (second n))
                 (mkhandler (fourth (first n)) (second n))
                 r))))
  (define normal-finish #f)
  (dynamic-wind
    (lambda () (void))
    (lambda ()
      (let loop ()
        (apply sync handlers)
        (unless (null? handlers)
          (loop)))
      (set! normal-finish #t))
    (lambda ()
      (unless normal-finish
        (for ([n nodes])
          (subprocess-kill (first (first n)) #f))))))

(define (main . args)
  (match args
    [(list "spawn" node-port)
       (define listener (tcp-listen (->number node-port) 4 #t))
       (write-flush (list (->number node-port)))
       (start-spawned-node-router listener)]

    [(list "launch" mod-path conf-name i)
       (startup (dynamic-require (->path mod-path) (string->symbol conf-name)) (->number i))]))
