#lang racket/base
(require racket/list
         racket/match
         racket/tcp
         racket/place
         racket/class
         racket/trait
         racket/udp
         racket/runtime-path)

(provide ssh-bin-path
         racket-path
         racloud-path
         racloud-launch-path
         get-current-module-path

         ;; New Design Pattern 2 API
         master-event-loop
         spawn-vm-supervise-dynamic-place-at
         spawn-vm-supervise-place-thunk-at
         supervise-named-place-thunk-at
         supervise-process-at
         every-seconds
         after-seconds
         restart-every
         after-seconds%

         connect-to-named-place


         ;; low-level API
         spawn-remote-racket-vm
         vm-send-exit
         remote-place 
         remote-dynamic-place 
         ll-channel-get
         ll-channel-put
         write-flush

         ;;
         start-spawned-node-router

         ;;Coercion Routines
         ->string
         ->path
         ->number
         ->length


         ;; Old Design Pattern 1 API
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
         startup-config
         (struct-out node-config)
         (struct-out dcg)

         event-container<%>

         )

(define-runtime-path racloud-launch-path "launch.rkt")

(define DEFAULT-ROUTER-PORT 6340)

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

; find ssh-binary
(define (ssh-bin-path)
  (define (exists? paths)
    (and paths
         (for/or ([p paths]) (and (file-exists? p) p))))

  (define (fallback-paths)
    (exists?
      (case (system-type 'os)
        [(unix macosx)
          (list "/usr/local/bin/ssh" "/usr/bin/ssh" "/bin/ssh" "/opt/local/bin/ssh" "/opt/bin/ssh")]
        [(windows) #f])))

  (define (which cmd)
    (define path (getenv "PATH"))
    (and path
         (exists? (map (lambda (x) (build-path x cmd)) (regexp-split (case (system-type 'os)
                                    [(unix macosx) ":"]
                                    [(windows) "#:;"])
                                  path)))))
  (or (which "ssh")
      (fallback-paths)
      (raise "ssh binary not found")))



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
(define (socket-channel-write-flush s x)
  (write-flush x (socket-channel-out s)))
(define (socket-channel-remove-subchannel sc scid)
  (set-socket-channel-subchannels! sc (filter-map 
                                        (lambda (x) (and (not (= (car x) scid)) x)) 
                                        (socket-channel-subchannels sc))))

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
  (define nc (new node% [listen-port listener]))
  (send nc sync-events)) 
   

(define (start-node-router chan-vec)
  (define nc (new node% [chan-vec chan-vec]))
  (send nc sync-events)) 

(define backlink 
  (trait->mixin 
    (trait
      (field [router #f])
      (define/public (remove-from-router)
        (and router (send router remove-ec this)))
      (define/public (backlink _router)
        (set! router _router))
      )))
                   
(define event-container<%>
  (interface ()
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
      (init-field [parent #f])
      (field [s #f]
             [i #f]
             [o #f]
             [e #f]
             [pid #f])

      (let-values ([(_s _o _i _e) (apply subprocess #f #f #f cmdline-list)])
        (set! pid (subprocess-pid _s)) 
        (set! s _s)
        (set! o (box _o))
        (set! i (box _i))
        (set! e (box _e)))
      (printf "SPAWNED-PROCESS:~a ~a\n" pid cmdline-list)

      (define (mk-handler _port desc)
        (define port (unbox _port))
        (if port
          (wrap-evt port (lambda (e) 
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
                   (set-box! _port #f)]
                  [else                                                                                   
                   (print-out (subbytes bb 0 bbl))])])))
          #f))

      (define/public (get-pid) pid)
      (define/public (wait-for-die) (subprocess-wait s))
      (define/public (register nes)
        (for/filter/fold/cons nes ([x (list s (list o "OUT") (list e "ERR"))])
          (cond 
            [(subprocess? x) (wrap-evt s (lambda (e) 
                                             (printf "SPAWNED-PROCESS ~a DIED\n" pid)
                                             (and parent (send parent process-died this))))]
            [(list? x) (apply mk-handler x)]
            [else #f])))
      (super-new)
  )))

(define place-socket-bridge%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field pch 
                  sch 
                  id)
      (define/public (register nes)
        (cons 
          (wrap-evt 
            (if (dchannel? pch) (dchannel-ch pch) pch) 
            (lambda (e)
              ;(printf "PLACE CHANNEL TO SOCKET ~a\n" e)
              (put-msg e)))
          nes))
      (define/public (get-sc-id) id)
      (define/public (get-msg)
        (let loop ()
          (define msg (read (socket-channel-in sch)))
          (if (= (dcgm-type msg) DCGM-DPLACE-DIED)
            (loop)
          (dcgm-msg msg))))
      (define/public (put-msg msg)
        (socket-channel-write-flush sch (dcgm DCGM-TYPE-INTER-DCHANNEL id id msg)))
      (super-new)
  )))

(define node% 
  (backlink
    (class*
      object% (event-container<%>)
      (init-field [chan-vec #f])
      (init-field [listen-port #f])
      (init-field [socket-ports null])
      (init-field [sub-ecs null])
      (init-field [psbs null])
      (init-field [spawned-vms null])
      (init-field [named-places (make-hash)])
      (init-field [beacon #f])
      (field [id 0])
      (define/public (nextid)
        (set! id (add1 id))
        id)
      (define (add-socket-port pair)
        (set! socket-ports (append socket-ports (list pair))))
      (define/public (add-sub-ec ec)
        (set! sub-ecs (append sub-ecs (list ec))))
      (define (add-spawned-vm ec)
        (set! spawned-vms (append spawned-vms (list ec))))
      (define (add-psb ec)
        (set! psbs (append psbs (list ec))))
      (define (add-named-place name np)
        (hash-set! named-places (->string name) np))
      (define (named-place-lookup name)
        (hash-ref named-places (->string name) #f))
      (define (add-place-channel-socket-bridge pch sch id)
        (add-psb (new place-socket-bridge% [pch pch] [sch sch] [id id])))
      (define (forward-mesg m src-channel)
        (match m
          [(dcgm 1 #;(== DCGM-TYPE-DIE) src dest "DIE") (exit 1)]
          [(dcgm 2 #;(== DCGM-TYPE-NEW-DCHANNEL) src dest pch)
            (define d (vector-ref chan-vec dest))
            (cond
              [(socket-channel? d)
               (define ch-id (nextid))
               (socket-channel-add-subchannel d ch-id pch)
               (add-place-channel-socket-bridge pch d ch-id)
               (socket-channel-write-flush d (dcgm DCGM-TYPE-NEW-INTER-DCHANNEL src dest ch-id))]
              [(or (place-channel? d) (place? d))
               (place-channel-put d m)])]
          [(dcgm 3 #;(== DCGM-TYPE-NEW-INTER-DCHANNEL) -1 (and place-exec (list-rest type rest)) ch-id)
           (match place-exec
             [(list 'connect name) 
              (define np (named-place-lookup name))
              (cond 
                [np 
                  (define nc (new connection%
                                 [name-pl np]
                                 [ch-id ch-id]
                                 [sc src-channel]))
                  (add-sub-ec nc)]
                 
                [else
                  (socket-channel-write-flush src-channel (dcgm DCGM-TYPE-INTER-DCHANNEL ch-id ch-id 
                                                       (format "ERROR: name not found ~a" name)))])]

             [else
              (define np (new place%
                             [place-exec place-exec]
                             [ch-id ch-id]
                             [sc src-channel]))
              (match place-exec
                [(list _ _ _ name) (add-named-place name np)]
                [else (add-sub-ec np)])])]
          [(dcgm 3 #;(== DCGM-TYPE-NEW-INTER-DCHANNEL) src dest ch-id)
            (define s src-channel)
            (define d (vector-ref chan-vec dest))
            (define-values (pch1 pch2) (place-channel))
            (socket-channel-add-subchannel s ch-id pch1)
            (add-place-channel-socket-bridge pch1 s ch-id)
            (place-channel-put d (dcgm DCGM-TYPE-NEW-DCHANNEL src dest pch2))]
          [(dcgm 4 #;(== DCGM-TYPE-INTER-DCHANNEL) _ ch-id msg)
            (define pch (socket-channel-lookup-subchannel src-channel ch-id))                                  
            (cond 
              [(place-channel? pch)
                ;(printf "SOCKET to PLACE CHANNEL ~a\n" msg)                                                        
                (place-channel-put pch msg)] 
              [(is-a? pch connection%)
               (send pch forward msg)])]
          [(dcgm 6 #;(== DCGM-TYPE-SPAWN-REMOTE-PROCESS) src (list node-name node-port mod-path funcname) ch1)
           (define vm
             (new remote-node%
                  [host-name node-name]
                  [listen-port node-port]
                  [cmdline-list (list (ssh-bin-path)  node-name (racket-path) "-tm" (->string racloud-launch-path) "spawn" (->string node-port))]))
           (add-spawned-vm vm)
           (send vm launch-place 
                    (list 'dynamic-place mod-path funcname)
                    ;#:initial-message initial-message
                    #:one-sided-place ch1
                    ;#:restart-on-exit restart-on-exit
                    )]
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

      (define us-buffer (make-bytes 4096))
      (define (register-beacon nes)
        (cond 
          [(not beacon) nes]
          [(equal? #t beacon)
           (set! beacon (udp-open-socket))
           (udp-bind! beacon "255.255.255.255" DEFAULT-ROUTER-PORT)
           (wrap-evt (udp-receive!-evt beacon us-buffer) 
            (match-lambda 
              [(list count host port)
              (void)]))]))


      (define/public (register nes)
        (let*
          ([nes
            (if chan-vec
              (for/fold ([n nes]) ([x (in-vector chan-vec)])
                (cons
                  (cond
                    [(socket-channel? x)
                     (define in (socket-channel-in x))
                     (wrap-evt in (lambda (e) 
                                      ;(printf "VECTOR SOCKET MESSAGE ~a\n" e)
                                      (forward-mesg (read in) x)))]
                    [(or (place-channel? x) (place? x))
                     (wrap-evt x (lambda (e) 
                                     ;(printf "VECTOR PLACE MESSAGE ~a\n" e)
                                     (forward-mesg e x)))])
                  n))
              nes)]
           [nes 
            (if listen-port
              (cons
                (wrap-evt listen-port (lambda (e)
                  (define-values (in out) (tcp-accept listen-port))
                  (define-values (lh lp rh rp) (tcp-addresses in #t))
                  (printf "INCOMING CONNECTION ~a:~a <- ~a:~a\n" lh lp rh rp)
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
                     (wrap-evt in (lambda (e) 
                                      ;(printf "SOCKET-PORT SOCKET MESSAGE ~a\n" e)
                                      (forward-mesg (read in) x)))]
                    [(or (place-channel? x) (place? x))
                     (wrap-evt x (lambda (e) 
                                     ;(printf "SOCKET-PORT PLACE MESSAGE ~a\n" e)
                                     (forward-mesg e x)))])
                  n))
              nes)]
           [nes
            (if sub-ecs
              (for/fold ([n nes]) ([x sub-ecs])
                (send x register n))
              nes)]
           [nes
            (if psbs
              (for/fold ([n nes]) ([x psbs])
                (send x register n))
              nes)]
           [nes
            (if spawned-vms
              (for/fold ([n nes]) ([x spawned-vms])
                (send x register n))
              nes)]
           [nes (register-beacon nes)])
          nes))

      (define/public (sync-events)
        (let loop ()
          (define l (register null))
          (apply sync/enable-break l)
         
          (loop )))


      (super-new)
  )))


; currently not used
(define socket-connection%
  (backlink
    (class* object% (event-container<%>)
      (init-field [in #f]
                  [out #f])
      (define (forward-mesg x) (void))
      (define/public (register nes)
        (cons 
          (wrap-evt in (lambda (e) 
            (printf "SOCKET-CONNECTION SOCKET MESSAGE ~a\n" e)
            (forward-mesg (read in))))
          nes))
      (super-new)
  )))

(define remote-node%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field host-name)
      (init-field listen-port)
      (init-field [cmdline-list #f])
      (init-field [sc #f]) ;socket-channel
      (init-field [restart-on-exit #f])
      (field [sp #f]) ;spawned-process
      (field [id 0])
      (field [remote-places null])

      (define/public (nextid)
        (set! id (add1 id))
        id)

      (define (add-remote-place rp)
        (set! remote-places (append remote-places(list rp))))
      (define (spawn-node)
        (set! sp (new spawned-process% [cmdline-list cmdline-list] [parent this])))
      (define (setup-socket-channel)
        (define-values (in out) (tcp-connect/retry host-name listen-port))
        (set! sc (socket-channel in out null)))
      (define (restart-node)
        (spawn-node)
        (setup-socket-channel))

      (when (and cmdline-list (not sc))
        (spawn-node))
      (unless sc
        (setup-socket-channel))

      (define (find-place-by-sc-id scid)
        (for/fold ([r #f]) ([rp remote-places])
          (if (= (send rp get-sc-id) scid)
            rp
            r)))

      (define (on-socket-event e)
        (define it (read e))
        (match it
          [(dcgm 7 #;(== DCGM-DPLACE-DIED) -1 -1 ch-id)
            (printf "SPAWNED-PROCESS:~a PLACE DIED ~a:~a:~a\n" (send sp get-pid) host-name listen-port ch-id)
            (cond 
              [(find-place-by-sc-id ch-id) => (lambda (rp)
                                                (send rp place-died))]
              [else (printf "remote-place for sc-id ~a not found\n" ch-id)])]
          [(dcgm 4 #;(== DCGM-TYPE-INTER-DCHANNEL) _ ch-id msg)                                               
            (define pch (socket-channel-lookup-subchannel sc ch-id))                                  
            (cond 
              [(place-channel? pch)
                ;(printf "SOCKET to PLACE CHANNEL ~a\n" msg)                                                        
                (place-channel-put pch msg)] 
              [(is-a? pch connection%)
               (send pch forward msg)])]
          [(? eof-object?)
           (define-values (lh lp rh rp) (tcp-addresses (socket-channel-in sc) #t))
           (printf "EOF on vm socket connection pid to ~a ~a:~a CONNECTION ~a:~a -> ~a:~a\n" (send sp get-pid) host-name listen-port lh lp rh rp)
           (set! sc #f)]

          [else (printf "received message ~a\n" it)]))

      (define/public (get-log-prefix) (format "PLACE ~a:~a" host-name listen-port))
      (define/public (process-died child) 
        (printf "Remote VM pid ~a ~a:~a died \n" (send sp get-pid) host-name listen-port)
        (set! sp #f)
        (cond
          [restart-on-exit
            (cond 
              [cmdline-list
                (if (equal? restart-on-exit #t)
                  (restart-node)
                  (send restart-on-exit restart restart-node))]
              [else
                (printf "No restart cmdline arguments for ~a\n" 
                        (get-log-prefix))])]
          [else
            (printf "No restart condition for ~a\n" 
                    (get-log-prefix))]))

      (define/public (get-first-place)
        (car remote-places))
      (define/public (get-first-place-channel)
        (send (car remote-places) get-channel))

      (define/public (drop-sc-id scid)
        (socket-channel-remove-subchannel sc scid))
                     
      (define/public (launch-place place-exec #:restart-on-exit [restart-on-exit #f] #:one-sided-place [one-sided-place #f])
        (define rp (new remote-place% [vm this] [place-exec place-exec] [restart-on-exit restart-on-exit]
                        [one-sided-place one-sided-place]))
        (add-remote-place rp)
        rp)

      (define/public (remote-connect name #:restart-on-exit [restart-on-exit #f])
        (define rp (new remote-connection% [vm this] [name name] [restart-on-exit restart-on-exit]))
        (add-remote-place rp)
        rp)

      (define/public (spawn-remote-place place-exec dch)
        (define ch-id (nextid))
        (socket-channel-add-subchannel sc ch-id dch)
        (socket-channel-write-flush sc (dcgm DCGM-TYPE-NEW-INTER-DCHANNEL -1 place-exec ch-id))
        (new place-socket-bridge% [pch dch] [sch sc] [id ch-id]))

      (define/public (spawn-remote-connection name dch)
        (define ch-id (nextid))
        (socket-channel-add-subchannel sc ch-id dch)
        (socket-channel-write-flush sc (dcgm DCGM-TYPE-NEW-INTER-DCHANNEL -1 (list 'connect name) ch-id))
        (new place-socket-bridge% [pch dch] [sch sc] [id ch-id]))

      (define/public (send-exit)
        (socket-channel-write-flush sc (dcgm DCGM-TYPE-DIE -1 -1 "DIE")))

      (define/public (wait-for-die)
        (send sp wait-for-die))

      (define/public (register es)
        (let* ([es (if sp (send sp register es) es)]
               [es (for/fold ([nes es]) ([rp remote-places])
                             (send rp register nes))]
               [es (if sc (cons (wrap-evt (socket-channel-in sc) on-socket-event) es) es)]
               [es (if (and restart-on-exit
                            (not (equal? restart-on-exit #t)))
                       (send restart-on-exit register es)
                       es)])
          es))

      (super-new)
      )))

(define (vm-send-exit vm) (send vm send-exit))

(define remote-place%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field vm)
      (init-field [place-exec #f])
      (init-field [restart-on-exit #f])
      (init-field [one-sided-place #f])
      (init-field [on-channel/2 #f])
      (field [psb #f])
      (field [pc #f])
      (field [rpc #f])
      (field [running #f])
      (field [k #f])
      (field [handle-channel #t])

      (cond 
        [one-sided-place
          (set! rpc one-sided-place)]
        [else
          (define-values (pch1 pch2) (place-channel)) 
          (set! rpc pch1)
          (set! pc pch2)])

      (set! psb (send vm spawn-remote-place place-exec rpc))

      (define (restart-place)
        (send vm drop-sc-id (send psb get-sc-id))
        (set! psb (send vm spawn-remote-place place-exec rpc)))

      (define/public (stop) (void))
      (define/public (get-channel) pc)
      (define/public (set-on-channel/2! proc) (set! on-channel/2 proc))
      (define/public (get-sc-id) (send psb get-sc-id))
      (define/public (set-handle-channel! x) (set! handle-channel x))
      (define/public (place-died)
        (cond
          [restart-on-exit
                (if (equal? restart-on-exit #t)
                  (restart-place)
                  (send restart-on-exit restart restart-place))]
          [else
            (printf "No restart condition for ~a:~a\n" 
                    (send vm get-log-prefix)
                    (send psb get-sc-id))]))
      (define (on-channel-event e)
        (printf "~a ~a\n" (send vm get-log-prefix) e))
      (define/public (register es)
        (let* ([es (if (and handle-channel pc)
                       (cons (wrap-evt pc
                                            (cond
                                              [k
                                               (lambda (e)
                                                 (call-with-continuation-prompt (lambda ()
                                                   (begin0
                                                     (k e)
                                                     (set! k #f)))))]
                                              [on-channel/2
                                               (lambda (e)
                                                 (on-channel/2 pc e))]
                                              [else
                                               on-channel-event])) es) 
                       es)]
               [es (send psb register es)]
               [es (if (and restart-on-exit
                            (not (equal? restart-on-exit #t)))
                       (send restart-on-exit register es)
                       es)])
          es))
      (define/public (set-continuation _k) (set! k _k))

      (define/public (get-msg) (send psb get-msg))
      (define/public (put-msg msg) (send psb put-msg msg))

      (super-new)
      )))

(define remote-connection%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field vm)
      (init-field name)
      (init-field [restart-on-exit #f])
      (init-field [on-channel/2 #f])
      (field [psb #f])
      (field [pc #f])
      (field [running #f])
      (field [k #f])

      (define-values (pch1 pch2) (place-channel))
      (set! psb (send vm spawn-remote-connection name pch1))
      (set! pc pch2)

      (define/public (stop) (void))
      (define/public (get-channel) pc)
      (define/public (set-on-channel/2! proc) (set! on-channel/2 proc))
      (define/public (get-sc-id) (send psb get-sc-id))
      (define/public (place-died)
            (printf "No restart condition for ~a:~a\n" 
                    (send vm get-log-prefix)
                    (send psb get-sc-id)))
      (define (on-channel-event e)
        (printf "~a ~a\n" (send vm get-log-prefix) e))
      (define/public (register es)
        (let* ([es (if pc (cons (wrap-evt pc
                                            (cond
                                              [k
                                               (lambda (e)
                                                 (call-with-continuation-prompt (lambda ()
                                                   (begin0
                                                     (k e)
                                                     (set! k #f)))))]
                                              [on-channel/2
                                                (lambda (e)
                                                  (on-channel/2 pc e))]
                                              [else
                                               on-channel-event])) es) es)]
               [es (send psb register es)])
          es))
      (define/public (set-continuation _k) (set! k _k))

      (define/public (get-msg) (send psb get-msg))
      (define/public (put-msg msg) (send psb put-msg msg))

      (super-new)
      )))

(define place%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field place-exec)
      (init-field ch-id)
      (init-field sc)
      (field [pd #f])
      (field [psb #f])
      (field [running #f])
      (define (default-on-place-dead e)
        (set! pd #f)
        (set! psb #f)
        (socket-channel-write-flush sc (dcgm DCGM-DPLACE-DIED -1 -1 ch-id))
        (socket-channel-remove-subchannel sc ch-id))

      (init-field [on-place-dead default-on-place-dead])

      (set! pd 
        (match place-exec
          ;place% is a named place
          [(list 'dynamic-place place-path place-func name)
            (dynamic-place (->path place-path) place-func)]
          [(list 'place place-path place-func name)
            ((dynamic-require (->path place-path) place-func))]
          ;place% is a single connected place
          [(list 'dynamic-place place-path place-func)
            (dynamic-place (->path place-path) place-func)]
          [(list 'place place-path place-func)
            ((dynamic-require (->path place-path) place-func))]))

      (socket-channel-add-subchannel sc ch-id pd)
      (set! psb (new place-socket-bridge% [pch pd] [sch sc] [id ch-id]))

      (define/public (get-channel) pd)
      (define/public (stop)
        (cond
          [pd 
            (place-kill pd)
            (set! pd #f)]
          [else (void)])) ;send place not running message

      (define/public (register es)
        (let* ([es (if pd (cons (wrap-evt (place-dead-evt pd) on-place-dead) es) es)]
               [es (if psb (send psb register es) es)])
          es))
      (super-new)
      )))

(define connection%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field name-pl)
      (init-field ch-id)
      (init-field sc)
      (field [psb #f])

      (define-values (pch1 pch2) (place-channel))

      (define name-ch (send name-pl get-channel))

      (init-field [on-place-dead #f])

      (socket-channel-add-subchannel sc ch-id this)
      (set! psb (new place-socket-bridge% [pch pch1] [sch sc] [id ch-id]))

      (define/public (forward msg)
        (place-channel-put name-ch (list msg pch2)))

      (define/public (put msg)
        (socket-channel-write-flush sc (dcgm DCGM-TYPE-INTER-DCHANNEL ch-id ch-id msg)))
      (define/public (register es) (send psb register es))

      (super-new)
      )))


(define (ll-channel-put ch msg) (send ch put-msg msg))
(define (ll-channel-get ch) (send ch get-msg))

(define respawn-and-fire%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field seconds)
      (init-field thunk)
      (field [fire-time (current-inexact-milliseconds)])

      (define/public (register es)
        (if fire-time 
          (cons 
            (wrap-evt (alarm-evt fire-time)
                        (lambda (x) 
                          (set! fire-time (+ (current-inexact-milliseconds) (* seconds 1000)))
                          (thunk)))
            es)
          es))

      (super-new)
      )))

(define after-seconds%
  (backlink
    (class* 
      object% (event-container<%>)
      (init-field seconds)
      (init-field thunk)
      (init-field [fire-time (+ (current-inexact-milliseconds) (* seconds 1000))])

      (define/public (register es)
        (if fire-time 
          (cons 
            (wrap-evt (alarm-evt fire-time)
                        (lambda (x)
                          (set! fire-time #f)
                          (call-with-continuation-prompt thunk)))
            es)
          es))

      (super-new)
      )))

(define restarter%
    (class*
      after-seconds% (event-container<%>)
      (inherit-field seconds thunk fire-time)
      (init-field [retry #f])
      (super-new [fire-time #f] [thunk #f])
      (init-field [retry-reset (* 2 seconds)])
      (field [last-attempt 0])
      (field [retries 0])
      (define/public (restart restart-func)
        (printf "Here I am ~a\n" retry)
        (cond
          [(and retry (>= retries retry))
           (printf "Already retried to restart ~a times\n" retry)]
          [(> (- (current-inexact-milliseconds) last-attempt) (* seconds 1000))
           (when (> (- (current-inexact-milliseconds) last-attempt) (* retry-reset 1000))
             (set! retries 0))
           (set! last-attempt (current-inexact-milliseconds))
           (set! retries (+ 1 retries))
           (set! fire-time #f)
           (restart-func)]
          [else
            (set! thunk (lambda () (restart restart-func)))
            (set! fire-time (+ (current-inexact-milliseconds) (* seconds 1000)))]))
      ))


(define (startup-config conf conf-idx)
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

(define (supervise-named-place-thunk-at vm name place-path place-func 
                            #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:restart-on-exit [restart-on-exit #f])
    (send vm launch-place 
        (list 'place (->string place-path) place-func (->string name))
        ;#:initial-message initial-message
        #:restart-on-exit restart-on-exit
        ))
(define (spawn-vm-supervise-dynamic-place-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:racloud-launch-path [racloudlaunchpath (->string racloud-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
    (spawn-vm-supervise-place-at/exec host (list 'dynamic-place (->string place-path) place-func) #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:racloud-launch-path racloudlaunchpath
                            #:restart-on-exit restart-on-exit))

(define (spawn-vm-supervise-place-thunk-at host place-path place-func #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:racloud-launch-path [racloudlaunchpath (->string racloud-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
    (spawn-vm-supervise-place-at/exec host (list 'place (->string place-path) place-func) #:listen-port listen-port
                            #:initial-message initial-message
                            #:racket-path racketpath
                            #:ssh-bin-path sshpath
                            #:racloud-launch-path racloudlaunchpath
                            #:restart-on-exit restart-on-exit))

(define (spawn-vm-supervise-place-at/exec host place-exec #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                            #:initial-message [initial-message #f]
                            #:racket-path [racketpath (racket-path)]
                            #:ssh-bin-path [sshpath (ssh-bin-path)]
                            #:racloud-launch-path [racloudlaunchpath (->string racloud-launch-path)]
                            #:restart-on-exit [restart-on-exit #f])
  (define vm (spawn-remote-racket-vm host
                                     #:listen-port listen-port
                                     #:racket-path racketpath
                                     #:ssh-bin-path sshpath
                                     #:racloud-launch-path racloudlaunchpath))
  (define dp 
    (send vm launch-place 
        place-exec
        ;#:initial-message initial-message
        #:restart-on-exit restart-on-exit
        ))

  vm)

(define (master-event-loop #:listen-port [listen-port DEFAULT-ROUTER-PORT] . event-containers)
  (define listener (tcp-listen listen-port 4 #t))
  (define nc (new node% [listen-port listener]))
  (for ([ec event-containers])
    (send nc add-sub-ec ec))
  (send nc sync-events)) 


(define (spawn-remote-racket-vm host #:listen-port [listen-port DEFAULT-ROUTER-PORT]
                                     #:racket-path [racketpath (racket-path)]
                                     #:ssh-bin-path [sshpath (ssh-bin-path)]
                                     #:racloud-launch-path [racloudlaunchpath (->string racloud-launch-path)])
  (new remote-node%
       [host-name host]
       [listen-port listen-port]
       [cmdline-list (list sshpath host racketpath "-tm" racloud-launch-path "spawn" (->string listen-port))]))

(define (remote-dynamic-place remote-vm place-path place-func)
  (send remote-vm launch-place (list 'dynamic-place (->string place-path) place-func)))

(define (remote-place remote-vm place-path place-func)
  (send remote-vm launch-place (list 'place (->string place-path) place-func)))

(define-syntax-rule (every-seconds _seconds _body ...)
  (new respawn-and-fire% [seconds _seconds] [thunk (lambda () _body ...)]))

(define-syntax-rule (after-seconds _seconds _body ...)
  (new after-seconds% [seconds _seconds] [thunk (lambda () _body ...)]))

(define (connect-to-named-place vm name)
  (send vm remote-connect name))

(define (restart-every seconds #:retry [retry #f] #:on-fail-email [fail-email-address #f])
  (new restarter% [seconds seconds] [retry retry]))




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
                        racloud-launch-path
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
            (wrap-evt port
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
        (apply sync/enable-break handlers)
        (unless (null? handlers)
          (loop)))
      (set! normal-finish #t))
    (lambda ()
      (unless normal-finish
        (for ([n nodes])
          (printf "Killing ~a\n" n)
          (define out (third (first n)))
          (with-handlers ([exn:fail? (lambda (e) (printf "Error sending Ctrl-C: ~a\n" e))])
            (write-byte 3 out)
            (flush-output out)
            (sleep))
          (subprocess-kill (first (first n)) #f))))))
