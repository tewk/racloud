#lang scribble/manual
@(require scribble/eval)
@(require racket/contract)
@(require (for-label "racloud.rkt"))

@title[#:tag "racloud"]{Racloud}

Racloud is a prototype of a distributed computing framework for
Racket.

Its use is predicated on a couple assumptions:

@itemlist[
@item{ .ssh/config and authorized_keys are configured correctly to allow passwordless connection
to remote hosts using public key authentication.}
@item{Racket is installed in the same location on all nodes in the distributed network.}
@item{The same user account is used across all nodes in the distributed network.}
@item{The racloud code and user code of each machine is identical.}
]

As Racloud matures some of the last three assumptions will be eliminated.

Design Pattern 2: (examples robotics3)

@defproc[(master-event-loop [ec events-container?] ...+) void?]{
Waits for an one of many events to become ready.  Endless loop.
}

@defproc[(spawn-vm-supervise-dynamic-place-at [hostname string?] 
                             [compute-instance-module-path module-path?]
                             [compute-instance-function-name symbol?]
                             [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]                      
                             [#:initial-message initial-message any? #f]                                            
                             [#:racket-path racketpath string-path? (racket-path)]                                          
                             [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]                                           
                             [#:racloud-launch-path racloudpath string-path? (->string racloud-launch-path)]                                       
                             [#:restart-on-exit restart-on-exit boolean? #f]) remote-place-supervisor%?]{
Spawns a new singleton node with one compute instance. This new compute instance is an orphan.  It is not
connected to any @racket[dcg].
}

@defproc[(spawn-vm-supervise-place-thunk-at [hostname string?] 
                             [compute-instance-module-path module-path?]
                             [compute-instance-function-name symbol?]
                             [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]                      
                             [#:initial-message initial-message any? #f]                                            
                             [#:racket-path racketpath string-path? (racket-path)]                                          
                             [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]                                           
                             [#:racloud-launch-path racloudpath string-path? (->string racloud-launchpath)]                                       
                             [#:restart-on-exit restart-on-exit boolean? #f]) remote-place-supervisor%?]{
Spawns a new singleton node with one compute instance. This new compute instance is an orphan.  It is not
connected to any @racket[dcg].
}

@defproc[(supervise-process-at [hostname string?] 
                               [commandline-argument string?] ...+
                               [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]) remote-process-supervisor%?]{
Spawns a new singleton node with an attached external process. This new node is an orphan.  It is not
connected to any @racket[dcg].
}
