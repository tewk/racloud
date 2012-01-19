#lang scribble/manual
@(require scribble/eval)
@(require racket/contract)
@(require (for-label "../racloud.rkt"))

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

@defproc[(spawn-vm-supervise-dynamic-place-at 
           [hostname string?] 
           [compute-instance-module-path module-path?]
           [compute-instance-place-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]                      
           [#:initial-message initial-message any? #f]                                            
           [#:racket-path racketpath string-path? (racket-path)]                                          
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]                                           
           [#:racloud-launch-path racloudpath string-path? (->string racloud-launch-path)]                                       
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place-supervisor%?]{
Spawns a new remote vm node at @racket[hostname] with one compute instance place.
}

@defproc[(spawn-vm-supervise-place-thunk-at 
           [hostname string?] 
           [compute-instance-module-path module-path?]
           [compute-instance-thunk-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]                      
           [#:initial-message initial-message any? #f]                                            
           [#:racket-path racketpath string-path? (racket-path)]                                          
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]                                           
           [#:racloud-launch-path racloudpath string-path? (->string racloud-launchpath)]                                       
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place-supervisor%?]{
Spawns a new remote vm node at @racket[hostname] with one compute instance place.
}

@defproc[(supervise-process-at [hostname string?] 
                               [commandline-argument string?] ...+
                               [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]) remote-process-supervisor%?]{
Spawns an attached external process at host @racket[hostname].
}

@defproc[(supervise-named-dynamic-place-at 
           [remote-vm remote-vm?]
           [place-name symbol?]
           [compute-instance-module-path module-path?]
           [compute-instance-place-function-name symbol?]
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place-supervisor%?]{
Creates a new place on the @racket[remote-vm] by dynamic-place invoking @racket[compute-instance-place-function-name]
from the module @racket[compute-instance-module-path].
}

@defproc[(supervise-named-place-thunk-at 
           [remote-vm remote-vm?]
           [place-name symbol?]
           [compute-instance-module-path module-path?]
           [compute-instance-thunk-function-name symbol?]
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place-supervisor%?]{
Creates a new place on the @racket[remote-vm] by executing the thunk @racket[compute-instance-place-function-name]
from the module @racket[compute-instance-module-path].
}

@defform[(every-seconds seconds body ....)]{
Executes the body expressions every @racket[seconds].
}

@defform[(after-seconds seconds body ....)]{
Executes the body expressions after a delay of @racket[seconds] from the start of the event loop..
}
