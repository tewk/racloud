#lang scribble/manual
@(require scribble/eval
          scribble/struct                                                                                    
          scribble/decode
          racket/contract
          "../racloud.rkt"
          racket/class)
@(require (for-label "../racloud.rkt" racket/class))


@(define evaler (make-base-eval))                                                                         
@(interaction-eval #:eval evaler (require "../racloud.rkt" racket/class)) 

@title[#:tag "racloud"]{Racloud}

@defmodule["../racloud.rkt"]

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
The @racket[master-event-loop] procedure constructs a @racket[node%] instance and adds all the declared
@racket[events-container<%>]s to the @racket[node%] and then calls the never ending loop @racket[sync-events]
method which handles the events for the node.
}
@(define (p . l) (decode-paragraph l))
@(define spawn-vm-note
    (make-splice                                                                                              
     (list                                                                                                    
      @p{This function returns a @racket[remote-node%] instance not a @racket[remote-place%]}
      @p{Call @racket[(send vm get-first-place)] to obtain the @racket[remote-place%] instance.})) )

@defproc[(spawn-vm-supervise-dynamic-place-at 
           [hostname string?] 
           [compute-instance-module-path module-path?]
           [compute-instance-place-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]                      
@;           [#:initial-message initial-message any? #f]                                            
           [#:racket-path racketpath string-path? (racket-path)]                                          
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]                                           
           [#:racloud-launch-path racloudpath string-path? (->string racloud-launch-path)]                                       
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place?]{
Spawns a new remote vm node at @racket[hostname] with one compute instance place.
@|spawn-vm-note|
}

@defproc[(spawn-vm-supervise-place-thunk-at 
           [hostname string?] 
           [compute-instance-module-path module-path?]
           [compute-instance-thunk-function-name symbol?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]                      
@;           [#:initial-message initial-message any? #f]                                            
           [#:racket-path racketpath string-path? (racket-path)]                                          
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]                                           
           [#:racloud-launch-path racloudpath string-path? (->string racloud-launchpath)]                                       
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place%?]{
Spawns a new remote vm node at @racket[hostname] with one compute instance place.
@|spawn-vm-note|
}

@defproc[(spawn-remote-racket-vm 
           [hostname string?]
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]                      
           [#:racket-path racketpath string-path? (racket-path)]                                          
           [#:ssh-bin-path sshpath string-path? (ssh-bin-path)]                                           
           [#:racloud-launch-path racloudpath string-path? (->string racloud-launchpath)]) remote-node%?]{
Spawns a new remote vm node at @racket[hostname] and returns a @racket[remote-node%] handle.
}

@defproc[(supervise-process-at 
           [hostname string?] 
           [commandline-argument string?] ...+
           [#:listen-port port non-negative-integer? DEFAULT-ROUTER-PORT]) remote-process%?]{
Spawns an attached external process at host @racket[hostname].
}

@defproc[(supervise-named-dynamic-place-at 
           [remote-vm remote-vm?]
           [place-name symbol?]
           [compute-instance-module-path module-path?]
           [compute-instance-place-function-name symbol?]
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place%?]{
Creates a new place on the @racket[remote-vm] by dynamic-place invoking @racket[compute-instance-place-function-name]
from the module @racket[compute-instance-module-path].
}

@defproc[(supervise-named-place-thunk-at 
           [remote-vm remote-vm?]
           [place-name symbol?]
           [compute-instance-module-path module-path?]
           [compute-instance-thunk-function-name symbol?]
           [#:restart-on-exit restart-on-exit boolean? #f]) remote-place%?]{
Creates a new place on the @racket[remote-vm] by executing the thunk @racket[compute-instance-place-function-name]
from the module @racket[compute-instance-module-path].
}

@defform[(every-seconds seconds body ....)]{
Executes the body expressions every @racket[seconds].
}

@defform[(after-seconds seconds body ....)]{
Executes the body expressions after a delay of @racket[seconds] from the start of the event loop.
}

@defproc[(connect-to-named-place [vm remote-node%?] [name symbol?]) remote-connection%?]{
Connects to a remote vm @racket[vm] named @racket[name] and returns a @racket[remote-connection%] object.
}

@definterface[event-container<%> ()
  (defmethod (register [events (listof events?)]) (listof events?))]{
  }

@defclass[spawned-process% object% (event-container<%>)
  (defmethod (get-pid) exact-positive-integer?) ]{

@defconstructor[([cmdline-list (listof (or/c string? path?))]
                 [parent remote-node%? #f]
                 )]{
The @racket[cmdline-list] is a list command line arguments of type @racket[string] and/or @racket[path].

The @racket[parent] argument is a @racket[remote-node%] instance that will be notified when the process dies via
a @racket[(send parent process-died this)] call.
}
}

@examples[ #:eval evaler
(new spawned-process% [cmdline-list
  (list (ssh-bin-path) "localhost" (racket-path) "-tm" racloud-launch-path "spawn" (->string 6340))])
]

@defclass[place-socket-bridge% object% (event-container<%>)
  (defmethod (get-sc-id) exact-positive-integer?) ]{

@defconstructor[([pch place-channel?]
                 [sch socket-channel?]
                 [id exact-positive-integer?]
                 )]{
The @racket[pch] argument is a @racket[place-channel].  Messages received on @racket[pch] are forwarded to the 
socket-channel @racket[sch] via a @racket[dcgm] message. e.g. @racket[(socket-channel-write-flush sch (dcgm DCGM-TYPE-INTER-DCHANNEL id id msg))] The @racket[id] is a @racket[exact-positive-integer] that identifies the socket-channel subchannel for this inter-node place connection.
}
}

@defclass[node% object% (event-container<%>)]{

The @racket[node%] instance controls a racloud node. It launches compute places and routes inter-node place messages in the distributed system.  The @racket[master-event-loop] form constructs a @racket[node%] instance under the hood.

@defconstructor[([listen-port tcp-llisten-port? #f])]{
 Constructs a @racket[node%] that will listen on @racket[listen-port] for inter-node connections.}

@defmethod[(sync-events) void?]{
 Starts the never ending event loop for this racloud node.
}
}

@(define place-exec-note
    (make-splice                                                                                              
     (list                                                                                                    
      @p{The @racket[place-exec] argument describes how the remote place should be launched.}
      @itemize[@item{@racket[(list 'place place-module-path place-thunk)]}
               @item{@racket[(list 'dynamic-place place-module-path place-func)]}]
      @p{The difference between these two launching methods is that the @racket['place] versio of @racket[place-exec] expects a thunk, zero argument function, 
to be explorted by the module @racket[place-module-path].  Executing the thunk is expected to create a new place and return a place descriptor to the newly created place. The @racket['dynamic-place] version of @racket[place-exec] expects place-func to be a function taking a single argument, the initial channel argument, and calls @racket[dynamic-place] on behalf of the user and creates the new place from the @racket[place-module-path] and @racket[place-func].}
)))

@(define one-sided-note
    (make-splice                                                                                              
     (list                                                                                                    
      @p{The @racket[#:one-sided-place] argument is an internal use argument for launching remote places from within a place using the old design pattern 1.})))

@(define restart-on-exit-note
    (make-splice                                                                                              
     (list                                                                                                    
      @p{The @racket[#:restart-on-exit] boolean argument instructs the remote-place% instance to respawn the place on the remote node should it exit or terminate at any time.  This boolean needs to be expanded to a restart criteria object in the future.})))


@defclass[remote-node% object% (event-container<%>)]{

  The @racket[node%] instance controls a racloud node. It launches
  compute places and routes inter-node place messages in the distributed system.
  This is the remote api to a racloud node that is returned by @racket[spawn-remote-racket-vm], 
  @racket[spawn-vm-supervise-dynamic-place-at], and @racket[spawn-vm-supervise-place-thunk-at].

  @defconstructor[([listen-port tcp-llisten-port? #f])]{
   Constructs a @racket[node%] that will listen on @racket[listen-port] for inter-node connections.}

  @defmethod[(get-first-place) remote-place%?]{
    Returns the @racket[remote-place%] instance first created on the remote node.
  }

  @defmethod[(get-log-prefox) string?]{
    Returns @racket[(format "PLACE ~a:~a" host-name listen-port)]
  }

  @defmethod[(launch-place 
               [place-exec list?]
               [#:restart-on-exit restart-on-exit boolean? #f]
               [#:one-sided-place one-sided-place boolean? #f]) remote-place%?]{
    Launches a place on the remote node represented by this @racket[remote-node%] instance.
    @|place-exec-note|
    @|one-sided-note|
    @|restart-on-exit-note|
  }

  @defmethod[(remote-connect [name string?]) remote-connection%]{
    Connects to a named place on the remote node represented by this @racket[remote-node%] instance.
  }

  @defmethod[(send-exit) void?]{
    Sends a message instructing the remote node to exit immediately
  }
}
               
@defclass[remote-place% object% (event-container<%>)]{

The @racket[remote-place%] instance provides a remote api to a place running on a remote racloud node. It launches a compute places and routes inter-node place messages to the remote place.

@defconstructor[([vm remote-node%?]
                 [place-exec list?]
                 [restart-on-exit #f]
                 [one-sided-place #f]
                 [on-channel/2 #f])]{
 Constructs a @racket[remote-place%] instance.
 @|place-exec-note|
 @|one-sided-note|
 @|restart-on-exit-note|

 See @racket[set-on-channel/2!] for description of @racket[on-channel/2] argument.
}
 
@defmethod[(set-on-channel/2! [callback (-> channel msg void?)]) void?]{
 Installs a handler function that handles messages from the remote place.
 @;The function body can be written in direct style since @racket
 The @racket[setup/distributed-docs] uses this handle job completion messages.
}
}
@defclass[place% object% (event-container<%>)]{

The @racket[place%] instance represents a place launched on a racloud node at that node. It launches a compute places and routes inter-node place messages to the place.

@defconstructor[([vm remote-node%?]
                 [place-exec list?]
                 [ch-id exact-positive-integer?]
                 [sc socket-channel?]
                 [on-place-dead (-> event void?) default-on-place-dead])]{
 Constructs a @racket[remote-place%] instance.
 @|place-exec-note|
 The @racket[ch-id] and @racket[sc] arguments are internally used to establish routing between the remote node spawning this place and the place itself.
 The @racket[on-place-dead] callback handles the event when the newly spawned place terminates.
}
}


@defclass[connection% object% (event-container<%>)]{

The @racket[connection%] instance represents a connection to a named-place instance running on the current node. It routes inter-node place messages to the named place.

@defconstructor[([vm remote-node%?]
                 [name string?]
                 [ch-id exact-positive-integer?]
                 [sc socket-channel?])]{
 Constructs a @racket[remote-place%] instance.
 @|place-exec-note|
 The @racket[ch-id] and @racket[sc] arguments are internally used to establish routing between the remote node and this named-place.
}
}

@defclass[respawn-and-fire% object% (event-container<%>)]{

The @racket[respawn-and-fire%] instance represents a thunk that should execute every @racket[n] seconds.

@defconstructor[([seconds (and/c real? (not/c negative?))]
                 [thunk (-> void?)])]{
 Constructs a @racket[respawn-and-fire%] instance that when placed inside a @racket[master-event-loop] construct causes the supplied thunk to execute every @racket[n] seconds.
}
}

@defclass[after-second% object% (event-container<%>)]{

The @racket[after-seconds%] instance represents a thunk that should execute after @racket[n] seconds.

@defconstructor[([seconds (and/c real? (not/c negative?))]
                 [thunk (-> void?)])]{
 Constructs an @racket[after-seconds%] instance that when placed inside a @racket[master-event-loop] construct causes the supplied thunk to execute after @racket[n] seconds.
}
}




@defproc[(ssh-bin-path) string?]{
Returns the path to the ssh binary on the local system in string form.
}
@examples[ #:eval evaler
(ssh-bin-path)
]

@defproc[(racket-path) path?]{
Returns the path to the currently executing racket binary on the local system.
}
@examples[ #:eval evaler
(racket-path)
]

@defproc[(racloud-path) string?]{
Returns the path to the racloud module on the local system.
}
@examples[ #:eval evaler
(racloud-path)
]

@defform[(get-current-module-path)]{
Returns the path to the current module.
}
@examples[ #:eval evaler
(get-current-module-path)
]

@defproc[(->string) string?]{
Coerces strings, numbers, symbols, and paths to a string.
}
@examples[ #:eval evaler
(->string "hello")
(->string 1)
(->string 'bye)
(->string (build-path "ridge"))
]

@defproc[(->number) number?]{
Coerces strings, numbers, to a number.
}
@examples[ #:eval evaler
(->number "100")
(->number 1)
]

@defproc[(->path) path?]{
Coerces paths and strings to a path.
}
@examples[ #:eval evaler
(->path "/usr/bin")
(->path (build-path "ridge"))
]

@defproc[(->length) path?]{
Returns the length of strings, bytes, and lists.
}
@examples[ #:eval evaler
(->length "Boo")
(->length #"Woo")
(->length (list 1 2 3 4))
]

@defproc[(write-flush [datum any?] [port port?]) (void)]{
Writes @racket[datum] to @racket[port] and then flushes @racket[port].
}
@examples[ #:eval evaler
(write-flush "Hello Mom" (current-output-port))
]
