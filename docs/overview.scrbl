#lang scribble/manual
@(require scribble/eval
          scribble/struct                                                                                    
          scribble/decode
          racket/contract
          "../racloud.rkt"
          racket/class
          "diagrams.rkt"
          (only-in slideshow/pict scale)
          scriblib/figure)
@(require (for-label "../racloud.rkt" racket/class))

@(define evaler (make-base-eval))                                                                         
@(interaction-eval #:eval evaler (require "../racloud.rkt" racket/class)) 

@title[#:tag "racloud-design-overview"]{Racloud Design Overview}

Racloud's distributed computing design is centered around machine nodes
that do computation.  The user/programmer configures a new distributed
system using a declarative syntax and callbacks.  A node begins life
with one initial place, the message router. See
@figure-ref["node-places"]. Once the node has been configured the
message router is activated by calling the @racket[master-event-loop]
function. The message router listens on a TCP port for incoming
connections from other nodes in the distributed system. Compute 
places can be spawned within the node by sending place-spawn
request messages to the node's message router.

@figure["node-places" "Racloud Node Places"
@(centered @(scale @(node-diagram) 0.75))
]

@include-section["example_named.scrbl"]

@section{Internal Design}
Racloud commands, which spawn remote nodes and places, return controller
objects that allow commands and messages to be communicated to the
remote controlled objects.  In @Figure-ref["interfaces"], when node A
spawns a new node B, A is given a @racket[remote-node%] object with
which to control B.  Consequently, B is created with a @racket[node%]
object that is connected to A's @racket[remote-node%] controller via a
TCP socket connection. B's @racket[node%] object is the message
router for the new node B.  A can then use its @racket[remote-node%]
controller to spawn a new place on node B. Upon successful spawning of
the new place on B, A is returned a @racket[remote-place%] controller
object.  On node B, a new place is spawned and a @racket[place%]
object is attached to B's @racket[node%] message-router.  Remotely
spawned places are private.  Only the node that spawned the place can
communicate with it.  Racloud also provides named-places.  Named
places are labeled with a name when they are created.  Any node can
connected to a named place by specifying the node, port, and label to
connect to.  The @racket[remote-connection%] object represents a
connection to a named place.

@figure["interfaces" "Controller - Controlled Pairs"
@(centered @(scale @(client-server-grid) 0.75))
]

@Figure-ref["big-picture"] shows the layout of the internal objects in
a simple three node distributed system.  The node at the top of the
figure is the original node spawned by the user.  Early in the
instantiation of the top node two additional nodes are spawned, 1 and
2. Then two places are spawned on each of node 1 and node 2.  The
instantiation code of the top node ends with a call to a
@racket[master-event-loop] form.  The @racket[master-event-loop]
contains the @racket[remote-node%] instances and the @racket[after-seconds] and
@racket[every-seconds] event responders.  Event responders which execute when
specific events occur or messages arrive from nodes 1 and 2.


@figure["big-picture" "Three Node Distributed System"
@(centered @(scale @(big-picture) 0.75))
]
