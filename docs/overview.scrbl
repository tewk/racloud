#lang scribble/manual
@(require scribble/eval
          scribble/struct                                                                                    
          scribble/decode
          racket/contract
          "../racloud.rkt"
          racket/class
          "diagrams.rkt"
          scriblib/figure)
@(require (for-label "../racloud.rkt" racket/class))

@(define evaler (make-base-eval))                                                                         
@(interaction-eval #:eval evaler (require "../racloud.rkt" racket/class)) 

@title[#:tag "racloud-design-overview"]{Racloud Design Overview}

Racloud's distrbuted computing design is centered around machine nodes
that do computation.  A node has one place at startup, the message
router. See @figure-ref["node-places"]. The message router listens on a
TCP port for incoming connectons. Additional compute places can be
spawned within the node by sending place spawn request messages to the
message router.

@;@figure["node-places" "Racloud Node Places"
@(centered @(node-diagram))
@;]

Racloud commands that spawn remote nodes and places return controller
objects that allow commands and messages to be communicated to the
remote controlled objects.  In @Figure-ref["interfaces"], when node A
spawns a new node B, A is given a @racket[remote-node%] object with
which to control B.  Consequently, B is created with a @racket[node%]
object that is connected to A's @racket[remote-node%] controller via a
TCP socket connection. B's @racket[node%]   object is the message
router for the new node B.  A can then use its @racket[remote-node%]
controller to spawn a new place on node B. Upon successful spawning of
the new place on B, A is returned a @racket[remote-place%] controller
object.  On node B, a new place is spawned and a @racket[place%]
object is attached to B's @racket[node%] message-router.  Remotely
spawned places are private.  Only the node that spawned the place can
communicate with it.  Racloud also provides named-places.  Named
places are labeled with a name when they are created.  Any node can
connected to a named place by specifing the node, port, and label to
connect to.  The @racket[remote-connection%] object represents a
connection to a named place.

@figure["interfaces" "Controller - Controlled Pairs"
@(centered @(client-server-grid))
]

@Figure-ref["big-picture"] shows the layout of the internal objects in
a simple three node distributed system.  The node at the top of the
figure is the original node spawned by the user.  Early in the
instanciation of the top node two additional nodes are spawned, 1 and
2. Then two places are spawned on each of node 1 and node 2.  The
instanciation code of the top node ends with a call to a
@racket[master-event-loop] form.  The @racket[master-event-loop]
contains the remote-node% instances and the after-seconds and
every-seconds event responders.  Event responders which execute when
specific events occur or messages arrive from nodes 1 and 2.


@figure["big-picture" "Three Node Distributed System"
@(centered @(big-picture))
]

