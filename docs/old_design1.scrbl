#lang scribble/manual
@(require scribble/eval)
@(require racket/contract)
@(require (for-label "../racloud.rkt"))

@title[#:tag "racloud"]{Racloud}

Design Pattern 1: (examples echo, robotics1, and robotics2)

A Racloud distributed system is made up of a set of Racket instances, called nodes, running on machines.
Each node instance has one or more compute instances (places). The nodes are described by a config object.

A config object is a list of node-configs.

@racket[
  (list                                                                                                       
    (node-config "localhost" "6431" 1 ssh-path racketpath racloudpath controlpath 'control-node controlpath 'config)
    (node-config "localhost" "6432" 1 ssh-path racketpath racloudpath controlpath 'executor-node controlpath 'config))]

Each node-config is made up of a hostname that node will run on @tt{"localhost"}.
A control port that other nodes will use to communicate with the node @tt{"6431"}.
The number of compute instances that node has @t{1}.
The ssh-path @tt{/usr/bin/ssh}.
The racket-path @tt{/usr/bin/racket}.
The racloudpath @tt{/home/tewk/racloud/racloud.rkt}.
The module-path for the compute instances @tt{/home/tewk/racloud/examples/robotics1/control-executor.rkt}.
The function name for the compute instances. @racket['control-node].
(Right now all the compute instances with in a single node run the same code. This could easily be changed.)
(module-path function-name points to a function that takes a single argument.)
The module-path for the module which contains the config object @tt{/home/tewk/racloud/examples/robotics1/control-executor.rkt}.
The object name for the config object @racket['config].

Each compute instance has an integer identifier starting from @racket[0]. In the example above the first node has
one compute instance @racket[(0)] and the second node has one compute instance @racket[(1)].

@defproc[(launch-config [config listof node-config?]) void?]{
  For each @racket[node-config] in @racket[config], @racket[launch-config] sshs to the hostname and launches Racloud on that node.
  Racloud starts up and listens on the specified port for messages.  Each node also launches its compute instances and connects to 
  all the other nodes in config object.}

@defproc[(dcg-get-cg [ch place-channel?]) dcg?]{
A distributed-communication-group (dcg) is created between all the compute instances.  A compute instance receives its dcg handle
by calling @racket[(dcg-get-cg ch)] on the channel @racket[ch] argument to its function definition.
}

@defproc[(dch-send [dcg-handle dcg?] [dest non-negative-integer?] [message any?]) void?]{
Sends a @racket[message] over the @racket[dcg] to the compute instance with id @racket[dest].
}

@defproc[(dch-recv [dcg-handle dcg?]) any?]{
Receives the next message sent to the compute instance from the dcg network.
}

@defproc[(dcg-send-new-dchannel [dcg-handle dcg?] [dest non-negative-integer?]) dchannel?]{
Creates a new point-to-point distributed channel (@racket[dchannel]) between the current compute-instance and the @racket[dest] compute instance.
One endpoint of the new @racket[dchannel] is sent to the @racket[dest] compute instance.
The other endpoint is returned from the @racket[dcg-send-new-dchannel] function.
}

@defproc[(dchannel-put [dch dchannel?] [msg any?]) void?]{
Sends a message across the dchannel.
}

@defproc[(dchannel-get [dch dchannel?]) any?]{
Receives a message from the dchannel.
}


