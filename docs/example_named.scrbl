#lang scribble/manual
@(require scribble/eval)
@(require racket/contract)
@(require (for-label "../racloud.rkt"))
@(require scriblib/figure)
@(require racket/port)
@(define (codeblockfromfile filename)
   (call-with-input-file 
     filename
     (lambda (i)
       (codeblock (port->string i))))) 

@title[#:tag "example-named"]{Example: named}

This is the documentation for the named example found in
@filepath{examples/named/master.rkt}.



@figure["named-example-master" "examples/named/master.rkt"]{
@codeblockfromfile["../examples/named/master.rkt"]}



The named example demonstrates how to launch a remote racket instance
vm instance, launch remote places on the new vm instance, and start an
event loop that monitors the remote-vm.

The @racket[spawn-remote-racket-vm] primitive connects to
@tt{"localhost"} and starts a racloud node there that listens on port
6344 for further instructions.  The handle to the new racloud node is
assigned to the @racket[remote-vm] variable. Localhost is used so that
the example can be run using only a single machine.  However localhost
can be replaced by any host with ssh publickey access and racket.  The
@racket[supervise-named-place-thunk-at] creates a new place on the
@racket[remote-vm].  The new place will be identified in the future by
its name symbol @racket['tuple-server].  A place descriptor is
expected to be returned by dynamically requiring
@racket['make-tuple-server] from the @racket[tuple-path] module and
invoking @racket['make-tuple-server].

The code for the tuple-server place exists in the file
@filepath{tuple.rkt}.  The @filepath{tuple.rkt} file contains the use of
@racket[define-named-remote-server] form, which defines a RPC server
suitiable for invocation by @racket[supervise-named-place-thunk-at].



@figure["named-example" "examples/named/tuple.rkt"]{
@codeblockfromfile["../examples/named/tuple.rkt"]}



The @racket[define-named-remote-server] form takes an identifier and a list of
custom expressions as its arguments.  From the identifier a place-thunk
function is created by prepending the @tt{make-} prefix.  In this case
@racket[make-tuple-server].  This is the compute-instance-place-function-name
given to the @racket[supervise-named-place-thunk-at] form above. The
@racket[define-state] custom form translates into a simple @racket[define]
form, which is closed over by the @racket[define-rpc] forms.

The @racket[define-rpc] form is expanded into two parts. The first part is the
client that calls the rpc function. The client function name is formed by
concatenating the @racket[define-named-remote-server] identifier
@tt{tuple-server} with the RPC function name @tt{set} to form
@racket[tuple-server-set]. The RPC client functions take a destination argument
with is a remote-place-descriptor and then the RPC function arguments. The RPC
client function sends the RPC function name, @racket[set], and the RPC
arguments to the destination by calling an internal function
@racket[named-place-channel-put]. The RPC client then calls
@racket[named-place-channel-get] to wait for the RPC response.

The second expansion part of @racket[define-rpc] is the server side of the RPC
call which is a match clause inside the server which is created in the
@racket[make-tuple-server] function.  Here the set match clause matches on @racket['set]
messages, executes the RPC call with the communicated arguments, and send the result back to 
the requester.

@figure["define-named-remote-server-expansion" "Expansion of define-named-remote-server"]{
@codeblock{
'(begin
   (require racket/place racket/match)
   (define/provide
    (tuple-server-set dest k v)
    (some-channel-put dest (list 'set k v))
    (some-channel-get dest))
   (define/provide
    (tuple-server-get dest k)
    (some-channel-put dest (list 'get k))
    (some-channel-get dest))
   (define/provide
    (make-tuple-server)
    (place
     ch
     (let ()
       (define h (make-hash))
       (let loop ()
         (define msg (place-channel-get ch))
         (define resp                                                                                                                   
           (match                                                                                                                       
            msg                                                                                                                         
            ((list (list 'set k v) src)                                                                                                 
             (define result (let () (hash-set! h k v) v))                                                                               
             (place-channel-put src result)                                                                                             
             (loop))                                                                                                                    
            ((list (list 'get k) src)                                                                                                   
             (define result (let () (hash-ref h k #f)))                                                                                 
             (place-channel-put src result)                                                                                             
             (loop))))                                                                                                                  
         (place-channel-put ch resp)                                                                                                    
         loop))))                                                                                                                       
   (void))
}
}




