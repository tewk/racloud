#lang racket/base
(require (for-syntax racket/base)
         (for-syntax syntax/stx)
         racket/place
         racket/match
         racket/class
         (for-syntax racket/pretty)
         "racloud.rkt")

(provide define-named-remote-server)

(define-syntax define/provide
  (syntax-rules ()
    [(_ (name x ...) body ...)
     (begin (provide name)
            (define (name x ...) body ...))]
    [(_ name val)                                                                                                 
     (begin (provide name)                                                                                    
            (define name val))]))

(define (named-place-channel-get dest)
  (cond
    [(place-channel? dest) (place-channel-get dest)]
    [else
      (call-with-composable-continuation
        (lambda (k) 
          (send dest set-continuation k)
          (abort-current-continuation (default-continuation-prompt-tag) void)))]))

(define (named-place-channel-put dest msg)
  (cond
    [(place-channel? dest) (place-channel-put dest msg)]
    [else
      (define pch (send dest get-channel))
      (place-channel-put pch msg)]))

(define-syntax (define-named-remote-server stx)
  (syntax-case stx ()
    [(_ name forms ...)
     (let ()
      (define (rpc-stc? stx)
        (equal? (syntax-e (stx-car stx)) 'define-rpc))

      (define-values (states rpcs)
        (for/fold ([states null]
                   [rpcs   null]) ([f (syntax->list #'(forms ...))])
          (cond
            [(rpc-stc? f)
             (values states (append rpcs (list f)))]
            [else
             (values (append states (list f)) rpcs)])))

      (define (id->string x)
        (symbol->string (syntax->datum x)))
      (define (string->id stx x)
        (datum->syntax stx (string->symbol x)))


      (define trans-rpcs 
        (for/list ([f rpcs])
          (syntax-case f ()
            [(_ (fname args ...) body ...)
             (with-syntax ([fname-symbol (string->id stx (format "~a-~a" (id->string  #'name) (id->string #'fname)))])

               #'(define/provide (fname-symbol dest args ...)
                     (named-place-channel-put dest (list (quote fname) args ...))
                     (named-place-channel-get dest)))])))

      (define trans-place
        (with-syntax ([(states2 ...)
                        (for/list ([s states])
                          (syntax-case s ()
                            [(_ rest ...)
                             #'(define rest ...)]))]
                      [(cases ...)
                        (for/list ([r rpcs])
                          (syntax-case r ()
                            [(_ (fname args ...) body ...)
                             (let ()
                             (with-syntax ([fname-symbol #'(quote fname)])
                               #'[(list (list fname-symbol args ...) src)
                                   (define result 
                                     (let ()
                                       body ...))
                                   (place-channel-put src result)
                                   (loop)]))]))])
        #`(place ch
            (let ()
              states2 ...
              (let loop ()
                (define msg (place-channel-get ch))
                (define resp
                  (match msg
                    cases ...
                  ))
                (place-channel-put ch resp)
                loop)
                ))))
    (with-syntax ([mkname (string->id stx (format "make-~a" (id->string #'name)))])
      (define x 
        #`(begin
          (require racket/place
                   racket/match)
          #,@trans-rpcs
          (define/provide (mkname) #,trans-place)
          (void)))
      ;(pretty-print (syntax->datum x))
      x))]))
