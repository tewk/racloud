#lang racket
(require slideshow
         racket/draw)

(define (t/2 txt) (scale/improve-new-text (t txt) 1/2))
(define (t/3 txt) (scale/improve-new-text (t txt) 1/3))

(define (surround/box pict #:pad [pad 5] #:w [bwa #f] #:h [bha #f] #:lc [lc "black"] 
                      #:s [superimposer #f] #:bg [bg "white"])
  (define pw (pict-width pict))
  (define ph (pict-height pict))
  (define r 4)
  (define-values (w h)
    (cond 
      [(pw . > . ph)
       (cond
         [((/ pw r) . > . ph)
          (values pw (/ pw r))]
         [else 
          (values pw ph)])]
      [else
       (cond
         [((/ ph r) . > . pw)
          (values (/ ph r) ph)]
         [else 
          (values pw ph)])]))
  
  (define bw (+ (or bwa w) (* 2 pad)))
  (define bh (+ (or bha h) (* 2 pad)))
  (define mw (+ bw (* 2 pad)))
  (define mh (+ bh (* 2 pad))) 
  (cc-superimpose
   ((or superimposer ct-superimpose)
    
    (cc-superimpose
     (colorize (filled-rectangle bw bh) bg)
     (colorize (rectangle bw bh) lc))
    pict)
   (colorize (rectangle mw mh)
             "white")))
(define (box/t/x x)
  (lambda (txt)
    (surround/box 
     (x txt))))
(define (box/t/2 txt) ((box/t/x t/2) txt))
(define (box/t/3 txt) ((box/t/x t/3) txt))

(define (psb)
  (box/t/3 "place-socket-bridge%"))

(define (remote-place)
  (surround/box 
   (vc-append
    (t/3 "remote-place%")
    (t/3 "parent-vm")
    (psb))))

(define (remote-vm name)
  (surround/box 
   (vc-append 3
              (t/2 (format "remote-node% - ~a" name))
              (surround/box
               (t/3 "spawned-process%"))
              (t/3 "socket-channel")
              (surround/box
               (vc-append 3
                          (t/3 "remote-places")
                          
                          (remote-place)
                          (remote-place))))))

(define (supervised-place)
  (surround/box
   (vc-append
    (t/3 "place%")
    (psb))))
(define (control name)
  (surround/box
   (vc-append
    (t (format "node% - ~a" name))
    (t/3 "socket-channel")
    (surround/box
     (vc-append 
      (t/2 "superivised places")
      (supervised-place)
      (supervised-place))))))

(define (big-picture)
  (vc-append
   (surround/box 
    (vc-append
     (t "node% - top")
     (t/2 "master-event-loop")
     (hc-append
      (remote-vm 1)
      (remote-vm 2)
      (box/t/2 "after-seconds")
      (box/t/2 "every-seconds"))))
   (blank 20)
   (hc-append 20
              (control "1")
              (control "2"))))

(define (pict->svg pict w h fn) 
  (define dc (new svg-dc% [output fn] [width w] [height h] [exists 'replace]))
  (send dc start-doc "pict->svg")
  (send dc start-page)
  (draw-pict pict dc 0 0)
  (send dc end-page)
  (send dc end-doc))

(define (pict->png pict w h fn)
  (define bm (make-object bitmap% w h))
  (define dc (new bitmap-dc% [bitmap bm]))
  (draw-pict pict dc 0 0)
  (send bm save-file fn 'png))

(define (pict->svg/png p np)
  (define w (inexact->exact (ceiling (pict-width p))))
  (define h (inexact->exact (ceiling (pict-height p))))
  (pict->svg p w h (string-append np ".svg"))
  (pict->png p w h (string-append np ".png")))

(define cc
  (list
   (list "controller" "controlled")
   (list "remote-node%" "node%")
   (list "remote-place%" "place%")
   (list "remote-connection%" "connection%")))

(define-syntax-rule (with-font dc f body ...)                                                                 
  (let ()                                                                                                     
    (define sf (send dc get-font))                                                                            
    (send dc set-font f)                                                                                      
    (begin0                                                                                                   
      (let () body ...)                                                                                       
      (send dc set-font sf)))) 

(define (gen-font dc #:size [size #f] #:face [face #f] #:family [family #f] #:style [style #f] #:weight [weight #f])
  (define f (send dc get-font))                                                                               
  (make-object font%                                                                                          
    (or size (send f get-point-size))                                            
    (or face (send f get-face))                                                  
    (or family (send f get-family))                                              
    (or style (send f get-style))                                                
    (or weight (send f get-weight))))

(define (div-current-font x)
  (define tdc (dc-for-text-size))
  (define f (send tdc get-font))
  (inexact->exact (round (/ (send f get-point-size) x))))

(define (text-extent-div txt x)
  (define tdc (dc-for-text-size))
  (define s (div-current-font x))
  (with-font tdc (gen-font tdc #:size s)
             (define-values (w h _1 _2) (send tdc get-text-extent txt))
             (values w h)))

(define (client-server-grid)
  (define-values (mw mh)
    (for/fold ([mw 0] [mh 0]) ([c cc])
      (for/fold ([mw mw] [mh mh]) ([i c])
        (define-values (w h) (text-extent-div i 1))
        (values (max mw w) (max mh h)))))
  (apply vc-append
         (apply hc-append
                (for/list ([i (first (take cc 1))])
                  (surround/box (t i) #:w mw #:h (* mh 2) #:lc "white" #:s cb-superimpose)))
         (for/list ([c (drop cc 1)])
           (apply hc-append
                  (for/list ([i c])
                    (surround/box (t/2 i) #:w mw #:h mh))))))
(define (node-diagram)
  (define (sq #:bg [bg "white"] . rest )
    (surround/box  #:w 150 #:h 150 #:bg bg
      (apply vc-append
        (map t/2 rest))))
       
  (surround/box
   (vc-append
    (t "Node")
    (vc-append
     (hc-append (sq #:bg "lawngreen" "place" "node%" "message-router")
                (sq "place" "compute place"))
     (hc-append (sq "place" "compute-place")
                (sq "place" "compute-place"))))))

;(node-diagram)

(provide node-diagram
         client-server-grid
         big-picture)

;(client-server-grid)
;(pict->svg/png (big-picture) "bigpicture")
;big-picture 
