#lang racket
(require slideshow
         racket/draw)

(define (t/2 txt) (scale/improve-new-text (t txt) 1/2))
(define (t/3 txt) (scale/improve-new-text (t txt) 1/3))

(define (surround/box pict #:pad [pad 5])
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
               
  (define bw (+ w (* 2 pad)))
  (define bh (+ h (* 2 pad)))
  (define mw (+ bw (* 2 pad)))
  (define mh (+ bh (* 2 pad))) 
  (cc-superimpose
   (ct-superimpose
   pict
   (rectangle bw bh))
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

(define remote-place
  (surround/box 
   (vc-append
    (t/3 "remote-place%")
    (t/3 "parent-vm")
    (psb))))

(define remote-vm
(surround/box 
 (vc-append 3
            (t/2 "remote-vm%")
            (surround/box
             (t/3 "spawned-process%"))
            (t/3 "socket-channel")
            (surround/box
             (vc-append 3
                        (t/3 "remote-places")
                        
                        remote-place
                        remote-place)))))

(define (supervised-place)
  (surround/box
   (vc-append
   (t/3 "supervised-place")
   (psb))))
(define (control name)
  (surround/box
   (vc-append
   (t (format "controller-node% - ~a" name))
   (t/3 "socket-channel")
   (surround/box
    (vc-append 
     (t/2 "superivised places")
     (supervised-place)
     (supervised-place))))))

(define big-picture
(vc-append
 (surround/box 
  (vc-append
   (t "master-event-loop")
   (hc-append
    remote-vm
    remote-vm
    (box/t/2 "after-seconds")
   (box/t/2 "every-seconds"))))
 (blank 30)
(hc-append
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
  
(pict->svg/png big-picture "bigpicture")
