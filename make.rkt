#lang racket/base
(require racket/file
         racket/match
         racket/system)

(define (build-docs p)
  (match (path->string p)
    [(and pp (pregexp "/*.scrbl$"))
     (system (format "scribble ~a" pp))
     (system (format "scribble --pdf ~a" pp))]
    [else #f]))

(parameterize ([current-directory "docs"])
  (system "racket diagrams.rkt")
  (find-files build-docs (current-directory)))


