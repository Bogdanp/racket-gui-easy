#lang racket/base

(require plot
         racket/gui/easy
         racket/gui/easy/operator)

(define (generate-data)
  (for/list ([x (in-range 100)])
    (list x (random 0 100))))

(define @data (@ (generate-data)))

(thread
 (λ ()
   (let loop ()
     (@data . := . (generate-data))
     (sleep 1)
     (loop))))

(render
 (window
  #:size '(1280 960)
  (dyn-view
   @data
   (λ (data)
     (snip data (λ (data w h)
                  (plot-snip
                   #:width w
                   #:height h
                   (lines data))))))))
