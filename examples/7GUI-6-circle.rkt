#lang racket/base

(require racket/class
         racket/gui/easy
         racket/gui/easy/operator
         racket/match
         racket/math)

;; circle struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct circle (x y diameter selected?)
  #:transparent)

(define (make-circle x y)
  (circle x y 40 #f))

(define (circle-distance c x y)
  (sqrt (+ (sqr (- x (circle-x c)))
           (sqr (- y (circle-y c))))))

(define (find-closest-circle circles x y)
  (define circles&distances
    (sort
     (for/list ([c (in-list circles)])
       (cons c (circle-distance c x y)))
     #:key cdr <))
  (match circles&distances
    ['() #f]
    [`((,(and (circle _ _ d _) c) . ,dist) ,_ ...)
     (and (< dist (/ d 2)) c)]))

(define (add-circle circles x y)
  (define c (make-circle x y))
  (append circles (list c)))

(define (select-circle circles selected-c)
  (for/list ([c (in-list circles)])
    (struct-copy circle c [selected? (eq? c selected-c)])))


;; history struct ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct history (prev current next)
  #:transparent)

(define (add-history h circles)
  (match-define (history prev current _) h)
  (cond
    [(equal? circles current) h]
    [else (struct-copy history h
                       [prev (cons current prev)]
                       [current circles]
                       [next null])]))

(define (undo h)
  (match-define (history prev current next) h)
  (history (cdr prev) (car prev) (cons current next)))

(define (redo h)
  (match-define (history prev current next) h)
  (history (cons current prev) (car next) (cdr next)))


;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/obs @circles null)
(define/obs @history (history null null null))

(define r
  (render
   (window
    #:title "Circles"
    #:size '(800 600)
    (hpanel
     #:stretch '(#t #f)
     (spacer)
     (button
      "Undo"
      #:enabled? (@history . ~> . (compose1 not null? history-prev))
      (λ ()
        (@history . <~ . (λ (h)
                           (define new-h (undo h))
                           (begin0 new-h
                             (@circles . := . (history-current new-h)))))))
     (button
      "Redo"
      #:enabled? (@history . ~> . (compose1 not null? history-next))
      (λ ()
        (@history . <~ . (λ (h)
                           (define new-h (redo h))
                           (begin0 new-h
                             (@circles . := . (history-current new-h)))))))
     (spacer))
    (canvas
     @circles
     #:mixin (λ (%)
               (class %
                 (super-new)
                 (define/override (on-event e)
                   (define x (send e get-x))
                   (define y (send e get-y))
                   (case (send e get-event-type)
                     [(left-up)
                      (define closest-c
                        (find-closest-circle (obs-peek @circles) x y))
                      (define new-circles
                        (@circles . <~ . (λ (circles)
                                           (if closest-c
                                               (select-circle circles closest-c)
                                               (add-circle circles x y)))))
                      (@history . <~ . (λ (h) (add-history h new-circles)))]

                     [(right-up)
                      (define closest-c
                        (find-closest-circle (obs-peek @circles) x y))
                      (when closest-c
                        (@circles . <~ . (λ (circles) (select-circle circles closest-c)))
                        (@history . <~ . (λ (h) (add-history h (obs-peek @circles))))
                        (render (make-circle-adjuster) r)
                        (@history . <~ . (λ (h) (add-history h (obs-peek @circles)))))]))))
     (λ (dc circles)
       (define old-brush (send dc get-brush))
       (send dc set-smoothing 'aligned)
       (for ([c (in-list circles)])
         (match-define (circle x y diameter selected?) c)
         (when selected?
           (send dc set-brush (color #xEE #xEE #xEE) 'solid))
         (send dc
               draw-ellipse
               (- x (/ diameter 2))
               (- y (/ diameter 2))
               diameter diameter)
         (when selected?
           (send dc set-brush old-brush))))))))

(define (make-circle-adjuster)
  (define @circle
    (@circles . ~> . (λ (circles)
                       (or (findf circle-selected? circles)
                           (make-circle 0 0)))))
  (dialog
   (vpanel
    (text
     (@circle . ~> . (λ (c)
                       (format "Adjust diameter of circle at (~a, ~a)."
                               (circle-x c)
                               (circle-y c)))))
    (slider
     #:min-value 20
     #:max-value 200
     (@circle . ~> . circle-diameter)
     (λ (diameter)
       (@circles . <~ . (λ (circles)
                          (for/list ([c (in-list circles)])
                            (if (circle-selected? c)
                                (struct-copy circle c [diameter diameter])
                                c)))))))))
