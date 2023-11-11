#lang racket

(require racket/gui/easy
         racket/gui/easy/operator
         sgl
         racket/draw)

(define @rot (@ 0))

(void
 (render
  (window
   #:title "OpenGL in racket/gui/easy"

   (canvas @rot
           (λ (dc rot)
             (let ([gl (send dc get-gl-context)])
               (send gl call-as-current
                     (thunk
                      (gl-clear-color 0.0 0.0 0.0 1.0)
                      (gl-clear 'color-buffer-bit)

                      (gl-enable 'multisample)

                      (let-values ([(x0 y0) (send dc get-origin)]
                                   [(w h) (send dc get-size)])
                        (gl-viewport 0 0 w h))

                      (gl-color 1.0 1.0 1.0)

                      (gl-push-matrix)
                      (gl-rotate rot 0 0 1)

                      (gl-begin 'line-loop)
                      (gl-vertex -0.5 -0.5 0)
                      (gl-vertex 0.5 -0.5 0)
                      (gl-vertex 0.5 0.5 0)
                      (gl-vertex -0.5 0.5 0)
                      (gl-end)

                      (gl-pop-matrix)

                      (send gl swap-buffers)))))

           #:style '(gl no-autoclear)

           #:mixin (λ (%)
                     (class %
                       (super-instantiate ()
                                          (gl-config
                                           (let ([cfg (new gl-config%)])
                                             (send cfg set-multisample-size 4)
                                             cfg)))

                       (define drag-start-x #f)
                       (define drag-start-rotation #f)

                       (define/override (on-event e)
                         (case (send e get-event-type)
                           [(left-down)
                            (set! drag-start-x (send e get-x))
                            (set! drag-start-rotation (obs-peek @rot))]

                           [(motion)
                            (when (send e get-left-down)
                              (let ([new-x (send e get-x)])
                                (<~ @rot (λ (oldrot) (- drag-start-rotation
                                                        (/ (- new-x drag-start-x) 10.0))))))]))))))))
