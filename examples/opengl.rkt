#lang racket/gui/easy

(require racket/class
         racket/draw
         sgl)

(define @rot (@ 0))
(define scale
  (inexact->exact
   (gui:get-display-backing-scale)))

(define ((compose-mixins . mixins) %)
  (let loop ([% %] [mixins mixins])
    (cond
      [(null? mixins) %]
      [else (loop
             ((car mixins) %)
             (cdr mixins))])))

(define ((mix-gl-config gl-config) %)
  (class %
    (super-instantiate
     []
     [gl-config gl-config])))

(define ((mix-mouse-events proc) %)
  (class %
    (super-new)
    (define/override (on-event e)
      (proc e))))

(define (rectangle dc rot)
  (define-values (w h)
    (send dc get-size))
  (define gl-context
    (send dc get-gl-context))
  (send gl-context
        call-as-current
        (lambda ()
          (gl-clear-color 0.0 0.0 0.0 1.0)
          (gl-clear 'color-buffer-bit)

          (gl-enable 'multisample)
          (gl-viewport 0 0 (* scale w) (* scale h))

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

          (send gl-context swap-buffers))))

(void
 (render
  (window
   #:title "OpenGL"
   #:size '(800 600)
   (canvas
    #:style '(gl no-autoclear)
    #:mixin
    (compose-mixins
     (mix-gl-config
      (let ([cfg (new gl-config%)])
        (begin0 cfg
          (send cfg set-multisample-size 4))))
     (mix-mouse-events
      (let ([drag-start-x #f]
            [drag-start-rot #f])
        (lambda (e)
          (case (send e get-event-type)
            [(left-down)
             (set! drag-start-x (send e get-x))
             (set! drag-start-rot (obs-peek @rot))]
            [(motion)
             (when (send e get-left-down)
               (define x (send e get-x))
               (@rot . <~ . (lambda (_rot)
                              (- drag-start-rot
                                 (/ (- x drag-start-x) 10.0)))))])))))
    @rot
    rectangle))))
