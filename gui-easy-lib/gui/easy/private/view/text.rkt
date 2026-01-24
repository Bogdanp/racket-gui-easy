#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../class.rkt"
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 text)

(define (make-text% gui-message%)
  (class* object% (view<%>)
    (init-private-field @label @color @margin @min-size @stretch font)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @color @margin @min-size @stretch)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define color (peek @color))
      (define the-message
        (new gui-message%
             [parent parent]
             [label (peek @label)]
             [font font]
             [vert-margin v-m]
             [horiz-margin h-m]
             [min-width w]
             [min-height h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]
             [auto-resize #t]))
      (begin0 the-message
        (when color
          (send the-message set-color color))))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]
        [@color (send v set-color val)]
        [@margin
         (match-define (list h-m v-m) val)
         (send* v
           (horiz-margin h-m)
           (vert-margin v-m))]
        [@min-size
         (match-define (list w h) val)
         (send* v
           (min-width (or w 0))
           (min-height (or h 0)))]
        [@stretch
         (match-define (list w-s? h-s?) val)
         (send* v
           (stretchable-width w-s?)
           (stretchable-height h-s?))]))

    (define/public (destroy _v)
      (void))))

(define (text @label
              #:color [@color #f]
              #:font [font gui:normal-control-font]
              #:margin [@margin '(2 2)]
              #:min-size [@min-size '(#f #f)]
              #:stretch [@stretch '(#f #f)]
              #:mixin [mix values])
  (new (make-text% (mix gui:message%))
       [@label @label]
       [@color @color]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [font font]))
