#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 button)

(define button%
  (class* object% (view<%>)
    (init-field @label @enabled? @margin @min-size @stretch action style font)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @enabled? @margin @min-size @stretch)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (new gui:button%
           [parent parent]
           [label (peek @label)]
           [callback (λ (_self _event)
                       (action))]
           [enabled (peek @enabled?)]
           [style style]
           [font font]
           [vert-margin v-m]
           [horiz-margin h-m]
           [min-width w]
           [min-height h]
           [stretchable-width w-s?]
           [stretchable-height h-s?]))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]
        [@enabled? (send v enable val)]
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

(define (button @label action
                #:enabled? [@enabled? #t]
                #:style [style null]
                #:font [font gui:normal-control-font]
                #:margin [@margin '(2 2)]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#f #f)])
  (new button%
       [@label @label]
       [@enabled? @enabled?]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [action action]
       [style style]
       [font font]))
