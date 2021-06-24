#lang racket/base

(require (only-in framework keymap:get-global)
         racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 input)

(define input%
  (class* object% (view<%>)
    (init-field @label @content @enabled? @background-color @margin @min-size @stretch action style font keymap)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @content @enabled? @background-color @margin @min-size @stretch)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define background-color (peek @background-color))
      (define the-field
        (new gui:text-field%
             [parent parent]
             [label (peek @label)]
             [init-value (peek @content)]
             [enabled (peek @enabled?)]
             [callback (λ (self event)
                         (action
                          (case (send event get-event-type)
                            [(text-field) 'input]
                            [(text-field-enter) 'return])
                          (send self get-value)))]
             [style style]
             [font font]
             [vert-margin v-m]
             [horiz-margin h-m]
             [min-width w]
             [min-height h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-field
        (when background-color
          (send the-field set-field-background background-color))
        (send+ the-field (get-editor) (set-keymap keymap))))

    (define/public (update v what val)
      (case/dep what
        [@label
         (send v set-label val)]
        [@content
         ;; Don't update the content if the value hasn't changed to avoid
         ;; forcing a change to the cursor position in case the input has
         ;; an action that somehow alters its own @content.
         (unless (string=? val (send v get-value))
           (send v set-value val)
           (send v refresh))]
        [@enabled?
         (send v enable val)]
        [@background-color
         (send v set-field-background val)]
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

(define (input @content [action void]
               #:label [@label #f]
               #:enabled? [@enabled? #t]
               #:background-color [@background-color #f]
               #:style [style '(single)]
               #:font [font gui:normal-control-font]
               #:keymap [keymap (keymap:get-global)]
               #:margin [@margin '(2 2)]
               #:min-size [@min-size '(#f #f)]
               #:stretch [@stretch '(#t #f)])
  (new input%
       [@label @label]
       [@content @content]
       [@enabled? @enabled?]
       [@background-color @background-color]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [action action]
       [style style]
       [font font]
       [keymap keymap]))
