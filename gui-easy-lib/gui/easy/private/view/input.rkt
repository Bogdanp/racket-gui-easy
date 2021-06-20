#lang racket/base

(require (only-in framework keymap:get-global)
         racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(provide
 input)

(define input%
  (class* object% (view<%>)
    (init-field @label @content @enabled? @background-color action style font keymap)
    (super-new)

    (define/public (dependencies)
      (list @label @content @enabled? @background-color))

    (define/public (create parent)
      (define background-color (obs-peek @background-color))
      (define the-field
        (new gui:text-field%
             [parent parent]
             [label (obs-peek @label)]
             [init-value (obs-peek @content)]
             [enabled (obs-peek @enabled?)]
             [callback (λ (self event)
                         (action
                          (case (send event get-event-type)
                            [(text-field) 'input]
                            [(text-field-enter) 'return])
                          (send self get-value)))]
             [style style]
             [font font]))
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
           (send v set-value val))]
        [@enabled?
         (send v enable val)]
        [@background-color
         (send v set-field-background val)]))

    (define/public (destroy _v)
      (void))))

(define (input @content [action void]
               #:label [@label (obs #f)]
               #:enabled? [@enabled? (obs #t)]
               #:background-color [@background-color (obs #f)]
               #:style [style '(single)]
               #:font [font gui:normal-control-font]
               #:keymap [keymap (keymap:get-global)])
  (new input%
       [@label (->obs @label)]
       [@content (->obs @content)]
       [@enabled? (->obs @enabled?)]
       [@background-color (->obs @background-color)]
       [action action]
       [style style]
       [font font]
       [keymap keymap]))
