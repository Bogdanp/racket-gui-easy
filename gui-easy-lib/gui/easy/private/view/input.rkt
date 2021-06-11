#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         "../observable.rkt"
         "view.rkt")

(provide
 input)

(define input%
  (class* object% (view<%>)
    (init-field @content @enabled action)
    (super-new)

    (define/public (dependencies)
      (list @content @enabled))

    (define/public (create parent)
      (new gui:text-field%
           [parent parent]
           [label #f]
           [init-value (obs-peek @content)]
           [enabled (obs-peek @enabled)]
           [callback (λ (self event)
                       (action
                        (case (send event get-event-type)
                          [(text-field) 'input]
                          [(text-field-enter) 'return])
                        (send self get-value)))]))

    (define/public (update v what val)
      (when (eq? what @content)
        ;; Don't update the content if the value hasn't changed to avoid
        ;; forcing a change to the cursor position if the input has an
        ;; action that somehow alters its own @content.
        (unless (string=? val (send v get-value))
          (send v set-value val)))
      (when (eq? what @enabled)
        (send v enable val)))

    (define/public (destroy _v)
      (void))))

(define (input @content [action void]
               #:enabled? [@enabled (obs #t)])
  (new input%
       [@content (->obs @content)]
       [@enabled (->obs @enabled)]
       [action action]))
