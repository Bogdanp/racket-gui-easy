#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match
         "../observable.rkt"
         "common.rkt"
         "keymap.rkt"
         "view.rkt")

(provide
 input)

(define (input% clazz)
  (class* object% (view<%>)
    (init-field @label @content @enabled? @background-color @margin @min-size @stretch action style font keymap value=? value->text)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @content @enabled? @background-color @margin @min-size @stretch)))

    (define last-val #f)
    (define/public (create parent)
      (set! last-val (obs-peek @content))
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define background-color (peek @background-color))
      (define the-field
        (new clazz
             [parent parent]
             [label (peek @label)]
             [init-value (value->text last-val)]
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

    (define (call-preserving-position ed thunk)
      (define old-text (send ed get-text))
      (define-values (start end)
        (let ([sb (box #f)]
              [eb (box #f)])
          (send ed get-position sb eb)
          (values
           (unbox sb)
           (unbox eb))))
      ;; When the entire text is selected and the new text after the
      ;; (thunk) is longer, expand the selection to cover the new
      ;; text.
      (define full-selection?
        (and (= start 0)
             (= end (string-length old-text))))
      (begin0 (thunk)
        ;; When the contents of the editor are empty, avoid changing
        ;; the position since doing so would place the cursor before
        ;; any newly-inserted text.
        (unless (string=? "" old-text)
          (if full-selection?
              (send ed set-position 0 (string-length (send ed get-text)))
              (send ed set-position start end)))))

    (define/public (update v what val)
      (case/dep what
        [@label
         (send v set-label val)]
        [@content
         (unless (value=? val last-val)
           (set! last-val val)
           (call-preserving-position
            (send v get-editor)
            (lambda ()
              (send v set-value (value->text val))
              (send v refresh))))]
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
               #:keymap [keymap the-default-keymap]
               #:margin [@margin '(2 2)]
               #:min-size [@min-size '(#f #f)]
               #:stretch [@stretch '(#t #f)]
               #:mixin [mix values]
               #:value=? [value=? equal?]
               #:value->text [value->text values])
  (new (input% (mix gui:text-field%))
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
       [keymap keymap]
       [value=? value=?]
       [value->text value->text]))
