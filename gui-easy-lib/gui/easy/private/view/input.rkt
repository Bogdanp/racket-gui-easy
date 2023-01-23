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

    (define/public (create parent)
      (define content (peek @content))
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list w h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define background-color (peek @background-color))
      (define the-field
        (new (context-mixin clazz)
             [parent parent]
             [label (peek @label)]
             [init-value (value->text content)]
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
      (define editor
        (send the-field get-editor))
      (send editor set-keymap keymap)
      (when background-color
        (send the-field set-field-background background-color))
      (begin0 the-field
        (send the-field set-context 'last-val content)))

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
         (define last-val (send v get-context 'last-val))
         (define text
           (value->text val))
         (cond
           [(not (value=? val last-val))
            (send v set-context 'last-val val)
            (call-preserving-position
             (send v get-editor)
             (lambda ()
               (send v set-value text)
               (send v refresh)))]

           [(string=? text "")
            (send v set-value "")
            (send v refresh)]

           [else
            (void)])]
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

    (define/public (destroy v)
      (send v clear-context))))

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
