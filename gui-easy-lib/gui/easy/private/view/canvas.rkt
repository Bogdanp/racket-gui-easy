#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/lazy-require
         racket/match
         "../class.rkt"
         "../observable.rkt"
         "common.rkt"
         "view.rkt")

(lazy-require
 [pict (draw-pict)])

(provide
 canvas
 pict-canvas
 snip-canvas
 editor-canvas)

(define missing (gensym 'missing))

(define (make-canvas% gui-canvas%)
  (class* object% (view<%>)
    (init-private-field @input @label @enabled? @margin @min-size @stretch draw style)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @input @label @enabled? @margin @min-size @stretch)))

    (define/public (create parent)
      (match-define (list h-m v-m) (peek @margin))
      (match-define (list min-w min-h) (peek @min-size))
      (match-define (list w-s? h-s?) (peek @stretch))
      (define the-canvas
        (new (context-mixin gui-canvas%)
             [parent parent]
             [paint-callback (λ (self dc)
                               (define input (send self get-context 'input missing))
                               (unless (eq? input missing)
                                 (draw dc input)))]
             [label (peek @label)]
             [enabled (peek @enabled?)]
             [style style]
             [horiz-margin h-m]
             [vert-margin v-m]
             [min-width min-w]
             [min-height min-h]
             [stretchable-width w-s?]
             [stretchable-height h-s?]))
      (begin0 the-canvas
        (send the-canvas set-context 'input (peek @input))))

    (define/public (update v what val)
      (case/dep what
        [@input
         (send v set-context 'input val)
         (send v refresh)]
        [@label
         (send v set-label val)]
        [@enabled?
         (send v enable val)]
        [@margin
         (match-define (list h v) val)
         (send* v
           (horiz-margin h)
           (vert-margin v))]
        [@min-size
         (match-define (list w h) val)
         (send* v
           (min-width w)
           (min-height h))]
        [@stretch
         (match-define (list w-s? h-s?) val)
         (send* v
           (stretchable-width w-s?)
           (stretchable-height h-s?))]))

    (define/public (destroy v)
      (send v clear-context))))

(define (canvas @input draw
                #:label [@label #f]
                #:enabled? [@enabled? #t]
                #:style [style null]
                #:margin [@margin '(0 0)]
                #:min-size [@min-size '(#f #f)]
                #:stretch [@stretch '(#t #t)]
                #:mixin [mix values])
  (new (make-canvas% (mix gui:canvas%))
       [@input @input]
       [@label @label]
       [@enabled? @enabled?]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [draw draw]
       [style style]))

(define pict-canvas
  (procedure-rename
   (make-keyword-procedure
    (λ (kws kw-args @data make-pict . args)
      (define (draw dc v)
        (send dc set-smoothing 'smoothed)
        (draw-pict (make-pict v) dc 0 0))
      (keyword-apply canvas kws kw-args @data draw args)))
   'pict-canvas))

(define snip-canvas
  (procedure-rename
   (make-keyword-procedure
    (λ (kws kw-args @data make-snip . args)
      (define draw
        (let ([last-w   #f]
              [last-h   #f]
              [last-v   #f]
              [last-bmp #f])
          (λ (dc v)
            (define-values (w h)
              (send dc get-size))
            (define bmp
              (cond
                [(and (equal? last-w w)
                      (equal? last-h h)
                      (equal? last-v v))
                 last-bmp]
                [else
                 (define snip
                   (make-snip v w h))
                 (define bmp
                   (send snip get-bitmap))
                 (begin0 bmp
                   (set! last-bmp bmp)
                   (set! last-v v)
                   (set! last-w w)
                   (set! last-h h))]))
            (send dc draw-bitmap bmp 0 0))))
      (keyword-apply canvas kws kw-args @data draw args)))
   'snip-canvas))

(define (make-editor-canvas% gui-canvas%)
  (class* gui-canvas% (view<%>)
    (init-private-field @editor @wheel-step @line-count @inset)
    (super-new)

    (define/override (dependencies)
      (append (super dependencies)
              (filter obs? (list @editor @wheel-step @line-count @inset))))

    (define/override (create parent)
      (define v (super create parent))
      (send v set-editor (peek @editor))
      (send v wheel-step (peek @wheel-step))
      (send v set-line-count (peek @line-count))
      (match-define (list h-i v-i) (peek @inset))
      (send v vertical-inset v-i)
      (send v horizontal-inset h-i)
      v)

    (define/override (update v what val)
      (case/dep what
        [@editor
         (send v set-editor val)]
        [@wheel-step
         (send v wheeel-step val)]
        [@line-count
         (send v set-line-count val)]
        [@inset
         (match-define (list h-i v-i) val)
         (send v vertical-inset v-i)
         (send v horizontal-inset h-i)]
        [else
         (super update v what val)]))))

(define (editor-canvas @editor
                       #:label [@label #f]
                       #:enabled? [@enabled? #t]
                       #:style [style null]
                       #:scrolls-per-page [scrolls-per-page 100]
                       #:wheel-step [@wheel-step 3]
                       #:line-count [@line-count #f]
                       #:inset [@inset '(5 5)]
                       #:margin [@margin '(0 0)]
                       #:min-size [@min-size '(#f #f)]
                       #:stretch [@stretch '(#t #t)]
                       #:mixin [mix values])
  (new (make-editor-canvas%
        (make-canvas%
         (class (mix gui:editor-canvas%)
           (init-private-field [(_ paint-callback)])
           (super-new [scrolls-per-page scrolls-per-page]))))
       [@editor @editor]
       [@wheel-step @wheel-step]
       [@line-count @line-count]
       [@inset @inset]
       [@input #f]
       [@label @label]
       [@enabled? @enabled?]
       [@margin @margin]
       [@min-size @min-size]
       [@stretch @stretch]
       [draw void]
       [style style]))
