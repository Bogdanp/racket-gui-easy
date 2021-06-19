#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui)
         "observable.rkt"
         "private/view.rkt")

(provide
 cond/view
 (contract-out
  [canvas (->* ((maybe-obs/c any/c)
                (-> (is-a?/c gui:dc<%>) any/c any))
               (#:label (maybe-obs/c gui:label-string?)
                #:enabled? (maybe-obs/c boolean?)
                #:style (listof (or/c 'border 'control-border 'combo
                                      'vscroll 'hscroll 'resize-corner
                                      'gl 'no-autoclear 'transparent
                                      'no-focus 'deleted))
                #:margin margin/c
                #:min-size size/c
                #:stretch stretch/c)
               (is-a?/c view<%>))]
  [checkbox (->* ((-> boolean? any))
                 (#:label (maybe-obs/c label/c)
                  #:checked? (maybe-obs/c boolean?)
                  #:enabled? (maybe-obs/c boolean?))
                 (is-a?/c view<%>))]
  [choice (->* ((maybe-obs/c (listof gui:label-string?))
                (-> gui:label-string? any))
               (#:selection (maybe-obs/c gui:label-string?)
                #:label (maybe-obs/c gui:label-string?)
                #:style (listof (or/c 'horizontal-label 'vertical-label 'deleted))
                #:enabled? (maybe-obs/c boolean?)
                #:min-size (maybe-obs/c size/c)
                #:stretch (maybe-obs/c stretch/c))
               (is-a?/c view<%>))]
  [hpanel panel/c]
  [vpanel panel/c]
  [button (-> (maybe-obs/c string?) (-> any) (is-a?/c view<%>))]
  [if/view (-> (maybe-obs/c any/c) (is-a?/c view<%>) (is-a?/c view<%>) (is-a?/c view<%>))]
  [image (->* ((maybe-obs/c path-string?))
              (#:size (maybe-obs/c size/c)
               #:mode (maybe-obs/c (or/c 'fit 'fill)))
              (is-a?/c view<%>))]
  [input (->* ((maybe-obs/c string?))
              ((-> (or/c 'input 'return) string? any)
               #:label (maybe-obs/c label/c)
               #:enabled? (maybe-obs/c boolean?)
               #:background-color (maybe-obs/c (or/c #f (is-a?/c gui:color%)))
               #:style (listof (or/c 'single 'multiple 'hscroll 'password
                                     'vertical-label 'horizontal-label
                                     'deleted))
               #:font (is-a?/c gui:font%))
              (is-a?/c view<%>))]
  [progress (->* ((maybe-obs/c gui:dimension-integer?))
                 (#:label (maybe-obs/c gui:label-string?)
                  #:enabled? (maybe-obs/c boolean?)
                  #:style (listof (or/c 'horizontal 'vertical 'plain
                                        'vertical-label 'horizontal-label
                                        'deleted))
                  #:range gui:positive-dimension-integer?
                  #:min-size (maybe-obs/c size/c)
                  #:stretch (maybe-obs/c stretch/c))
                 (is-a?/c view<%>))]
  [slider (->* ((maybe-obs/c gui:position-integer?)
                (-> gui:position-integer? any))
               (#:label (maybe-obs/c gui:label-string?)
                #:enabled? (maybe-obs/c boolean?)
                #:style (listof (or/c 'horizontal 'vertical 'plain
                                      'vertical-label 'horizontal-label
                                      'deleted))
                #:min-value gui:position-integer?
                #:max-value gui:position-integer?
                #:min-size (maybe-obs/c size/c)
                #:stretch (maybe-obs/c stretch/c))
               (is-a?/c view<%>))]
  [snip (->* ((maybe-obs/c any/c)
              (-> any/c gui:dimension-integer? gui:dimension-integer? (is-a?/c gui:snip%)))
             ((-> (is-a?/c gui:snip%) any/c any)
              #:label (maybe-obs/c (or/c #f gui:label-string?))
              #:enabled? (maybe-obs/c boolean?)
              #:style (listof (or/c 'no-border 'control-border 'combo
                                    'resize-corner 'no-focus 'deleted
                                    'transparent))
              #:margin margin/c
              #:min-size size/c
              #:stretch stretch/c)
             (is-a?/c view<%>))]
  [spacer (-> (is-a?/c view<%>))]
  [table (->* ((listof gui:label-string?)
               (maybe-obs/c (vectorof vector?))
               (-> (or/c 'select 'dclick 'column)
                   (or/c #f exact-nonnegative-integer?)
                   any))
              (#:label (maybe-obs/c (or/c #f gui:label-string?))
               #:selection (maybe-obs/c (or/c #f exact-nonnegative-integer? (listof exact-nonnegative-integer?)))
               #:enabled? (maybe-obs/c boolean?)
               #:style (listof (or/c 'single 'multiple 'extended
                                     'vertical-label 'horizontal-label
                                     'variable-columns 'column-headers
                                     'clickable-headers 'reorderable-headers
                                     'deleted))
               #:font (is-a?/c gui:font%)
               #:margin margin/c
               #:min-size size/c
               #:stretch stretch/c)
              (is-a?/c view<%>))]
  [text (-> (maybe-obs/c gui:label-string?) (is-a?/c view<%>))]
  [dialog (window/c (listof (or/c 'no-caption 'no-sheet 'resize-border 'close-button)))]
  [window (window/c (listof (or/c 'no-resize-border 'no-caption
                                  'no-system-menu 'hide-menu-bar
                                  'toolbar-button 'float 'metal
                                  'fullscreen-button 'fullscreen-aux)))]))

(define alignment/c
  (list/c (or/c 'left 'center 'right)
          (or/c 'top 'center 'bottom)))

(define label/c
  (or/c #f gui:label-string?))

(define margin/c
  (list/c gui:spacing-integer?
          gui:spacing-integer?))

(define position/c
  (or/c 'center (list/c gui:position-integer?
                        gui:position-integer?)))

(define size/c
  (list/c (or/c #f gui:dimension-integer?)
          (or/c #f gui:dimension-integer?)))

(define stretch/c
  (list/c boolean? boolean?))

(define panel/c
  (->* ()
       (#:alignment (maybe-obs/c alignment/c)
        #:enabled? (maybe-obs/c boolean?)
        #:style (listof (or/c 'border 'deleted
                              'hscroll 'auto-hscroll 'hide-hscroll
                              'vscroll 'auto-vscroll 'hide-vscroll))
        #:spacing (maybe-obs/c gui:spacing-integer?)
        #:margin (maybe-obs/c margin/c)
        #:min-size (maybe-obs/c size/c)
        #:stretch (maybe-obs/c stretch/c))
       #:rest (listof (is-a?/c view<%>))
       (is-a?/c view<%>)))

(define (window/c style/c)
  (->* ()
       (#:title (maybe-obs/c string?)
        #:size (maybe-obs/c size/c)
        #:alignment (maybe-obs/c alignment/c)
        #:position (maybe-obs/c position/c)
        #:min-size (maybe-obs/c size/c)
        #:stretch (maybe-obs/c stretch/c)
        #:style style/c)
       #:rest (listof (is-a?/c view<%>))
       (is-a?/c view<%>)))
