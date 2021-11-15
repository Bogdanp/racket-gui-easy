#lang racket/base

(require (prefix-in gui: mrlib/snip-canvas)
         (prefix-in p: pict)
         racket/class
         racket/contract
         (prefix-in gui: racket/gui)
         "contract.rkt"
         "observable.rkt"
         "private/view.rkt")

(provide
 view<%>
 window-view<%>
 popup-menu-view<%>
 case/dep
 cond-view
 case-view
 (contract-out
  ;; Windows & Dialogs
  [window (window/c gui:frame% (listof (or/c 'no-resize-border 'no-caption
                                             'no-system-menu 'hide-menu-bar
                                             'toolbar-button 'float 'metal
                                             'fullscreen-button 'fullscreen-aux)))]
  [dialog (window/c gui:dialog% (listof (or/c 'no-caption 'no-sheet 'resize-border 'close-button)))]

  ;; Menus & Menu Items
  [popup-menu (-> (is-a?/c view<%>) ...
                  (is-a?/c popup-menu-view<%>))]
  [menu-bar (-> (is-a?/c view<%>) ...
                (is-a?/c menu-bar-view<%>))]
  [menu (-> (maybe-obs/c maybe-label/c)
            (is-a?/c view<%>) ...
            (is-a?/c menu-view<%>))]
  [menu-item (->* ((maybe-obs/c maybe-label/c))
                  ((-> any))
                  (is-a?/c view<%>))]
  [menu-item-separator (-> (is-a?/c view<%>))]

  ;; Containers
  [hpanel (panel/c)]
  [vpanel (panel/c)]
  [group (panel/c (maybe-obs/c gui:label-string?))]
  [tabs (->* ((maybe-obs/c (listof gui:label-string?))
              (-> (or/c 'close 'reorder 'select)
                  (listof any/c)
                  (or/c #f exact-nonnegative-integer?)
                  any))
             (#:choice->label (-> any/c gui:label-string?)
              #:selection (maybe-obs/c (or/c #f exact-nonnegative-integer?))
              #:alignment (maybe-obs/c alignment/c)
              #:enabled? (maybe-obs/c boolean?)
              #:style (listof (or/c 'no-border 'can-reorder 'can-close 'flat-portable 'deleted))
              #:spacing (maybe-obs/c gui:spacing-integer?)
              #:margin (maybe-obs/c margin/c)
              #:min-size (maybe-obs/c size/c)
              #:stretch (maybe-obs/c stretch/c))
             #:rest (listof (is-a?/c view<%>))
             (is-a?/c view<%>))]
  [if-view (-> (maybe-obs/c any/c)
               (is-a?/c view<%>)
               (is-a?/c view<%>)
               (is-a?/c view<%>))]
  [dyn-view (->* (obs? (-> any/c (is-a?/c view<%>)))
                 (#:equal? (-> any/c any/c any/c))
                 (is-a?/c view<%>))]
  [list-view (->* (obs? (-> any/c any/c (is-a?/c view<%>)))
                  (#:alignment (maybe-obs/c alignment/c)
                   #:enabled? (maybe-obs/c boolean?)
                   #:style (listof (or/c 'horizontal 'vertical 'border 'deleted
                                         'hscroll 'auto-hscroll 'hide-hscroll
                                         'vscroll 'auto-vscroll 'hide-vscroll))
                   #:spacing (maybe-obs/c gui:spacing-integer?)
                   #:margin (maybe-obs/c margin/c)
                   #:min-size (maybe-obs/c size/c)
                   #:stretch (maybe-obs/c stretch/c)
                   #:key (-> any/c any/c))
                  (is-a?/c view<%>))]

  ;; Canvases & Snips
  [canvas (canvas/c (-> (is-a?/c gui:dc<%>) any/c any))]
  [pict-canvas (canvas/c (-> any/c p:pict?))]
  [snip-canvas (canvas/c (-> any/c gui:dimension-integer? gui:dimension-integer? (is-a?/c gui:snip%)))]
  [snip (->* ((maybe-obs/c any/c)
              (-> any/c gui:dimension-integer? gui:dimension-integer? (is-a?/c gui:snip%)))
             ((-> (is-a?/c gui:snip%) any/c any)
              #:label (maybe-obs/c (or/c #f gui:label-string?))
              #:enabled? (maybe-obs/c boolean?)
              #:style (listof (or/c 'no-border 'control-border 'combo
                                    'resize-corner 'no-focus 'deleted
                                    'transparent))
              #:margin (maybe-obs/c margin/c)
              #:min-size (maybe-obs/c size/c)
              #:stretch (maybe-obs/c stretch/c)
              #:mixin (make-mixin-contract gui:snip-canvas%))
             (is-a?/c view<%>))]

  ;; Widgets
  [button (->* ((maybe-obs/c gui:label-string?)
                (-> any))
               (#:enabled? (maybe-obs/c boolean?)
                #:style (listof (or/c 'border 'multi-line 'deleted))
                #:font (is-a?/c gui:font%)
                #:margin (maybe-obs/c margin/c)
                #:min-size (maybe-obs/c size/c)
                #:stretch (maybe-obs/c stretch/c))
               (is-a?/c view<%>))]
  [checkbox (->* ((-> boolean? any))
                 (#:label (maybe-obs/c gui:label-string?)
                  #:checked? (maybe-obs/c boolean?)
                  #:enabled? (maybe-obs/c boolean?))
                 (is-a?/c view<%>))]
  [choice (->* ((maybe-obs/c (listof any/c))
                (-> (or/c #f any/c) any))
               (#:choice->label (-> any/c gui:label-string?)
                #:choice=? (-> any/c any/c boolean?)
                #:selection (maybe-obs/c any/c)
                #:label (maybe-obs/c maybe-label/c)
                #:style (listof (or/c 'horizontal-label 'vertical-label 'deleted))
                #:enabled? (maybe-obs/c boolean?)
                #:min-size (maybe-obs/c size/c)
                #:stretch (maybe-obs/c stretch/c))
               (is-a?/c view<%>))]
  [image (->* ((maybe-obs/c path-string?))
              (#:size (maybe-obs/c size/c)
               #:mode (maybe-obs/c (or/c 'fit 'fill)))
              (is-a?/c view<%>))]
  [input (->* ((maybe-obs/c any/c))
              ((-> (or/c 'input 'return) string? any)
               #:label (maybe-obs/c maybe-label/c)
               #:enabled? (maybe-obs/c boolean?)
               #:background-color (maybe-obs/c (or/c #f (is-a?/c gui:color%)))
               #:style (listof (or/c 'single 'multiple 'hscroll 'password
                                     'vertical-label 'horizontal-label
                                     'deleted))
               #:font (is-a?/c gui:font%)
               #:keymap (is-a?/c gui:keymap%)
               #:margin (maybe-obs/c margin/c)
               #:min-size (maybe-obs/c size/c)
               #:stretch (maybe-obs/c stretch/c)
               #:mixin (make-mixin-contract gui:text-field%)
               #:value=? (-> any/c any/c boolean?)
               #:value->text (-> any/c string?))
              (is-a?/c view<%>))]
  [progress (->* ((maybe-obs/c gui:position-integer?))
                 (#:label (maybe-obs/c gui:label-string?)
                  #:enabled? (maybe-obs/c boolean?)
                  #:style (listof (or/c 'horizontal 'vertical 'plain
                                        'vertical-label 'horizontal-label
                                        'deleted))
                  #:range (maybe-obs/c gui:positive-dimension-integer?)
                  #:min-size (maybe-obs/c size/c)
                  #:stretch (maybe-obs/c stretch/c))
                 (is-a?/c view<%>))]
  [radios (->* ((listof any/c)
                (-> (or/c #f any/c) any))
               (#:choice->label (-> any/c gui:label-string?)
                #:choice=? (-> any/c any/c boolean?)
                #:selection (maybe-obs/c (or/c #f any/c))
                #:label (maybe-obs/c maybe-label/c)
                #:style (listof (or/c 'horizontal 'vertical
                                      'horizontal-label 'vertical-label
                                      'deleted))
                #:enabled? (maybe-obs/c boolean?)
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
  [spacer (-> (is-a?/c view<%>))]
  [table (->* ((listof gui:label-string?)
               (maybe-obs/c vector?))
              (table-action/c
               #:label (maybe-obs/c (or/c #f gui:label-string?))
               #:entry->row (-> any/c vector?)
               #:selection (maybe-obs/c (or/c #f exact-nonnegative-integer? (listof exact-nonnegative-integer?)))
               #:enabled? (maybe-obs/c boolean?)
               #:style (listof (or/c 'single 'multiple 'extended
                                     'vertical-label 'horizontal-label
                                     'variable-columns 'column-headers
                                     'clickable-headers 'reorderable-headers
                                     'deleted))
               #:font (is-a?/c gui:font%)
               #:margin (maybe-obs/c margin/c)
               #:min-size (maybe-obs/c size/c)
               #:stretch (maybe-obs/c stretch/c)
               #:column-widths (maybe-obs/c
                                (listof
                                 (or/c (list/c exact-nonnegative-integer? gui:dimension-integer?)
                                       (list/c exact-nonnegative-integer? gui:dimension-integer? gui:dimension-integer? gui:dimension-integer?)))))
              (is-a?/c view<%>))]
  [text (->* ((maybe-obs/c gui:label-string?))
             (#:color (maybe-obs/c (or/c #f string? (is-a?/c gui:color%)))
              #:font (is-a?/c gui:font%))
             (is-a?/c view<%>))]))

(define (canvas/c draw/c)
  (->* ((maybe-obs/c any/c) draw/c)
       (#:label (maybe-obs/c gui:label-string?)
        #:enabled? (maybe-obs/c boolean?)
        #:style (listof (or/c 'border 'control-border 'combo
                              'vscroll 'hscroll 'resize-corner
                              'gl 'no-autoclear 'transparent
                              'no-focus 'deleted))
        #:margin (maybe-obs/c margin/c)
        #:min-size (maybe-obs/c size/c)
        #:stretch (maybe-obs/c stretch/c)
        #:mixin (make-mixin-contract gui:canvas%))
       (is-a?/c view<%>)))

(define-syntax-rule (panel/c arg/c ...)
  (->* (arg/c ...)
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

(define (window/c % style/c)
  (->* ()
       (#:title (maybe-obs/c string?)
        #:size (maybe-obs/c size/c)
        #:alignment (maybe-obs/c alignment/c)
        #:position (maybe-obs/c position/c)
        #:min-size (maybe-obs/c size/c)
        #:stretch (maybe-obs/c stretch/c)
        #:style style/c
        #:mixin (make-mixin-contract %))
       #:rest (listof (is-a?/c view<%>))
       (is-a?/c view<%>)))

(define table-action/c
  (-> (or/c 'select 'dclick 'column)
      vector?
      (or/c #f exact-nonnegative-integer? (listof exact-nonnegative-integer?))
      any))
