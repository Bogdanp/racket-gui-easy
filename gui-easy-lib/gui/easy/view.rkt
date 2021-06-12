#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui)
         "observable.rkt"
         "private/view.rkt")

(provide
 cond/view
 (contract-out
  [checkbox (->* ((-> any))
                 (#:label (maybe-obs/c label/c)
                  #:checked? (maybe-obs/c boolean?)
                  #:enabled? (maybe-obs/c boolean?))
                 (is-a?/c view<%>))]
  [hpanel panel/c]
  [vpanel panel/c]
  [button (-> (maybe-obs/c string?) (-> any) (is-a?/c view<%>))]
  [if/view (-> (maybe-obs/c any/c) (is-a?/c view<%>) (is-a?/c view<%>) (is-a?/c view<%>))]
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
  [label (-> (maybe-obs/c gui:label-string?) (is-a?/c view<%>))]
  [spacer (-> (is-a?/c view<%>))]
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

(define panel/c
  (->* ()
       (#:alignment (maybe-obs/c alignment/c)
        #:enabled? (maybe-obs/c boolean?)
        #:style (listof (or/c 'border 'deleted
                              'hscroll 'auto-hscroll 'hide-hscroll
                              'vscroll 'auto-vscroll 'hide-vscroll))
        #:min-size (maybe-obs/c size?)
        #:stretch (maybe-obs/c stretch?))
       #:rest (listof (is-a?/c view<%>))
       (is-a?/c view<%>)))

(define (window/c style/c)
  (->* ()
       (#:title (maybe-obs/c string?)
        #:size (maybe-obs/c size?)
        #:alignment (maybe-obs/c alignment/c)
        #:position (maybe-obs/c (or/c 'center (cons/c gui:position-integer? gui:position-integer?)))
        #:min-size (maybe-obs/c size?)
        #:stretch (maybe-obs/c stretch?)
        #:style style/c)
       #:rest (listof (is-a?/c view<%>))
       (is-a?/c view<%>)))
