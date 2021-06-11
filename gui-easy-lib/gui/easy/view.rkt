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
                 (#:label (maybe-obs/c (or/c #f string?))
                  #:checked? (maybe-obs/c boolean?)
                  #:enabled? (maybe-obs/c boolean?))
                 (is-a?/c view<%>))]
  [hpanel panel/c]
  [vpanel panel/c]
  [button (-> (maybe-obs/c string?) (-> any) (is-a?/c view<%>))]
  [if/view (-> (maybe-obs/c any/c) (is-a?/c view<%>) (is-a?/c view<%>) (is-a?/c view<%>))]
  [input (->* ((maybe-obs/c string?))
              ((-> (or/c 'input 'return) string? any)
               #:enabled? (maybe-obs/c boolean?))
              (is-a?/c view<%>))]
  [label (-> (maybe-obs/c string?) (is-a?/c view<%>))]
  [dialog (window/c (listof (or/c 'no-caption 'no-sheet 'resize-border 'close-button)))]
  [window (window/c (listof (or/c 'no-resize-border 'no-caption
                                  'no-system-menu 'hide-menu-bar
                                  'toolbar-button 'float 'metal
                                  'fullscreen-button 'fullscreen-aux)))]))

(define panel/c
  (-> (is-a?/c view<%>) ... (is-a?/c view<%>)))

(define (window/c style/c)
  (->* ()
       (#:label (maybe-obs/c string?)
        #:size (maybe-obs/c (cons/c gui:dimension-integer? gui:dimension-integer?))
        #:position (maybe-obs/c (or/c 'center (cons/c gui:position-integer? gui:position-integer?)))
        #:style style/c)
       #:rest (listof (is-a?/c view<%>))
       (is-a?/c view<%>)))
