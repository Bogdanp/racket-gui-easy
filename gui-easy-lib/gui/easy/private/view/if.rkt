#lang racket/base

(require (for-syntax racket/base)
         racket/class
         (prefix-in gui: racket/gui)
         syntax/parse/define
         "../observable.rkt"
         "../renderer.rkt"
         "common.rkt"
         "container.rkt"
         "proxy.rkt"
         "view.rkt")

(provide
 if-view
 cond-view
 case-view)

(define ->bool (λ (v) (and v #t)))

(define if-view%
  (class* container% (view<%>)
    (init-field @cond-e then-proc else-proc)
    (inherit add-child get-child has-child? remove-child)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @cond-e)))

    (define/public (create parent)
      (define the-pane
        (new (context-mixin gui:panel%)
             [parent parent]
             [min-width #f]
             [min-height #f]
             [stretchable-width #t]
             [stretchable-height #t]))
      (define this-bool (->bool (peek @cond-e)))
      (begin0 the-pane
        (send the-pane set-context 'last-bool this-bool)
        (if this-bool
            (create&add-then-view the-pane)
            (create&add-else-view the-pane))))

    (define/public (update v what val)
      (case/dep what
        [@cond-e
         (define this-bool (->bool val))
         (unless (eq? (get-last-bool v) this-bool)
           (with-container-sequence v
             (when (has-then-view? v)
               (remove&destroy-then-view v))
             (when (has-else-view? v)
               (remove&destroy-else-view v))
             (if this-bool
                 (create&add-then-view v)
                 (create&add-else-view v))
             (send v set-context 'last-bool this-bool)))]))

    (define/public (destroy v)
      (when (has-then-view? v)
        (remove&destroy-then-view v))
      (when (has-else-view? v)
        (remove&destroy-else-view v))
      (send v clear-context))

    (define/private (get-last-bool v)
      (send v get-context 'last-bool v))
    (define/private (get-then-view v)
      (send v get-context 'then-view #f))
    (define/private (get-else-view v)
      (send v get-context 'else-view #f))
    (define/private (has-then-view? v)
      (has-child? v (get-then-view v)))
    (define/private (has-else-view? v)
      (has-child? v (get-else-view v)))

    (define-syntax-rule (define-adder id view-proc view-id deps-id)
      (define (id pane)
        (define view-id (proxy (view-proc)))
        (define deps (send view-id dependencies))
        (define widget (send view-id create pane))
        (send pane set-context 'view-id view-id)
        (send pane set-context 'deps-id (send (current-renderer) add-dependencies deps view-id widget))
        (add-child pane view-id widget)))

    (define-adder create&add-then-view then-proc then-view then-deps)
    (define-adder create&add-else-view else-proc else-view else-deps)

    (define-syntax-rule (define-remover id view-id deps-id)
      (define (id pane)
        (define view-id (send pane get-context 'view-id))
        (define deps-id (send pane get-context 'deps-id))
        (send (current-renderer) remove-dependencies deps-id)
        (define widget (get-child pane view-id))
        (send pane delete-child widget)
        (send view-id destroy widget)
        (remove-child pane view-id)
        (send pane remove-context 'view-id)
        (send pane remove-context 'deps-id)))

    (define-remover remove&destroy-then-view then-view then-deps)
    (define-remover remove&destroy-else-view else-view else-deps)))

(define (make-if-view @cond-e then-proc else-proc)
  (new if-view%
       [@cond-e @cond-e]
       [then-proc then-proc]
       [else-proc else-proc]
       [children null]))

(define-syntax-parser if-view
  [(_ @cond-e:expr then-e:expr else-e:expr)
   #'(make-if-view @cond-e (λ () then-e) (λ () else-e))])

(define-syntax-parser cond-view
  #:literals (else)
  [(_ [@cond-e:expr then-e:expr] [else else-e:expr])
   #'(if-view @cond-e then-e else-e)]

  [(_ [@cond-e-1:expr then-e-1:expr]
      [@cond-e:expr then-e:expr] ...
      [else else-e:expr])
   #'(if-view @cond-e-1
              then-e-1
              (cond-view
               [@cond-e then-e] ...
               [else else-e]))])

(define-syntax-parser case-view
  #:literals (else)
  [(_ @e
      [(lit ...+) then-e:expr] ...
      [else else-e:expr])
   #'(cond-view
      [(obs-map @e (λ (e) (member e '(lit ...)))) then-e] ...
      [else else-e])])
