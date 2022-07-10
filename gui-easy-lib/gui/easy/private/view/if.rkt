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

(define ->bool (compose1 not not))

(define if-view%
  (class* container% (view<%>)
    (init-field @cond-e then-proc else-proc)
    (inherit add-child get-child has-child? remove-child)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @cond-e)))

    (define/public (create parent)
      (define the-pane
        (new gui:panel%
             [parent parent]
             [min-width #f]
             [min-height #f]
             [stretchable-width #t]
             [stretchable-height #t]))
      (begin0 the-pane
        (if last-bool
            (create&add-then-view the-pane)
            (create&add-else-view the-pane))))

    (define last-bool (->bool (peek @cond-e)))
    (define then-view #f)
    (define else-view #f)
    (define then-deps #f)
    (define else-deps #f)
    (define/public (update v what val)
      (case/dep what
        [@cond-e
         (define this-bool (->bool val))
         (unless (eq? last-bool this-bool)
           (send v begin-container-sequence)
           (when (has-child? then-view)
             (remove&destroy-then-view v))
           (when (has-child? else-view)
             (remove&destroy-else-view v))
           (if this-bool
               (create&add-then-view v)
               (create&add-else-view v))
           (set! last-bool this-bool)
           (send v end-container-sequence))]))

    (define/public (destroy v)
      (when (has-child? then-view)
        (remove&destroy-then-view v))
      (when (has-child? else-view)
        (remove&destroy-else-view v)))

    (define-syntax-rule (define-adder id view-proc view-id deps-id)
      (define (id pane)
        (set! view-id (proxy (view-proc)))
        (define deps (send view-id dependencies))
        (define widget (send view-id create pane))
        (set! deps-id (send (current-renderer) add-dependencies deps view-id widget))
        (add-child view-id widget)))

    (define-adder create&add-then-view then-proc then-view then-deps)
    (define-adder create&add-else-view else-proc else-view else-deps)

    (define-syntax-rule (define-remover id view-id deps-id)
      (define (id pane)
        (send (current-renderer) remove-dependencies deps-id)
        (define widget (get-child view-id))
        (send pane delete-child widget)
        (send view-id destroy widget)
        (remove-child widget)
        (set! view-id #f)
        (set! deps-id #f)))

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
      [(obs-map @e (λ (e) (memv e '(lit ...)))) then-e] ...
      [else else-e])])
