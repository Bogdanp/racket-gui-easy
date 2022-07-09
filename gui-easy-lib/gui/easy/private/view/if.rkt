#lang racket/base

(require (for-syntax racket/base)
         racket/class
         (prefix-in gui: racket/gui)
         racket/list
         syntax/parse/define
         "../observable.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 if-view
 cond-view
 case-view)

(define if-view%
  (class* container% (view<%>)
    (init-field @cond-e then-view else-view)
    (inherit add-child get-child has-child? remove-child)
    (super-new)

    (define/public (dependencies)
      (filter obs? (remove-duplicates
                    (append
                     (list @cond-e)
                     (send then-view dependencies)
                     (send else-view dependencies)))))

    (define/public (create parent)
      (define the-pane
        (new gui:panel%
             [parent parent]
             [min-width #f]
             [min-height #f]
             [stretchable-width #t]
             [stretchable-height #t]))
      (begin0 the-pane
        (if (peek @cond-e)
            (add-child then-view (send then-view create the-pane))
            (add-child else-view (send else-view create the-pane)))))

    (define then-pending null)
    (define else-pending null)
    (define/public (update v what val)
      (case/dep what
        [@cond-e
         (send v begin-container-sequence)
         (when (and val (has-child? else-view))
           (define w (get-child else-view))
           (send else-view destroy w)
           (send v delete-child w)
           (remove-child else-view)
           (add-child then-view (send then-view create v))
           (unless (null? then-pending)
             (for ([pending (in-list then-pending)])
               (send/apply then-view update (get-child then-view) pending))
             (set! then-pending null)))
         (when (and (not val) (has-child? then-view))
           (define w (get-child then-view))
           (send then-view destroy w)
           (send v delete-child w)
           (remove-child then-view)
           (add-child else-view (send else-view create v))
           (unless (null? else-pending)
             (for ([pending (in-list else-pending)])
               (send/apply else-view update (get-child else-view) pending))
             (set! else-pending null)))
         (send v end-container-sequence)])

      (when (member what (send then-view dependencies))
        (if (has-child? then-view)
            (send then-view update (get-child then-view) what val)
            (set! then-pending (add-pending then-pending what val))))
      (when (member what (send else-view dependencies))
        (if (has-child? else-view)
            (send else-view update (get-child else-view) what val)
            (set! else-pending (add-pending else-pending what val)))))

    (define (add-pending pending what val)
      (cons
       (list what val)
       (filter-not (λ (p) (equal? (car p) what)) pending)))

    (define/public (destroy _v)
      (when (has-child? then-view)
        (send then-view destroy (get-child then-view))
        (remove-child then-view))
      (when (has-child? else-view)
        (send else-view destroy (get-child else-view))
        (remove-child else-view)))))

(define (make-if-view @cond-e then-view else-view)
  (new if-view%
       [@cond-e @cond-e]
       [then-view then-view]
       [else-view else-view]
       [children null]))

(define-syntax-parser if-view
  [(_ @cond-e:expr then-e:expr else-e:expr)
   #'(make-if-view @cond-e then-e else-e)])

(define-syntax-parser cond-view
  #:literals (else)
  [(_ [@cond-e:expr then-e:expr] [else else-e:expr])
   #'(make-if-view @cond-e then-e else-e)]

  [(_ [@cond-e-1:expr then-e-1:expr] [@cond-e:expr then-e:expr] ... [else else-e:expr])
   #'(make-if-view @cond-e-1
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
