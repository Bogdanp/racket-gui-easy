#lang racket/base

(require racket/class
         racket/contract
         (prefix-in gui: racket/gui)
         racket/list
         "../observable.rkt"
         "common.rkt"
         "container.rkt"
         "view.rkt")

(provide
 popup-menu-view<%>
 popup-menu
 menu-bar-view<%>
 menu-bar
 menu-view<%>
 menu
 menu-item
 menu-item-separator)

(define popup-menu-view<%>
  (interface (view<%>)
    [create (->m #f (is-a?/c gui:popup-menu%))]))

(define popup-menu%
  (class* container% (popup-menu-view<%>)
    (inherit-field children)
    (inherit add-child update-children destroy-children child-dependencies)
    (super-new)

    (define/public (dependencies)
      (child-dependencies))

    (define/public (create _parent)
      (define the-menu (new (context-mixin gui:popup-menu%)))
      (begin0 the-menu
        (for ([c (in-list children)])
          (add-child the-menu c (send c create the-menu)))))

    (define/public (update v what val)
      (update-children v what val))

    (define/public (destroy v)
      (destroy-children v))))

(define menu-bar-view<%>
  (interface (view<%>)
    [create (->m (is-a?/c gui:frame%)
                 (is-a?/c gui:menu-bar%))]))

(define menu-bar%
  (class* container% (menu-bar-view<%>)
    (inherit-field children)
    (inherit add-child update-children destroy-children child-dependencies)
    (super-new)

    (define/public (dependencies)
      (child-dependencies))

    (define/public (create parent)
      (define the-menu-bar
        (new (context-mixin gui:menu-bar%)
             [parent parent]))
      (begin0 the-menu-bar
        (for ([c (in-list children)])
          (add-child the-menu-bar c (send c create the-menu-bar)))))

    (define/public (update v what val)
      (update-children v what val))

    (define/public (destroy v)
      (destroy-children v))))

(define menu-view<%>
  (interface (view<%>)
    [create (->m (or/c (is-a?/c gui:menu-bar%)
                       (is-a?/c gui:popup-menu%)
                       (is-a?/c gui:menu%))
                 (is-a?/c gui:menu%))]))

(define menu%
  (class* container% (menu-view<%>)
    (inherit-field children)
    (init-field @label)
    (inherit add-child update-children destroy-children child-dependencies)
    (super-new)

    (define/public (dependencies)
      (remove-duplicates
       (append (filter obs? (list @label))
               (child-dependencies))))

    (define/public (create parent)
      (define the-menu
        (new (context-mixin gui:menu%)
             [parent parent]
             [label (peek @label)]))
      (begin0 the-menu
        (for ([c (in-list children)])
          (add-child the-menu c (send c create the-menu)))))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)])
      (update-children v what val))

    (define/public (destroy v)
      (destroy-children v))))

(define menu-item%
  (class* object% (view<%>)
    (init-field @label @enabled? @help @shortcut action)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label @enabled? @help @shortcut)))

    (define/public (create parent)
      (define the-item
        (new gui:menu-item%
             [parent parent]
             [help-string (obs-peek @help)]
             [label (peek @label)]
             [callback (λ (_self _event)
                         (action))]))
      (begin0 the-item
        (send the-item enable (obs-peek @enabled?))
        (set-shortcut the-item (obs-peek @shortcut))))

    (define/public (update v what val)
      (case/dep what
        [@enabled? (send v enable val)]
        [@help (send v set-help-string val)]
        [@label (send v set-label val)]
        [@shortcut (set-shortcut v val)]))

    (define/public (destroy _v)
      (void))

    (define/private (set-shortcut v s)
      (cond
        [s
         ;; Contract guarantees at least one prefix and one key.
         (define-values (p k)
           (for/fold ([p null] [k #f] #:result (values (reverse p) k))
                     ([v (in-list s)])
             (values (if k (cons k p) p) v)))
         (send v set-shortcut k)
         (send v set-shortcut-prefix p)]
        [else
         (send v set-shortcut #f)
         (send v set-shortcut-prefix null)]))))

(define menu-item-separator%
  (class* object% (view<%>)
    (super-new)

    (define/public (dependencies)
      null)

    (define/public (create parent)
      (new gui:separator-menu-item%
           [parent parent]))

    (define/public (update _v _what _val)
      (void))

    (define/public (destroy _v)
      (void))))

(define (popup-menu . children)
  (new popup-menu%
       [children children]))

(define (menu-bar . children)
  (new menu-bar%
       [children children]))

(define (menu @label . children)
  (new menu%
       [@label @label]
       [children children]))

(define (menu-item @label [action void]
                   #:enabled? [@enabled? (obs #t)]
                   #:help [@help (obs #f)]
                   #:shortcut [@shortcut (obs #f)])
  (new menu-item%
       [@label (->obs @label)]
       [@enabled? (->obs @enabled?)]
       [@help (->obs @help)]
       [@shortcut (->obs @shortcut)]
       [action action]))

(define (menu-item-separator)
  (new menu-item-separator%))
