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
      (define the-menu (new gui:popup-menu%))
      (begin0 the-menu
        (for ([c (in-list children)])
          (add-child c (send c create the-menu)))))

    (define/public (update _v what val)
      (update-children what val))

    (define/public (destroy _v)
      (destroy-children))))

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
        (new gui:menu-bar%
             [parent parent]))
      (begin0 the-menu-bar
        (for ([c (in-list children)])
          (add-child c (send c create the-menu-bar)))))

    (define/public (update _v what val)
      (update-children what val))

    (define/public (destroy _v)
      (destroy-children))))

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
        (new gui:menu%
             [parent parent]
             [label (peek @label)]))
      (begin0 the-menu
        (for ([c (in-list children)])
          (add-child c (send c create the-menu)))))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)])
      (update-children what val))

    (define/public (destroy _v)
      (destroy-children))))

(define menu-item%
  (class* object% (view<%>)
    (init-field @label action)
    (super-new)

    (define/public (dependencies)
      (filter obs? (list @label)))

    (define/public (create parent)
      (new gui:menu-item%
           [parent parent]
           [label (peek @label)]
           [callback (λ (_self _event)
                       (action))]))

    (define/public (update v what val)
      (case/dep what
        [@label (send v set-label val)]))

    (define/public (destroy _v)
      (void))))

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

(define (menu-item @label [action void])
  (new menu-item%
       [@label @label]
       [action action]))

(define (menu-item-separator)
  (new menu-item-separator%))
