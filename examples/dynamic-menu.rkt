#lang racket/gui/easy

(require racket/gui/easy/debugger
         racket/list
         racket/match)

(define/obs @items
  `(("a" . #f)
    ("b" . #f)))

(define ((make-update-item label) checked?)
  (@items . <~ . (lambda (items)
                   (for/list ([item (in-list items)]
                              #:do [(match-define (cons item-label _) item)])
                     (if (equal? item-label label)
                         (cons label checked?)
                         item)))))

(start-debugger)
(render
 (window
  #:title "Dynamic Menu"
  (menu-bar
   (observable-view
    @items
    (lambda (items)
      (apply
       menu
       "File"
       (menu-item "New")
       (menu-item "Open...")
       (menu-item-separator)
       (append
        (for/list ([item (in-list items)])
          (match-define (cons label checked?) item)
          (checkable-menu-item label (make-update-item label) #:checked? checked?))
        (list
         (menu-item-separator)
         (menu-item "Quit")))))))
  (button
   "Shuffle"
   (lambda ()
     (@items . <~ . shuffle)))))
