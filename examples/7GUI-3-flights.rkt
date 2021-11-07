#lang racket/base

(require gregor
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/list)

(define types '(one-way return))
(define (~type t)
  (case t
    [(one-way) "one-way flight"]
    [(return) "return flight"]))

(define (~date d)
  (~t d "yyyy.MM.dd"))
(define (string->date s)
  (with-handlers ([(λ (_) #t)
                   (λ (_) #f)])
    (parse-date s "yyyy.MM.dd")))

(define (~color s)
  (if (string->date s)
      #f
      (color "red")))

(define/obs @type 'one-way)
(define/obs @t1 (~date (today)))
(define/obs @t2 (~date (+days (today) 1)))
(define/obs @valid?
  (obs-combine
   (λ (type t1 t2)
     (case type
       [(one-way)
        (and (string->date t1) #t)]

       [(return)
        (define d1 (string->date t1))
        (define d2 (string->date t2))
        (and d1 d2 (date<? d1 d2))]))
   @type @t1 @t2))

(define r
  (render
   (window
    #:title "Book Flight"
    (vpanel
     (choice
      types
      #:choice->label ~type
      #:selection (@type . ~> . (λ (t) (index-of types t)))
      (λ:= @type))
     (input
      #:background-color (@t1 . ~> . ~color)
      (obs-peek @t1)
      (λ (_event text)
        (:= @t1 text)))
     (input
      #:background-color (@t2 . ~> . ~color)
      #:enabled? (@type . ~> . (λ (t) (eq? t 'return)))
      (obs-peek @t2)
      (λ (_event text)
        (:= @t2 text)))
     (button
      #:enabled? @valid?
      "Book"
      (λ ()
        (render
         (dialog
          #:title "Flight Booked"
          #:style '(no-sheet)
          (vpanel
           (text
            (obs-combine
             (λ (type t1 t2)
               (case type
                 [(one-way) (format "You've booked a one-way flight on ~a." t1)]
                 [(return) (format "You've booked a return flight on ~a (returning on ~a)." t1 t2)]))
             @type @t1 @t2))
           (hpanel
            (spacer)
            (button "OK" (λ ()
                           ((gui:application-quit-handler)))))))
         r)))))))
