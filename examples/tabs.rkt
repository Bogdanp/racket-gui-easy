#lang racket/base

(require racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/match)

(struct person (id name age) #:transparent)

(define (person=? a b)
  (= (person-id a)
     (person-id b)))

(define (~person p)
  (match-define (person id name age) p)
  (format "~a #~a"
          (if age (format "~a (~a)" name age) name)
          id))

(define (~person-age p)
  (cond
    [(person-age p) => ~a]
    [else ""]))

(define (person-detail p [action void])
  (define/obs @person p)
  (vpanel
   (hpanel
    #:stretch '(#t #f)
    (text "Name:")
    (input
     (person-name p)
     (λ (_event text)
       (@person . <~ . (λ (p) (struct-copy person p [name text]))))))
   (hpanel
    #:stretch '(#t #f)
    (text "Age:")
    (input
     (~person-age p)
     (λ (_event text)
       (define maybe-age (string->number text))
       (when maybe-age
         (@person . <~ . (λ (p) (struct-copy person p [age maybe-age])))))))
   (hpanel
    #:stretch '(#t #f)
    #:alignment '(right center)
    (button "Save" (λ () (action (obs-peek @person)))))))

(struct state (seq people selection) #:transparent)

(define (state-add-person st)
  (match-define (state seq people _) st)
  (define next-id (add1 seq))
  (define new-person (person next-id "Unnamed" #f))
  (define new-people (append people (list new-person)))
  (struct-copy state st
               [seq next-id]
               [people new-people]
               [selection new-person]))

(define (state-update-person st id proc)
  (match-define (state _seq people _) st)
  (define new-people
    (for/list ([p (in-list people)])
      (if (eqv? (person-id p) id)
          (proc p)
          p)))
  (struct-copy state st [people new-people]))

(define ((make-person-setter p) st)
  (state-update-person st (person-id p) (λ (_) p)))

(define/obs @state
  (let ([p (person 0 "Bogdan" 30)])
    (state 0 (list p) p)))

(render
 (window
  #:size '(800 600)
  (tabs
   (@state . ~> . state-people)
   #:style '(no-border can-reorder can-close new-button flat-portable)
   #:choice->label ~person
   #:choice=? person=?
   #:selection (@state . ~> . state-selection)
   (lambda (event people selection)
     (case event
       [(new)
        (@state . <~ . state-add-person)]
       [(select)
        (@state . <~ . (λ (st) (struct-copy state st [selection selection])))]
       [(reorder)
        (@state . <~ . (λ (st) (struct-copy state st [people people])))]
       [(close)
        (@state . <~ . (λ (st) (struct-copy state st
                                            [people people]
                                            [selection selection])))]))
   (dyn-view
    (@state . ~> . state-selection)
    (λ (p)
      (if p
          (person-detail p (λ (updated-p)
                             (@state . <~ . (make-person-setter updated-p))))
          (spacer)))))))
