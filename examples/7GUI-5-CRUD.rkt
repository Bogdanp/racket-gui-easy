#lang racket/base

(require racket/gui/easy
         racket/gui/easy/operator
         racket/string)

(struct person (name surname) #:transparent)
(define (~person p)
  (format "~a, ~a" (person-surname p) (person-name p)))
(define (person-row p)
  (vector (~person p)))

(define/obs @people
  (list
   (person "Emil" "Hans")
   (person "Max" "Mustermann")
   (person "Roman" "Tisch")))
(define/obs @prefix "")
(define/obs @filtered-people
  (obs-combine
   (λ (people prefix)
     (for/list ([p (in-list people)]
                #:when (string-prefix? (person-surname p) prefix))
       p))
   @people @prefix))
(define/obs @current-person #f)
(define/obs @current-name "")
(define/obs @current-surname "")

(define (field label v)
  (hpanel
   #:stretch '(#t #f)
   (hpanel
    #:stretch '(#f #t)
    #:min-size '(70 #f)
    (text label))
   v))

(render
 (window
  #:title "CRUD"
  (hpanel
   #:min-size '(#f 200)
   (vpanel
    (hpanel
     #:stretch '(#t #f)
     (text "Filter prefix:")
     (input @prefix (λ (_event text)
                      (@prefix . := . text))))
    (table
     '("Full Name")
     (@filtered-people . ~> . list->vector)
     #:entry->row person-row
     (λ (_event entries selection)
       (when selection
         (define p (vector-ref entries selection))
         (@current-name . := . (person-name p))
         (@current-surname . := . (person-surname p))
         (@current-person . := . p)))))
   (vpanel
    (hpanel
     #:stretch '(#t #f)
     #:min-size '(#f 20))
    (field
     "Name:"
     (input @current-name
            (λ (_event text)
              (@current-name . := . text))))
    (field
     "Surname:"
     (input @current-surname
            (λ (_event text)
              (@current-surname . := . text))))))
  (hpanel
   (button
    "Create"
    (λ ()
      (define name (obs-peek @current-name))
      (define surname (obs-peek @current-surname))
      (unless (or (string=? name "")
                  (string=? surname ""))
        (@people . <~ . (λ (people)
                          (append people (list (person name surname))))))))
   (button
    #:enabled? @current-person
    "Update"
    (λ ()
      (define name (obs-peek @current-name))
      (define surname (obs-peek @current-surname))
      (unless (or (string=? name "")
                  (string=? surname ""))
        (define new-p (person name surname))
        (@people . <~ . (λ (people)
                          (define sel (obs-peek @current-person))
                          (for/list ([p (in-list people)])
                            (if (eq? sel p) new-p p))))
        (@current-person . := . new-p))))
   (button
    #:enabled? @current-person
    "Delete"
    (λ ()
      (@people . <~ . (λ (people)
                        (remq (obs-peek @current-person) people)))
      (@current-person . := . #f))))))
