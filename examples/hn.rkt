#lang racket/base

(require net/http-easy
         racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list)

;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (async e0 e ...)
  (void (thread (λ () e0 e ...))))

(define (get-story id)
  (response-json
   (get (format "https://hacker-news.firebaseio.com/v0/item/~a.json" id))))

(define (get-stories which limit [concurrency limit])
  (define ids
    (take
     (response-json
      (get (format "https://hacker-news.firebaseio.com/v0/~a.json" which)))
     limit))
  (define stories-ch (make-channel))
  (define sema (make-semaphore concurrency))
  (for ([id (in-list ids)])
    (thread
     (lambda ()
       (call-with-semaphore sema
         (lambda ()
           (channel-put stories-ch (get-story id)))))))
  (define all-stories
    (for/list ([_ (in-range limit)])
      (channel-get stories-ch)))
  (for/list ([id (in-list ids)])
    (findf
     (λ (s)
       (= id (hash-ref s 'id)))
     all-stories)))

(define (get-top-stories limit [concurrency 20])
  (get-stories "topstories" limit concurrency))

(define (get-new-stories limit [concurrency 20])
  (get-stories "newstories" limit concurrency))


;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; either '(overview) or '(story data)
(define @mode (@ '(overview)))
(define @story (@mode . ~> . (λ (m)
                               (case (car m)
                                 [(story) (cadr m)]
                                 [else #f]))))

(define @sections (@ #(top new)))
(define @section (@ 'top))

(define @top-stories (@ null))
(define @new-stories (@ null))
(async (@top-stories . := . (get-top-stories 50)))
(async (@new-stories . := . (get-new-stories 50)))

(define section-list
  (hpanel
   #:min-size '(150 #f)
   #:stretch '(#f #t)
   (table
    #:style '(single)
    #:selection (obs-combine
                 (λ (sections section)
                   (index-of (vector->list sections) section))
                 @sections @section)
    #:entry->row (λ (section)
                   (vector (string-titlecase (symbol->string section))))
    '("Section") @sections
    (λ (event sections index)
      (when index
        (define section
          (vector-ref sections index))
        (case event
          [(select)
           (@section . := . section)]

          [(dclick)
           (case section
             [(top)
              (@top-stories . := . null)
              (async (@top-stories . := . (get-top-stories 50)))]

             [(new)
              (@new-stories . := . null)
              (async (@new-stories . := . (get-new-stories 50)))])]))))))

(define (story-list @mode @stories)
  (hpanel
   (cond/view
    [(@stories . ~> . null?)
     (text "Loading...")]

    [else
     (table
      '("Title" "Score" "By")
      (@stories . ~> . list->vector)
      #:entry->row (λ (s)
                     (vector (hash-ref s 'title)
                             (~a (hash-ref s 'score))
                             (~a (hash-ref s 'by))))
      (λ (event stories index)
        (case event
          [(dclick)
           (@mode . := . `(story ,(vector-ref stories index)))])))])))

(render
 (window
  #:size '(800 600)
  (cond/view
   [@story
    (vpanel
     #:alignment '(left top)
     (button "Back" (λ () (@mode . := . '(overview))))
     (text (@story . ~> . (λ (s) (if s (hash-ref s 'title) "")))))]

   [else
    (hpanel
     #:alignment '(left top)
     section-list
     (story-list
      @mode
      (obs-combine
       (λ (section top-stories new-stories)
         (case section
           [(top) top-stories]
           [(new) new-stories]))
       @section @top-stories @new-stories)))])))
