#lang racket/base

(require (only-in browser/external send-url)
         (prefix-in cl: canvas-list)
         json
         net/http-easy
         (prefix-in p: pict)
         racket/class
         racket/format
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/match)

;; Data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct story (id author title url score text comment-ids comments) #:transparent)
(struct comment (id author text deleted?) #:transparent)

(define (hash->story h)
  (story
   (hash-ref h 'id)
   (hash-ref h 'by)
   (hash-ref h 'title "<untitled>")
   (hash-ref h 'url #f)
   (hash-ref h 'score 0)
   (hash-ref h 'text #f)
   (hash-ref h 'kids null)
   null))

(define (hash->comment h)
  (comment
   (hash-ref h 'id)
   (hash-ref h 'by "anon")
   (hash-ref h 'text "")
   (hash-ref h 'deleted #f)))


;; API ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (async e0 e ...)
  (void (thread (λ () e0 e ...))))

(define (get-item id)
  (response-json
   (get (format "https://hacker-news.firebaseio.com/v0/item/~a.json" id))))

(define (get-items ids [concurrency (length ids)])
  (define items-ch (make-channel))
  (define sema (make-semaphore concurrency))
  (for ([id (in-list ids)])
    (thread
     (lambda ()
       (call-with-semaphore sema
         (lambda ()
           (channel-put items-ch (get-item id)))))))
  (define all-items
    (for/list ([_ (in-range (length ids))])
      (channel-get items-ch)))
  (filter values
          (for/list ([id (in-list ids)])
            (findf
             (λ (it)
               (and (not (eq? it (json-null)))
                    (= id (hash-ref it 'id))))
             all-items))))

(define (get-stories which limit [concurrency limit])
  (define ids
    (take
     (response-json
      (get (format "https://hacker-news.firebaseio.com/v0/~a.json" which)))
     limit))
  (get-items ids concurrency))

(define (get-top-stories limit [concurrency 20])
  (map hash->story (get-stories "topstories" limit concurrency)))

(define (get-new-stories limit [concurrency 20])
  (map hash->story (get-stories "newstories" limit concurrency)))

(define (load-comments s [concurrency 20])
  (struct-copy story s [comments (map hash->comment (get-items (story-comment-ids s) concurrency))]))

(define (visit it)
  (cond
    [(and (story? it) (story-url it)) => send-url]
    [(story? it) (send-url (format "https://news.ycombinator.com/item?id=~a" (story-id it)))]
    [else (send-url (format "https://news.ycombinator.com/item?id=~a" (comment-id it)))]))


;; Helpers and Widgets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define title-font (font "SF Pro" 18 #:weight 'medium))
(define subtitle-font (font "SF Pro" 14 #:weight 'medium))
(define regular-font (font "SF Pro" 12 #:weight 'medium))
(define title-color (color "black"))
(define highlight-color (color "blue"))
(define points-color (color "orange"))
(define regular-color (color #x50 #x50 #x50))
(define inverted-color (color "white"))

;; A custom view<%> to display a list of picts.
(define pict-list%
  (class* object% (view<%>)
    (init-field @items item->pict item-height action)
    (super-new)

    (define/public (dependencies)
      (list @items))

    (define/public (create parent)
      (new cl:canvas-list%
           [parent parent]
           [hover-color (color #xF9 #xF9 #xF9)]
           [selection-color (color #x00 #x00 #xFF)]
           [items (obs-peek @items)]
           [item-height item-height]
           [paint-item-callback (λ (_self item state dc _w _h)
                                  (p:draw-pict (item->pict item state) dc 10 10))]
           [action-callback (λ (_self item _event)
                              (action item))]))

    (define/public (update v what val)
      (when (eq? what @items)
        (send v set-items val)))

    (define/public (destroy _v)
      (void))))

(define (pict-list @items item->pict action
                   #:item-height item-height)
  (new pict-list%
       [@items @items]
       [item->pict item->pict]
       [item-height item-height]
       [action action]))

(define (styled-text t)
  (apply p:hbl-append (for/list ([t (in-list t)])
                        (match t
                          [(? string? text) (p:colorize (p:text text regular-font) regular-color)]
                          [`(,color ,text) (p:colorize (p:text text regular-font) color)]
                          [`(,font ,color ,text) (p:colorize (p:text text font) color)]))))

(define (story-pict s)
  (p:vl-append
   5
   (styled-text `((,title-font ,title-color ,(story-title s))))
   (styled-text `((,subtitle-font ,highlight-color ,(or (story-url s) "show"))))
   (styled-text `((,points-color ,(~a (story-score s)))
                  " points by "
                  (,highlight-color ,(story-author s))))))

(define (comment-pict c state)
  (define username (comment-author c))
  (p:vl-append
   5
   (p:lt-superimpose
    (p:dc
     (λ (dc dx dy)
       (define old-brush (send dc get-brush))
       (define old-pen (send dc get-pen))
       (define-values (w h _d _v)
         (send dc get-text-extent username regular-font))
       (send dc set-smoothing 'smoothed)
       (send dc set-pen "black" 0 'transparent)
       (send dc set-brush inverted-color 'solid)
       (send dc draw-rounded-rectangle dx dy (+ w 8) (+ h 4) 5)
       (send dc set-pen old-pen)
       (send dc set-brush old-brush))
     0 0)
    (p:inset
     (styled-text `((,points-color ,(comment-author c))))
     4 2))
   (styled-text
    `((,(case state
          [(selected) inverted-color]
          [else regular-color])
       ,(comment-text c))))))


;; GUI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define/obs @mode '(overview))
(define/obs @story (@mode . ~> . (match-lambda
                                   [`(story ,s) s]
                                   [_ #f])))

(define/obs @sections #(top new))
(define/obs @section 'top)

(define/obs @top-stories null)
(define/obs @new-stories null)
(async (@top-stories . := . (get-top-stories 50)))
(async (@new-stories . := . (get-new-stories 50)))

(define (section-list)
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

(define (story-list @m @ss)
  (hpanel
   (cond-view
    [(@ss . ~> . null?)
     (text "Loading...")]

    [else
     (table
      '("Title" "Score" "By")
      #:column-widths '((0 350))
      (@ss . ~> . list->vector)
      #:entry->row (λ (s)
                     (vector (story-title s)
                             (~a (story-score s))
                             (~a (story-author s))))
      (λ (event stories index)
        (case event
          [(dclick)
           (define s (vector-ref stories index))
           (@m . := . `(story ,s))
           (async (@m . := . `(story ,(load-comments s))))])))])))

(define (story-view @m @s)
  (vpanel
   #:alignment '(left top)
   (hpanel
    #:stretch '(#t #f)
    (button "Back" (λ () (@m . := . '(overview))))
    (spacer)
    (button "Visit" (λ () (visit (obs-peek @s)))))
   (vpanel
    (pict-canvas
     #:style '(transparent)
     #:stretch '(#t #f)
     #:min-size '(#f 90)
     @s
     (λ (s)
       (p:inset (story-pict s) 10 10)))
    (pict-list
     (@s . ~> . (λ (s) (if s (filter-not comment-deleted? (story-comments s)) null)))
     #:item-height 60
     (λ (c state)
       (comment-pict c state))
     (λ (c)
       (visit c))))))

(define app
  (window
   #:title "Hacker News"
   #:size '(800 600)
   (if-view @story
            (story-view @mode @story)
            (hpanel
             #:alignment '(left top)
             (section-list)
             (story-list
              @mode
              (obs-combine
               (λ (section top-stories new-stories)
                 (case section
                   [(top) top-stories]
                   [(new) new-stories]))
               @section @top-stories @new-stories))))))

(module+ main
  (render app))
