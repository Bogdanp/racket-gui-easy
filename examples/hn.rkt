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
         racket/list)

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
  (get-stories "topstories" limit concurrency))

(define (get-new-stories limit [concurrency 20])
  (get-stories "newstories" limit concurrency))

(define (load-children it [concurrency 20])
  (hash-set it 'children (get-items (hash-ref it 'kids null) concurrency)))


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
           (define story (vector-ref stories index))
           (@mode . := . `(story ,story))
           (async (@mode . := . `(story ,(load-children story))))])))])))

(define title-font (font "SF Pro" 18 #:weight 'medium))
(define subtitle-font (font "SF Pro" 14 #:weight 'medium))
(define regular-font (font "SF Pro" 12 #:weight 'medium))
(define highlight-color (color "blue"))
(define points-color (color "orange"))
(define regular-color (color 80 80 80))
(define inverted-color (color "white"))

(define (story-pict s)
  (p:vl-append
   5
   (p:text (hash-ref s 'title) title-font)
   (p:colorize (p:text (hash-ref s 'url "show") subtitle-font) highlight-color)
   (p:hc-append
    5
    (p:colorize (p:text (number->string (hash-ref s 'score)) regular-font) points-color)
    (p:colorize (p:text "points by" regular-font) regular-color)
    (p:colorize (p:text (hash-ref s 'by) regular-font) highlight-color))))

(define (comment-pict c state)
  (define username (hash-ref c 'by "anon"))
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
     (p:colorize (p:text (hash-ref c 'by "anon") regular-font) points-color)
     4 2))
   (p:colorize
    (p:text (hash-ref c 'text) regular-font)
    (case state
      [(selected) inverted-color]
      [else regular-color]))))

(define canvas-list%
  (class* object% (view<%>)
    (init-field @items item->pict item-height)
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
                                  (p:draw-pict (item->pict item state) dc 10 10))]))

    (define/public (update v what val)
      (when (eq? what @items)
        (send v set-items val)))

    (define/public (destroy _v)
      (void))))

(define (canvas-list @items item->pict
                     #:item-height item-height)
  (new canvas-list%
       [@items @items]
       [item->pict item->pict]
       [item-height item-height]))

(render
 (window
  #:title "Hacker News"
  #:size '(800 600)
  (cond/view
   [@story
    (vpanel
     #:alignment '(left top)
     (hpanel
      #:stretch '(#t #f)
      (button "Back" (λ ()
                       (@mode . := . '(overview))))
      (spacer)
      (button "Visit" (λ ()
                        (send-url (hash-ref (obs-peek @story) 'url)))))
     (vpanel
      (canvas
       #:style '(transparent)
       #:stretch '(#t #f)
       #:min-size '(#f 90)
       @story
       (λ (dc story)
         (send dc set-smoothing 'smoothed)
         (p:draw-pict (story-pict story) dc 10 10)))
      (canvas-list
       (@story . ~> . (λ (story)
                        (let ([story (or story (hash))])
                          (for/list ([comment (in-list (hash-ref story 'children null))]
                                     #:unless (hash-ref comment 'deleted #f))
                            comment))))
       #:item-height 60
       (λ (c state)
         (comment-pict c state)))))]

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
