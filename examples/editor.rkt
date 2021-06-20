#lang racket/base

(require (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator
         racket/port)

(struct buffer (path contents) #:transparent)

(define (buffer-name b)
  (define p (buffer-path b))
  (define-values (_base filename _must-be-dir?)
    (split-path p))
  (path->string filename))

(define @buffers (@ null))
(define @buffer-index (@ #f))
(define @current-buffer
  (obs-combine
   (λ (buffers index)
     (and index (list-ref buffers index)))
   @buffers @buffer-index))

(render
 (window
  #:size '(600 600)
  (button
   "Open file..."
   (λ ()
     (define filename (gui:get-file))
     (when filename
       (define new-buffers
         (@buffers . <~ . (λ (buffers)
                            (define contents (call-with-input-file filename port->string))
                            (append buffers (list (buffer filename contents))))))
       (@buffer-index . := . (sub1 (length new-buffers))))))
  (tabs
   #:style '(no-border can-close can-reorder)
   #:choice->label buffer-name
   #:selection @buffer-index
   @buffers
   (λ (event buffers index)
     (case event
       [(close)
        (when (= (obs-peek @buffer-index) index)
          (@buffer-index . := . 0))
        (@buffers . := . (for/list ([b (in-list buffers)]
                                    [i (in-naturals)]
                                    #:unless (= i index))
                           b))]

       [(reorder)
        (@buffer-index . := . index)
        (@buffers . := . buffers)]

       [(select)
        (@buffer-index . := . index)]))
   (input
    #:font (font "Operator Mono" 12 #:weight 'light #:family 'modern)
    #:style '(multiple)
    (@current-buffer . ~> . (λ (b)
                              (cond
                                [b (buffer-contents b)]
                                [else ""])))))))
