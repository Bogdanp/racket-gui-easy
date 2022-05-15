#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/gui/easy
         racket/gui/easy/operator)

(struct buffer (path text) #:transparent)

(define (buffer-name b)
  (define p (buffer-path b))
  (define-values (_base filename _must-be-dir?)
    (split-path p))
  (path->string filename))

(define/obs @buffers null)
(define/obs @buffer #f)

(define editor-canvas%
  (class* object% (view<%>)
    (init-field @editor)
    (super-new)

    (define/public (dependencies)
      (list @editor))

    (define/public (create parent)
      (new gui:editor-canvas%
           [parent parent]
           [editor (obs-peek @editor)]))

    (define/public (update v what val)
      (case/dep what
        [@editor
         (send v set-editor val)]))

    (define/public (destroy v)
      (void))))

(define (editor-canvas @editor)
  (new editor-canvas% [@editor @editor]))

(define editor-styles
  (let ([sl (new gui:style-list%)])
    (define dt (new gui:style-delta%))
    (send dt set-delta-face "Dank Mono" 'modern)
    (send dt set-delta 'change-weight 'normal)
    (define style (send sl find-or-create-style (send sl basic-style) dt))
    (define _std (send sl new-named-style "Standard" style))
    sl))

(render
 (window
  #:size '(600 600)
  (button
   "Open file..."
   (λ ()
     (define filename (gui:get-file))
     (when filename
       (thread
        (lambda ()
          (define text (new gui:text%))
          (send text load-file filename)
          (send text set-style-list editor-styles)
          (@buffers . <~ . (λ (buffers)
                             (define buf (buffer filename text))
                             (begin0 (append buffers (list buf))
                               (@buffer . := . buf)))))))))
  (tabs
   #:style '(no-border can-close can-reorder)
   #:selection @buffer
   #:choice->label buffer-name
   #:choice=? eq?
   @buffers
   (λ (event bs b)
     (case event
       [(close)
        (@buffers . := . bs)
        (@buffer . := . b)]

       [(reorder)
        (@buffers . := . bs)]

       [(select)
        (@buffers . := . bs)
        (@buffer . := . b)]))
   (editor-canvas (@buffer . ~> . (λ (maybe-b) (and maybe-b (buffer-text maybe-b))))))))
