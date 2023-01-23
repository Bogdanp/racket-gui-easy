#lang racket/base

(require racket/class
         (prefix-in gui: racket/gui)
         racket/match)

(provide
 the-default-keymap)

(define the-default-keymap
  (new gui:keymap%))

(define ((make-editor-operation id . args) editor _event)
  (apply dynamic-send editor id args))

(define (goto-start editor _event)
  (send editor set-position 0))

(define (goto-end editor _event)
  (send editor set-position (send editor last-position)))

(define mappings
  (case (system-type 'os)
    [(macosx)
     `(("c:a" goto-start ,goto-start)
       ("c:e" goto-end   ,goto-end)
       ("d:z" undo       ,(make-editor-operation 'undo))
       ("d:Z" redo       ,(make-editor-operation 'redo))
       ("d:a" select-all ,(make-editor-operation 'select-all))
       ("d:x" cut        ,(make-editor-operation 'cut))
       ("d:c" copy       ,(make-editor-operation 'copy))
       ("d:v" paste      ,(make-editor-operation 'paste)))]
    [else
     `(("home" goto-start ,goto-start)
       ("end"  goto-end   ,goto-end)
       ("c:z"  undo       ,(make-editor-operation 'undo))
       ("c:Z"  redo       ,(make-editor-operation 'redo))
       ("c:a"  select-all ,(make-editor-operation 'select-all))
       ("c:x"  cut        ,(make-editor-operation 'cut))
       ("c:c"  copy       ,(make-editor-operation 'copy))
       ("c:v"  paste      ,(make-editor-operation 'paste)))]))

(for ([mapping (in-list mappings)])
  (match-define (list binding name proc) mapping)
  (define name-str (symbol->string name))
  (send the-default-keymap add-function name-str proc)
  (send the-default-keymap map-function binding name-str))
