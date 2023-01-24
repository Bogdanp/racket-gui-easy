#lang racket/base

(require (for-syntax racket/base)
         racket/class
         (prefix-in gui: racket/gui))

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

(define (forward-word editor _event)
  (define pos (box (send editor get-end-position)))
  (send editor find-wordbreak #f pos 'caret)
  (send editor set-position (unbox pos)))

(define (backward-word editor _event)
  (define pos (box (send editor get-start-position)))
  (send editor find-wordbreak pos #f 'caret)
  (send editor set-position (unbox pos)))

(define (delete-word-backward editor _event)
  (define start-pos (send editor get-start-position))
  (define end-pos
    (let ([b (box start-pos)])
      (send editor find-wordbreak b #f 'caret)
      (unbox b)))
  (unless (= start-pos end-pos)
    (send editor delete end-pos start-pos)))

(define-syntax (define-proc stx)
  (syntax-case stx ()
    [(_ id) #'(define-proc id id)]
    [(_ id e) #'(send the-default-keymap add-function (symbol->string 'id) e)]))

(define-syntax-rule (define-procs binder ...)
  (begin (define-proc . binder) ...))

(define-syntax-rule (bind [binding id] ...)
  (begin (send the-default-keymap map-function binding (symbol->string 'id)) ...))

(define-procs
  [backward-word]
  [copy (make-editor-operation 'copy)]
  [cut (make-editor-operation 'cut)]
  [delete-word-backward]
  [forward-word]
  [goto-end]
  [goto-start]
  [paste (make-editor-operation 'paste)]
  [redo (make-editor-operation 'redo)]
  [select-all (make-editor-operation 'select-all)]
  [undo (make-editor-operation 'undo)])

(case (system-type 'os)
  [(macosx)
   (bind
    ["a:backspace" delete-word-backward]
    ["a:left"      backward-word]
    ["a:right"     forward-word]
    ["c:a"         goto-start]
    ["c:e"         goto-end]
    ["d:Z"         redo]
    ["d:c"         copy]
    ["d:v"         paste]
    ["d:x"         cut]
    ["d:z"         undo])]
  [else
   (bind
    ["c:Z"         redo]
    ["c:a"         select-all]
    ["c:backspace" delete-word-backward]
    ["c:c"         copy]
    ["c:left"      backward-word]
    ["c:right"     forward-word]
    ["c:v"         paste]
    ["c:x"         cut]
    ["c:z"         undo]
    ["end"         goto-end]
    ["home"        goto-start])])
