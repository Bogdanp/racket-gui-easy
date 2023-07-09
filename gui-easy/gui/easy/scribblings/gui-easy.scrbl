#lang scribble/manual

@(require scribble/core
          scribble/html-properties
          scribble/latex-properties
          racket/runtime-path
          (for-label racket/base
                     racket/gui/easy
                     racket/gui/easy/operator))

@title{@tt{gui-easy}: Declarative GUIs}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

This library provides a declarative API on top of
@racketmodname[racket/gui].  This library is still a work in progress,
so expect some breaking changes.

@(define-runtime-path youtubestub.tex "youtubestub.tex")
@(define embed-style
  (make-style
   "youtubeembed"
   (list
    (make-tex-addition youtubestub.tex)
    (make-alt-tag "iframe")
    (make-attributes '((width           . "700")
                       (height          . "394")
                       (src             . "https://www.youtube.com/embed/AXJ9tTVGDwU")
                       (frameborder     . "0")
                       (allowfullscreen . ""))))))

@element[embed-style]{
}

@include-section["quickstart.scrbl"]
@include-section["geometry.scrbl"]
@include-section["custom-views.scrbl"]
@include-section["escape-hatches.scrbl"]
@include-section["reference.scrbl"]
