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

The goal of GUI Easy is to simplify user interface
construction in Racket by wrapping the existing imperative API
(@racketmodname[racket/gui]) in a functional shell.

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

@(bibliography
  @(bib-entry #:key "knoblepopa23"
              #:title "Functional Shell and Reusable Components for Easy GUIs"
              #:author "D. B. Knoble and B. Popa"
              #:location "FUNARCH 2023: Proceedings of the 1st ACM SIGPLAN International Workshop on Functional Software Architecture"
              #:date "31 August 2023 "
              #:url "https://arxiv.org/abs/2308.16024")
  @(bib-entry #:key "repo-link"
              #:title "Declarative GUIs in Racket"
              #:author "B. Popa"
              #:location "Online"
              #:date "Accessed: 28 July 2025"
              #:url "https://github.com/Bogdanp/racket-gui-easy"))
