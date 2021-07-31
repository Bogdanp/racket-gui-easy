#lang scribble/manual

@(require (for-label racket/base
                     racket/gui/easy
                     racket/gui/easy/operator))

@title{@tt{gui-easy}: Declarative GUIs}
@author[(author+email "Bogdan Popa" "bogdan@defn.io")]

This library provides a declarative API on top of
@racketmodname[racket/gui].  This library is still a work in progress,
so expect some breaking changes.

@include-section["quickstart.scrbl"]
@include-section["reference.scrbl"]
