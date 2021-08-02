#lang scribble/manual

@(require (for-label racket/base
                     racket/gui/easy
                     racket/gui/easy/operator))

@title{Geometry Management}

@(define geometry-mgmt-doc
   (secref "containeroverview" #:doc '(lib "scribblings/gui/gui.scrbl")))

See @geometry-mgmt-doc in the @racketmodname[racket/gui] docs for
details on how views get laid out.

@secref["containers"], @secref["windows&dialogs"] take optional
keyword arguments that allow you to control the @racket[#:spacing] and
@racket[#:alignment] of their children and their own
@racket[#:min-size], @racket[#:stretch] and @racket[#:margin].  All of
these arguments can be passed as either regular values or as
@tech{observables}, in which case the properties they control will
vary with changes to the observables.
