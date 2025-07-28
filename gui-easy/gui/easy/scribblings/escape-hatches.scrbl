#lang scribble/manual

@(require (for-label racket/base
                     racket/class
                     (prefix-in gui: racket/gui)
                     racket/gui/easy
                     racket/gui/easy/operator))

@; Example links cited within this section
@(define example-link-close-window
  (link "https://github.com/Bogdanp/racket-gui-easy/blob/master/examples/close-window.rkt"
        @filepath{examples/close-window.rkt}))

@title{Escape Hatches}

Some views take a @racket[#:mixin] argument that can be used to alter
the behavior of the underlying widget.  These are intended to be used
as ``escape hatches'' when the library doesn't provide a piece of
functionality you need, but that functionality is available on the
native widget.

See @|example-link-close-window| for an example of using a mixin to
programmatically toggle a window's visibility.
