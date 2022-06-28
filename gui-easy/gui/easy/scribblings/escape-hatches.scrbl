#lang scribble/manual

@(require (for-label racket/base
                     racket/class
                     (prefix-in gui: racket/gui)
                     racket/gui/easy
                     racket/gui/easy/operator))

@title{Escape Hatches}

Some views take a @racket[#:mixin] argument that can be used to alter
the behavior of the underlying widget.  These are intended to be used
as ``escape hatches'' when the library doesn't provide a piece of
functionality you need, but that functionality is available on the
native widget.

See "examples/close-window.rkt" for a example of using a mixin to
programmatically toggle a window's visibility.
