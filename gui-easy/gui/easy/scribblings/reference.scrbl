#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract
                     racket/class
                     racket/gui/easy
                     racket/gui/easy/operator
                     (prefix-in gui: racket/gui)))

@title{Reference}
@defmodule[racket/gui/easy]

@section{Renderers}

@deftech{Renderers} convert view definitions to GUI elements.

@defproc[(renderer? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{renderer}.
}

@defproc[(render [view (is-a?/c window-view<%>)]
                 [parent (or/c #f renderer?) #f]) renderer?]{
  Renders the view hierarchy represented by @racket[view].  If a
  @racket[parent] is provided, then the view is rendered as a child of
  the root view of that parent.  This is useful, for example, if you
  need to render a modal dialog on top of an existing window.
}

@defproc[(render-popup-menu [parent renderer?]
                            [view (is-a?/c popup-menu-view<%>)]
                            [x gui:position-integer?]
                            [y gui:position-integer?]) void?]{

  Renders the popup menu represented by @racket[view] as a child of
  @racket[parent].
}


@section{Views}

@defproc[(window [#:title title (maybe-obs/c string?) "Untitled"]
                 [#:size size
                         (maybe-obs/c
                          (list/c
                           (or/c #f gui:dimension-integer?)
                           (or/c #f gui:dimension-integer?)))
                         '(#f #f)]
                 [#:alignment alignment
                              (maybe-obs/c
                               (list/c (or/c 'left 'center 'right)
                                       (or/c 'top 'center 'bottom)))
                              '(center top)]
                 [#:position position
                             (maybe-obs/c
                              (or/c 'center
                                    (list/c gui:position-integer?
                                            gui:position-integer?)))
                             'center]
                 [#:min-size min-size
                             (maybe-obs/c
                              (list/c
                               (or/c boolean? gui:dimension-integer?)
                               (or/c boolean? gui:dimension-integer?)))
                             '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c (list/c boolean? boolean?))
                            '(#t #t)]
                 [#:style style
                          (listof (or/c 'no-resize-border 'no-caption
                                        'no-system-menu 'hide-menu-bar
                                        'toolbar-button 'float 'metal
                                        'fullscreen-button 'fullscreen-aux))
                          null]
                 [child (is-a?/c view<%>)] ...+) (is-a?/c window-view<%>)]{

  Returns a @racket[window-view<%>] that represents a top level
  window.
}

@defproc[(dialog [#:title title (maybe-obs/c string?) "Untitled"]
                 [#:size size
                         (maybe-obs/c
                          (list/c
                           (or/c #f gui:dimension-integer?)
                           (or/c #f gui:dimension-integer?)))
                         '(#f #f)]
                 [#:alignment alignment
                              (maybe-obs/c
                               (list/c (or/c 'left 'center 'right)
                                       (or/c 'top 'center 'bottom)))
                              '(center top)]
                 [#:position position
                             (maybe-obs/c
                              (or/c 'center
                                    (list/c gui:position-integer?
                                            gui:position-integer?)))
                             'center]
                 [#:min-size min-size
                             (maybe-obs/c
                              (list/c
                               (or/c boolean? gui:dimension-integer?)
                               (or/c boolean? gui:dimension-integer?)))
                             '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c (list/c boolean? boolean?))
                            '(#t #t)]
                 [#:style style
                          (listof (or/c 'no-caption 'no-sheet 'resize-border 'close-button))
                          '(close-button)]
                 [child (is-a?/c view<%>)] ...+) (is-a?/c window-view<%>)]{

  Returns a @racket[window-view<%>] that represents a dialog.
}

@defproc[(hpanel [#:alignment alignment
                              (maybe-obs/c
                               (list/c (or/c 'left 'center 'right)
                                       (or/c 'top 'center 'bottom)))
                              '(center top)]
                 [#:style style
                          (listof (or/c 'border 'deleted
                                        'hscroll 'auto-hscroll 'hide-hscroll
                                        'vscroll 'auto-vscroll 'hide-vscroll))
                          null]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:spacing spacing (maybe-obs/c gui:spacing-integer?) 0]
                 [#:margin margin
                           (maybe-obs/c
                            (list/c gui:spacing-integer?
                                    gui:spacing-integer?))
                           '(0 0)]
                 [#:min-size min-size
                             (maybe-obs/c
                              (list/c
                               (or/c boolean? gui:dimension-integer?)
                               (or/c boolean? gui:dimension-integer?)))
                             '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c (list/c boolean? boolean?))
                            '(#t #t)]

                 [child (is-a?/c view<%>)] ...+) (is-a?/c view<%>)]{

  Returns a panel that lays out its children horizontally.
}

@defproc[(vpanel [#:alignment alignment
                              (maybe-obs/c
                               (list/c (or/c 'left 'center 'right)
                                       (or/c 'top 'center 'bottom)))
                              '(center top)]
                 [#:style style
                          (listof (or/c 'border 'deleted
                                        'hscroll 'auto-hscroll 'hide-hscroll
                                        'vscroll 'auto-vscroll 'hide-vscroll))
                          null]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:spacing spacing (maybe-obs/c gui:spacing-integer?) 0]
                 [#:margin margin
                           (maybe-obs/c
                            (list/c gui:spacing-integer?
                                    gui:spacing-integer?))
                           '(0 0)]
                 [#:min-size min-size
                             (maybe-obs/c
                              (list/c
                               (or/c boolean? gui:dimension-integer?)
                               (or/c boolean? gui:dimension-integer?)))
                             '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c (list/c boolean? boolean?))
                            '(#t #t)]
                 [child (is-a?/c view<%>)] ...+) (is-a?/c view<%>)]{

  Returns a panel that lays out its children vertically.
}

@defproc[(button [label (maybe-obs/c gui:label-string?)]
                 [action (-> any)]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:style style (listof (or/c 'border 'multi-line 'deleted)) null]
                 [#:margin margin
                           (maybe-obs/c
                            (list/c gui:spacing-integer?
                                    gui:spacing-integer?))
                           '(0 0)]
                 [#:min-size min-size
                             (maybe-obs/c
                              (list/c
                               (or/c boolean? gui:dimension-integer?)
                               (or/c boolean? gui:dimension-integer?)))
                             '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c (list/c boolean? boolean?))
                            '(#t #t)]) (is-a?/c view<%>)]{

  Returns a button that calls @racket[action] when clicked.
}

@defproc[(text [s (maybe-obs/c gui:label-string?)]) (is-a?/c view<%>)]{
  Returns a textual label.
}

@defproc[(list-view [o (maybe-obs/c list?)]
                    [make-view (-> any/c any/c (is-a?/c view<%>))]
                    [#:key key (-> any/c any/c) values]
                    [#:alignment alignment
                                 (maybe-obs/c
                                  (list/c (or/c 'left 'center 'right)
                                          (or/c 'top 'center 'bottom)))
                                 '(center top)]
                    [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                    [#:style style
                             (listof (or/c 'horizontal 'vertical 'border 'deleted
                                           'hscroll 'auto-hscroll 'hide-hscroll
                                           'vscroll 'auto-vscroll 'hide-vscroll))
                             null]
                    [#:spacing spacing (maybe-obs/c gui:spacing-integer?) 0]
                    [#:margin margin
                              (maybe-obs/c
                               (list/c gui:spacing-integer?
                                       gui:spacing-integer?))
                              '(0 0)]
                    [#:min-size min-size
                                (maybe-obs/c
                                 (list/c
                                  (or/c boolean? gui:dimension-integer?)
                                  (or/c boolean? gui:dimension-integer?)))
                                '(#f #f)]
                    [#:stretch stretch
                               (maybe-obs/c (list/c boolean? boolean?))
                               '(#t #t)]) (is-a?/c view<%>)]{
}

@subsection{Interfaces}

@definterface[view<%> ()]{
  A @racket[view<%>] object is a wrapper around a GUI object that
  knows what its data dependecies are and how to respond to their
  changes.

  @defmethod[(dependencies) (listof obs?)]{
    Returns the set of observers that this view depends on.
  }

  @defmethod[(create [parent (is-a?/c gui:area-container<%>)]) (is-a?/c gui:area<%>)]{
    Instantiates the underlying GUI object, associates it with
    @racket[parent] and returns it so that the parent of this
    @racket[view<%>] can manage it.
  }

  @defmethod[(update [v (is-a?/c gui:area<%>)]
                     [dep obs?]
                     [val any/c]) void?]{
     Responds to a change to the contents of @racket[dep].  The
     @racket[val] argument is the most recent value of @racket[dep]
     and the @racket[v] argument is the GUI object created by
     @racket[create].
  }

  @defmethod[(destroy [v (is-a?/c gui:area<%>)]) void?]{
    Destroys the GUI object @racket[v] and performs any necessary
    cleanup.
  }
}

@definterface[window-view<%> (view<%>)]{
  A @racket[window-view<%>] is like a regular @racket[view<%>] but its
  @racket[create] method has additional constraints placed on it.

  @defmethod[(create [parent (or/c (is-a?/c gui:frame%)
                                   (is-a?/c gui:dialog%)
                                   #f)]) (is-a?/c gui:window<%>)]{
    Returns a new @racket[gui:window<%>] belonging to @racket[parent].
  }
}

@definterface[popup-menu-view<%> (view<%>)]{
  A @racket[popup-menu-view<%>] is like a regular @racket[view<%>] but
  its @racket[create] method has additional constraints placed on it.

  @defmethod[(create [parent #f]) (is-a?/c gui:popup-menu%)]{
    Returns a new @racket[gui:popup-menu%].
  }
}


@section{Observables}

@deftech{Observables} are containers for values that may change over
time.  Their changes may be observed by arbitrary functions.

@; Require the private module to avoid requiring racket/gui/base.
@(define obs-sb (make-base-eval '(require racket/gui/easy/private/observable)))

@examples[
  #:eval obs-sb
  (define |@ints| (obs 0))
  (obs-observe! |@ints| (λ (v) (printf "observer 1 got ~s~n" v)))
  (obs-observe! |@ints| (λ (v) (printf "observer 2 got ~s~n" v)))
  (obs-update! |@ints| add1)
]

@deftech{Mapped observables} are @tech{observables} whose values
depend on other observables.  Mapped observables cannot be updated
using @racket[obs-update!].

@defproc[(obs? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is an @tech{observable}.
}

@defproc[(maybe-obs/c [c contract?]) contract?]{
  Returns a contract that accepts an @racket[obs?] whose values
  conform to @racket[c] or a value that conforms to @racket[c].
}

@defproc[(obs [v any/c]) obs?]{
  Returns a new observable, whose intiial value is @racket[v].
}

@defproc[(obs-observe! [o obs?]
                       [f (-> any/c any/c)]) void?]{
  Registers @racket[f] as an observer of @racket[o], applying it to
  the value contained by @racket[o] every time it changes.
}

@defproc[(obs-unobserve! [o obs?]
                         [f (-> any/c any/c)]) void?]{
  Removes @racket[f] from @racket[o]'s set of observers.
}

@defproc[(obs-update! [o obs?]
                      [f (-> any/c any/c)]) any/c]{
  Updates the value within @racket[o] by applying @racket[f] to it and
  storing the result.  Returns the new value.
}

@defproc[(obs-peek [o obs?]) any/c]{
  Returns the current value contained within @racket[o].
}

@defproc[(obs-map [o obs?]
                  [f (-> any/c any/c)]) obs?]{
  Returns a new @tech{mapped observable} whose value changes every
  time @racket[o]'s value does.  The values held by the new observable
  are mapped using @racket[f].
}

@defproc[(obs-combine [f (-> any/c ...+ any/c)]
                      [o obs?] ...+) obs?]{
  Returns a new @tech{mapped observable} whose value changes every
  time one of the @racket[o]s change.  The values held by the new
  observable are the values of the @racket[o]s combined via
  @racket[f].
}

@defproc[(obs-debouce [o obs?]
                      [#:duration duration-ms exact-nonnegative-integer? 200]) obs?]{
  Returns a new @tech{mapped observable} based on @racket[o], whose
  values change at most once every @racket[duration-ms] milliseconds.
}


@section{Observable Operators}
@defmodule[racket/gui/easy/operator]

@defproc[(|@| [v any/c]) obs?]{
  Converts @racket[v] into an @tech{observable}.  If @racket[v] is
  already an observable, it is returned unchanged.
}

@defproc[(:= [o obs?]
             [v any/c]) any/c]{
  Changes the value of @racket[o] to @racket[v].
}

@defproc[((λ:= [o obs?]
               [f (-> any/c any/c)]) [v any/c]) any/c]{
  Changes the value of @racket[o] to the result of @racket[(f v)].
}

@defproc[(<~ [o obs?]
             [f (-> any/c any/c)]) any/c]{
  An alias for @racket[obs-update!].
}

@defproc[(~> [o obs?]
             [f (-> any/c any/c)]) obs?]{
  An alias for @racket[obs-map].
}
