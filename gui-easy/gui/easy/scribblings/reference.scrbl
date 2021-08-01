#lang scribble/manual

@(require scribble/example
          (for-label (only-in pict pict?)
                     racket/base
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

@subsection{Windows & Dialogs}

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

  Returns a representation of a top-level window.
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

  Returns a representation of a dialog.
}

@subsection{Menus & Menu Items}

@defproc[(popup-menu [menu (is-a?/c view<%>)]) (is-a?/c popup-menu-view<%>)]{
  Returns a representation of a popup menu.  Popup menus are rendered
  using @racket[render-popup-menu].
}

@defproc[(menu-bar [menu (is-a?/c view<%>)]) (is-a?/c view<%>)]{
  Returns a representation of a menu-bar menu.
}

@defproc[(menu [label (maybe-obs/c (or/c #f gui:label-string?))]
               [item (is-a?/c view<%>)] ...) (is-a?/c view<%>)]{

  Returns a representation of a menu with @racket[item]s as children.
}

@defproc[(menu-item [label (maybe-obs/c (or/c #f gui:label-string?))]
                    [action (-> any) void]) (is-a?/c view<%>)]{

  Returns a representation of a menu item that calls @racket[action]
  when clicked.
}

@defproc[(menu-item-separator) (is-a?/c view<%>)]{
  Returns a representation of a menu item separator.
}

@subsection{Containers}

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

  Returns a representation of a panel that lays out its children
  horizontally.
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

  Returns a representation of a panel that lays out its children
  vertically.
}

@defproc[(if-view [cond-value (maybe-obs/c any/c)]
                  [then-view (is-a?/c view<%>)]
                  [else-view (is-a?/c view<%>)]) (is-a?/c view<%>)]{

  Returns a repersentation of a panel that renders @racket[then-view]
  when the current-value of @racket[cond-value] is truthy and
  @racket[else-view] otherwise.
}

@defform[#:literals (else)
         (cond-view
          [cond-e view-e] ...+
          [else view-e])
         #:contracts ([cond-e (maybe-obs/c any/c)]
                      [view-e (is-a?/c view<%>)])]{

  Returns a representation of a panel that renders the first
  @racket[view-e] for which the associated @racket[cond-e]'s current
  value is truthy.
}

@defproc[(list-view [entries (maybe-obs/c list?)]
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

  Returns a representation of a panel that renders the
  @racket[entries] by passing each one as a @tech{derived observable}
  to @racket[make-view].  Each entry must have a unique
  @racket[#:key].
}

@subsection{Canvases & Snips}

@defproc[(canvas [data (maybe-obs/c any/c)]
                 [draw (-> (is-a?/c gui:dc<%>) any/c any)]
                 [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:style style (listof (or/c 'border 'control-border 'combo
                                              'vscroll 'hscroll 'resize-corner
                                              'gl 'no-autoclear 'transparent
                                              'no-focus 'deleted)) null]
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

  Returns a representation of a canvas that is redrawn using
  @racket[draw] whenever @racket[data] changes.
}

@defproc[(pict-canvas [data (maybe-obs/c any/c)]
                      [make-pict (-> any/c pict?)]
                      [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                      [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                      [#:style style (listof (or/c 'border 'control-border 'combo
                                                   'vscroll 'hscroll 'resize-corner
                                                   'gl 'no-autoclear 'transparent
                                                   'no-focus 'deleted)) null]
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

  Returns a representation of a canvas that is redrawn using the
  result of @racket[make-pict] whenever @racket[data] changes.
}

@defproc[(snip [data (maybe-obs/c any/c)]
               [make-snip (-> any/c
                              gui:dimension-integer?
                              gui:dimension-integer?
                              (is-a?/c gui:snip%))]
               [update-snip (-> (is-a?/c gui:snip%) any/c any) void]
               [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
               [#:enabled? enabled? (maybe-obs/c boolean?) #t]
               [#:style style (listof (or/c 'no-border 'control-border 'combo
                                            'resize-corner 'no-focus 'deleted
                                            'transparent)) null]
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

  Returns the representation of an editor that holds a snip generated
  via @racket[make-snip].  The snip may be updated whenever
  @racket[data] changes via @racket[update-snip].
}

@subsection{Widgets}

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

  Returns a representation of a button that calls @racket[action] when
  clicked.
}

@defproc[(checkbox [action (-> boolean? any)]
                   [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                   [#:checked? checked? (maybe-obs/c boolean?) #f]
                   [#:enabled? enabled? (maybe-obs/c boolean?) #f]) (is-a?/c view<%>)]{
  Returns a representation of a checkbox that calls @racket[action]
  when toggled.
}

@defproc[(choice [choices (maybe-obs/c (listof gui:label-string?))]
                 [action (-> (or/c #f gui:label-string?) any)]
                 [#:selection selection (maybe-obs/c exact-nonnegative-integer?) 0]
                 [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                 [#:style style (listof (or/c 'horizontal-label 'vertical-label 'deleted)) null]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:min-size min-size
                             (maybe-obs/c
                              (list/c
                               (or/c boolean? gui:dimension-integer?)
                               (or/c boolean? gui:dimension-integer?)))
                             '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c (list/c boolean? boolean?))
                            '(#t #t)]) (is-a?/c view<%>)]{

  Returns a representation of a choice widget that calls
  @racket[action] whenever the current selection changes.
}

@defproc[(image [path (maybe-obs/c path-string?)]
                [#:size size
                        (maybe-obs/c
                         (list/c (or/c #f gui:dimension-integer?)
                                 (or/c #f gui:dimension-integer?)))
                        '(#f #f)]
                [#:mode mode (maybe-obs/c (or/c 'fit 'fill)) 'fit]) (is-a?/c view<%>)]{

  Returns a representation of an image.

  The @racket[#:mode] argument controls how the image stretches to
  fill its container.  If the mode is @racket['fit], then the image
  will preserve its aspect ratio, otherwise it will stretch to fill
  the container.
}

@defproc[(input [value (maybe-obs/c string?)]
                [action (-> (or/c 'input 'return) string? any) void]
                [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                [#:background-color background-color (maybe-obs/c (or/c #f (is-a?/c gui:color%))) #f]
                [#:style style (listof (or/c 'single 'multiple 'hscroll 'password
                                             'vertical-label 'horizontal-label
                                             'deleted)) '(single)]
                [#:font font (is-a?/c gui:font%) gui:normal-control-font]
                [#:keymap keymap (is-a?/c gui:keymap%) (new gui:keymap%)]
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
  Returns a representation of a text field that calls @racket[action]
  on change.  The first argument to the @racket[action] is the type of
  event that caused the input to change and the second is the contents
  of the text field.
}

@defproc[(progress [value (maybe-obs/c gui:position-integer?)]
                   [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                   [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                   [#:style style (listof (or/c 'horizontal 'vertical 'plain
                                                'vertical-label 'horizontal-label
                                                'deleted)) '(horizontal)]
                   [#:min-value min-value gui:position-integer? 0]
                   [#:max-value max-value gui:position-integer? 100]
                   [#:min-size min-size
                               (maybe-obs/c
                                (list/c
                                 (or/c boolean? gui:dimension-integer?)
                                 (or/c boolean? gui:dimension-integer?)))
                               '(#f #f)]
                   [#:stretch stretch
                              (maybe-obs/c (list/c boolean? boolean?))
                              (list (memq 'horizontal style)
                                    (memq 'vertical   style))]) (is-a?/c view<%>)]{
  Returns a representation of a progress bar.
}

@defproc[(slider [value (maybe-obs/c gui:position-integer?)]
                 [action (-> gui:position-integer? any)]
                 [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:style style (listof (or/c 'horizontal 'vertical 'plain
                                              'vertical-label 'horizontal-label
                                              'deleted)) '(horizontal)]
                 [#:min-value min-value gui:position-integer? 0]
                 [#:max-value max-value gui:position-integer? 100]
                 [#:min-size min-size
                             (maybe-obs/c
                              (list/c
                               (or/c boolean? gui:dimension-integer?)
                               (or/c boolean? gui:dimension-integer?)))
                             '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c (list/c boolean? boolean?))
                            (list (memq 'horizontal style)
                                  (memq 'vertical   style))]) (is-a?/c view<%>)]{
  Returns a representation of a slider that calls the @racket[action] on change.
}

@defproc[(spacer) (is-a?/c view<%>)]{
  Returns a representation of a spacer.  Spacers extend to fill the
  space of their parents.
}

@defproc[(table [columns (listof gui:label-string?)]
                [entries (maybe-obs/c vector?)]
                [action (-> (or/c 'select 'dclick 'column)
                            vector?
                            (or/c #f
                                  exact-nonnegative-integer?
                                  (listof exact-nonnegative-integer?))
                            any)]
                [#:entry->row entry->row (-> any/c vector?) values]
                [#:selection selection
                             (maybe-obs/c
                              (or/c #f
                                    exact-nonnegative-integer?
                                    (listof exact-nonnegative-integer?)))
                             #f]
                [#:label label (maybe-obs/c (or/c #f gui:label-string?)) #f]
                [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                [#:style style
                         (listof (or/c 'single 'multiple 'extended
                                       'vertical-label 'horizontal-label
                                       'variable-columns 'column-headers
                                       'clickable-headers 'reorderable-headers
                                       'deleted))
                         '(single columnn-headers clickable-headers reorderable-headers)]
                [#:font font (is-a?/c gui:font%) gui:view-control-font]
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

  Returns a representation of a table that calls @racket[action] when
  the selection changes or when one of its columns is clicked (if the
  @racket['clickable-headers] style is set).  The action is called
  with the type of event that occurred, the set of entries at the time
  of the event and the current selection, if any.  The current
  selection can either be a single index in the set of entries or a
  list of indices in the case of a @racket['multiple] selection table.

  The @racket[#:entry->row] argument converts each row in the input
  data for display in the table.
}

@defproc[(text [s (maybe-obs/c gui:label-string?)]) (is-a?/c view<%>)]{
  Returns a representation of a textual label.
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

@deftech{Derived observables} are @tech{observables} whose values
depend on other observables.  Derived observables cannot be updated
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
  Returns a new @tech{derived observable} whose value changes every
  time @racket[o]'s value does.  The values held by the new observable
  are mapped via @racket[f].
}

@defproc[(obs-combine [f (-> any/c ...+ any/c)]
                      [o obs?] ...+) obs?]{
  Returns a new @tech{derived observable} whose value changes every
  time one of the @racket[o]s change.  The values held by the new
  observable are the values of the @racket[o]s combined via
  @racket[f].
}

@defproc[(obs-debouce [o obs?]
                      [#:duration duration-ms exact-nonnegative-integer? 200]) obs?]{
  Returns a new @tech{derived observable} based on @racket[o], whose
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

@defproc[(λ<~ [o obs?]
              [f (-> any/c any/c)]) (-> any/c)]{
  Returns a function that updates @racket[o] using @racket[f] when
  applied.
}
