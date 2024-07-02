#lang scribble/manual

@(require scribble/example
          (for-label (prefix-in mrlib: mrlib/snip-canvas)
                     (only-in pict pict?)
                     racket/base
                     racket/contract
                     racket/class
                     racket/gui/easy
                     racket/gui/easy/contract
                     racket/gui/easy/operator
                     (prefix-in gui: racket/gui)))

@title{Reference}
@defmodule[racket/gui/easy]

@section{Renderers}

@deftech{Renderers} convert view definitions to GUI elements.

@defproc[(renderer? [v any/c]) boolean?]{
  Returns @racket[#t] if @racket[v] is a @tech{renderer}.
}

@defproc[(embed [parent (is-a?/c gui:area<%>)]
                [view (is-a?/c view<%>)]) renderer?]{
  Renders the view hierarchy represented by @racket[view] as a child
  of @racket[parent].

  Use this function when you need to embed one or more
  @racket[view<%>]s within an existing @racketmodname[racket/gui]
  application.  Otherwise, use @racket[render].
}

@defproc[(render [view (is-a?/c window-view<%>)]
                 [parent (or/c #f renderer?) #f]) renderer?]{
  Renders the view hierarchy represented by @racket[view].

  When a @racket[parent] renderer is provided, renders the view as a
  child of the root view of @racket[parent].  This is useful when you
  need to render a modal dialog on top of an existing window.
}

@defproc[(render-popup-menu [parent renderer?]
                            [view (is-a?/c popup-menu-view<%>)]
                            [x gui:position-integer?]
                            [y gui:position-integer?]) void?]{

  Renders the popup menu represented by @racket[view] as a child of
  @racket[parent].
}

@defproc[(renderer-root [r renderer?]) any/c]{
  Returns the root widget of @racket[r].  This function is handy when
  you need to @racket[embed] a @racket[gui:top-level-window<%>].  The
  @racket[embed] function won't show the embedded window, so you'll
  need to get it and send it a @tt{show} message.
}

@defproc[(renderer-destroy [r renderer?]) void?]{
  Destroys the render tree managed by @racket[r].
}


@section{Views}

@subsection[#:tag "windows&dialogs"]{Windows & Dialogs}

@defproc[(window [#:title title (maybe-obs/c string?) "Untitled"]
                 [#:size size (maybe-obs/c size/c) '(#f #f)]
                 [#:alignment alignment (maybe-obs/c alignment/c) '(center top)]
                 [#:position position (maybe-obs/c position/c) 'center]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                 [#:style style
                          (listof (or/c 'no-resize-border 'no-caption
                                        'no-system-menu 'hide-menu-bar
                                        'toolbar-button 'float 'metal
                                        'fullscreen-button 'fullscreen-aux))
                          null]
                 [#:mixin mix (make-mixin-contract gui:frame%) values]
                 [child (is-a?/c view<%>)] ...+) (is-a?/c window-view<%>)]{

  Returns a representation of a top-level window.
}

@defproc[(dialog [#:title title (maybe-obs/c string?) "Untitled"]
                 [#:size size (maybe-obs/c size/c) '(#f #f)]
                 [#:alignment alignment (maybe-obs/c alignment/c) '(center top)]
                 [#:position position (maybe-obs/c position/c) 'center]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                 [#:style style
                          (listof (or/c 'no-caption 'no-sheet 'resize-border 'close-button))
                          '(close-button)]
                 [#:mixin mix (make-mixin-contract gui:dialog%) values]
                 [child (is-a?/c view<%>)] ...+) (is-a?/c window-view<%>)]{

  Returns a representation of a dialog.
}

@subsection{Menus & Menu Items}

@defproc[(popup-menu [menu-or-item (is-a?/c view<%>)] ...) (is-a?/c popup-menu-view<%>)]{
  Returns a representation of a popup menu.  Popup menus are rendered
  using @racket[render-popup-menu].

  @racketblock[
    (popup-menu
     (menu
      "File"
      (menu-item "Open...")
      (menu-item "Save"))
     (menu-item-separator)
     (menu-item "Quit"))
  ]
}

@defproc[(menu-bar [#:enabled? enabled? (maybe-obs/c any/c) #t]
                   [menu-or-item (is-a?/c view<%>)] ...) (is-a?/c view<%>)]{
  Returns a representation of a menu-bar menu.

  @racketblock[
    (menu-bar
     (menu
      "File"
      (menu-item "Open...")
      (menu-item "Save")
      (menu-item-separator)
      (menu-item "Quit"))
     (menu
      "Help"
      (menu-item "Getting Started")))
  ]

  @history[
    #:changed "0.15" @elem{The @racket[#:enabled?] argument.}
  ]
}

@defproc[(menu [label (maybe-obs/c maybe-label/c)]
               [#:enabled? enabled? (maybe-obs/c any/c) #t]
               [#:help help-text (maybe-obs/c (or/c #f string?)) #f]
               [item (is-a?/c view<%>)] ...) (is-a?/c view<%>)]{

  Returns a representation of a menu with @racket[item]s as children.

  @history[
    #:changed "0.15" @elem{
      The @racket[#:enabled?] and @racket[#:help] arguments.
    }
  ]
}

@defproc[(menu-item [label (maybe-obs/c maybe-label/c)]
                    [action (-> any) void]
                    [#:enabled? enabled? (maybe-obs/c any/c) #t]
                    [#:help help-text (maybe-obs/c (or/c #f string?)) #f]
                    [#:shortcut shortcut (maybe-obs/c (or/c #f (*list/c
                                                                (or/c 'alt 'cmd 'meta 'ctl 'shift 'option)
                                                                (or/c 'alt 'cmd 'meta 'ctl 'shift 'option)
                                                                (or/c char? symbol?)))) #f]) (is-a?/c view<%>)]{

  Returns a representation of a menu item that calls @racket[action]
  when clicked.

  @history[
    #:changed "0.15" @elem{
      The @racket[#:enabled?], @racket[#:help] and @racket[#:shortcut]
      arguments.
    }
  ]
}

@defproc[(checkable-menu-item [label (maybe-obs/c maybe-label/c)]
                              [action (-> boolean? any) void]
                              [#:checked? checked? (maybe-obs/c any/c) #f]
                              [#:enabled? enabled? (maybe-obs/c any/c) #t]
                              [#:help help-text (maybe-obs/c (or/c #f string?)) #f]
                              [#:shortcut shortcut (maybe-obs/c (or/c #f (*list/c
                                                                          (or/c 'alt 'cmd 'meta 'ctl 'shift 'option)
                                                                          (or/c 'alt 'cmd 'meta 'ctl 'shift 'option)
                                                                          (or/c char? symbol?)))) #f]) (is-a?/c view<%>)]{

  Returns a representation of a menu item with a checkbox. The
  @racket[action] callback is called with the current checked state
  when the menu item is clicked. Use @racket[#:checked?] to set or
  update the checkbox programmatically.

  @history[#:added "0.18"]
}

@defproc[(menu-item-separator) (is-a?/c view<%>)]{
  Returns a representation of a menu item separator.
}

@subsection[#:tag "containers"]{Containers}

@defproc[(hpanel [#:alignment alignment (maybe-obs/c alignment/c) '(left center)]
                 [#:style style
                          (listof (or/c 'border 'deleted
                                        'hscroll 'auto-hscroll 'hide-hscroll
                                        'vscroll 'auto-vscroll 'hide-vscroll))
                          null]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:spacing spacing (maybe-obs/c spacing/c) 0]
                 [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                 [#:mixin mix (make-mixin-contract gui:panel%) values]
                 [child (is-a?/c view<%>)] ...+) (is-a?/c view<%>)]{

  Returns a representation of a panel that lays out its children
  horizontally.

  @history[#:changed "0.13" @elem{Added the @racket[#:mixin] argument.}]
}

@defproc[(vpanel [#:alignment alignment (maybe-obs/c alignment/c) '(center top)]
                 [#:style style
                          (listof (or/c 'border 'deleted
                                        'hscroll 'auto-hscroll 'hide-hscroll
                                        'vscroll 'auto-vscroll 'hide-vscroll))
                          null]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:spacing spacing (maybe-obs/c spacing/c) 0]
                 [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                 [#:mixin mix (make-mixin-contract gui:panel%) values]
                 [child (is-a?/c view<%>)] ...+) (is-a?/c view<%>)]{

  Returns a representation of a panel that lays out its children
  vertically.

  @history[#:changed "0.13" @elem{Added the @racket[#:mixin] argument.}]
}

@defproc[(group [label (maybe-obs/c gui:label-string?)]
                [#:alignment alignment (maybe-obs/c alignment/c) '(center top)]
                [#:style style (listof (or/c 'deleted)) null]
                [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                [#:spacing spacing (maybe-obs/c spacing/c) 0]
                [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                [#:mixin mix (make-mixin-contract gui:panel%) values]
                [child (is-a?/c view<%>)] ...+) (is-a?/c view<%>)]{

  Returns a representation of a labeled vertical panel.

  @history[#:changed "0.13" @elem{Added the @racket[#:mixin] argument.}]
}

@defproc[(tabs [choices (maybe-obs/c (listof any/c))]
               [action (-> (or/c 'new 'close 'reorder 'select)
                           (listof any/c)
                           (or/c #f any/c)
                           any)]
               [child (is-a?/c view<%>)] ...
               [#:choice->label choice->label (-> any/c gui:label-string?) values]
               [#:choice=? choice=? (-> any/c any/c boolean?) equal?]
               [#:selection selection (maybe-obs/c (or/c #f any/c)) #f]
               [#:alignment alignment (maybe-obs/c alignment/c) '(left center)]
               [#:enabled? enabled? (maybe-obs/c boolean?) #t]
               [#:style style (listof (or/c 'no-border
                                            'can-reorder 'can-close 'new-button
                                            'flat-portable 'deleted)) null]
               [#:spacing spacing (maybe-obs/c spacing/c) 0]
               [#:margin margin (maybe-obs/c margin/c) 0]
               [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
               [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]) (is-a?/c view<%>)]{

  Returns a representation of a tab panel.

  The @racket[#:choice->label] argument controls how each choice is
  displayed and the @racket[#:choice=?] argument controls how the
  current @racket[#:selection] is compared against the list of choices
  to determine the currently selected tab.

  On user interaction, @racket[action] is called with a symbol
  representing the event, the set of choices at the moment the action
  occurred and the current selection.  The selection may be adjusted
  depending on the event (eg. when the current tab is closed, the
  selection changes to an adjacent tab).  When tabs are reordered, the
  choices provided to the action represent the new tab order.

  See @filepath{examples/tabs.rkt} for an example.

  @history[
    #:changed "0.3" @elem{Added the @racket[#:choice=?] argument.}
    #:changed "0.3" @elem{The @racket[selection] is now a value in the set of choices instead of an index.}
  ]
}

@defform[(if-view cond-e then-e else-e)
         #:contracts ([cond-e (maybe-obs/c any/c)]
                      [then-e (is-a?/c view<%>)]
                      [else-e (is-a?/c view<%>)])]{

  Returns a representation of a panel that renders @racket[then-e]
  when the current-value of @racket[cond-e] is truthy and
  @racket[else-e] otherwise.

  @history[
    #:changed "0.4" @elem{The @racket[if-view] form was converted from a procedure into a syntactic form.}
  ]
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

@defform[#:literals (else)
         (case-view e
          [(case-lit ...+) view-e] ...+
          [else view-e])
         #:contracts ([e (obs/c any/c)]
                      [view-e (is-a?/c view<%>)])]{

  Returns a representation of a panel that renders the first
  @racket[view-e] where one of the @racket[case-lit]s is
  @racket[equal?] to @racket[e]'s current value.
}

@defproc[(list-view [entries (maybe-obs/c list?)]
                    [make-view (-> any/c any/c (is-a?/c view<%>))]
                    [#:key key (-> any/c any/c) values]
                    [#:alignment alignment (maybe-obs/c alignment/c) '(center top)]
                    [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                    [#:style style
                             (listof (or/c 'horizontal 'vertical 'border 'deleted
                                           'hscroll 'auto-hscroll 'hide-hscroll
                                           'vscroll 'auto-vscroll 'hide-vscroll))
                             '(vertical auto-vscroll)]
                    [#:spacing spacing (maybe-obs/c spacing/c) 0]
                    [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                    [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                    [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                    [#:mixin mix (make-mixin-contract gui:panel%) values]) (is-a?/c view<%>)]{

  Returns a representation of a panel that renders the @racket[entries]
  by passing each one as a @tech{derived observable} to
  @racket[make-view]. The @racket[make-view] procedure is called with
  the key and the derived observable of each entry. The @racket[#:key]
  procedure must return a unique value for each entry in the list, as
  compared using @racket[equal?].

  See @filepath{examples/list.rkt} for an example.
}

@defproc[(observable-view [data (obs/c any/c)]
                          [make-view (-> any/c (is-a?/c view<%>)) values]
                          [#:equal? equal?-proc (-> any/c any/c boolean?) equal?]) (is-a?/c view<%>)]{

  Returns a representation of a pane whose content is the result of
  applying @racket[make-view] to the value of @racket[data].  The
  content of the pane changes every time @racket[data] changes and its
  current value is not equal (according to @racket[equal?-proc]) to
  the previous value.  The pane automatically adjusts its area
  properties when its child's area properties change to match.

  @history[#:added "0.9"]
}

@subsection{Canvases & Snips}

@defproc[(canvas [data (maybe-obs/c any/c)]
                 [draw (-> (is-a?/c gui:dc<%>) any/c any)]
                 [#:label label (maybe-obs/c maybe-label/c) #f]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:style style (listof (or/c 'border 'control-border 'combo
                                              'vscroll 'hscroll 'resize-corner
                                              'gl 'no-autoclear 'transparent
                                              'no-focus 'deleted)) null]
                 [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                 [#:mixin mix (make-mixin-contract gui:canvas%) values]) (is-a?/c view<%>)]{

  Returns a representation of a canvas that is redrawn using
  @racket[draw] whenever @racket[data] changes.
}

@defproc[(pict-canvas [data (maybe-obs/c any/c)]
                      [make-pict (-> any/c pict?)]
                      [#:label label (maybe-obs/c maybe-label/c) #f]
                      [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                      [#:style style (listof (or/c 'border 'control-border 'combo
                                                   'vscroll 'hscroll 'resize-corner
                                                   'gl 'no-autoclear 'transparent
                                                   'no-focus 'deleted)) null]
                      [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                      [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                      [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                      [#:mixin mix (make-mixin-contract gui:canvas%) values]) (is-a?/c view<%>)]{

  Returns a representation of a canvas that is redrawn using the
  result of @racket[make-pict] whenever @racket[data] changes.
}

@defproc[(snip-canvas [data (maybe-obs/c any/c)]
                      [make-snip (-> any/c gui:dimension-integer? gui:dimension-integer? (is-a?/c gui:snip%))]
                      [#:label label (maybe-obs/c maybe-label/c) #f]
                      [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                      [#:style style (listof (or/c 'border 'control-border 'combo
                                                   'vscroll 'hscroll 'resize-corner
                                                   'gl 'no-autoclear 'transparent
                                                   'no-focus 'deleted)) null]
                      [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                      [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                      [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                      [#:mixin mix (make-mixin-contract gui:canvas%) values]) (is-a?/c view<%>)]{

  Returns a representation of a canvas that is redrawn using the
  result of @racket[make-snip] whenever @racket[data] changes.  The
  snip is converted to a bitmap before being drawn to the canvas so it
  is non-interactive.  Use this view when you want to efficiently
  update plots.  For interactive snips, see @racket[snip].
}

@defproc[(snip [data (maybe-obs/c any/c)]
               [make-snip (-> any/c
                              gui:dimension-integer?
                              gui:dimension-integer?
                              (is-a?/c gui:snip%))]
               [update-snip (-> (is-a?/c gui:snip%) any/c any) void]
               [#:label label (maybe-obs/c maybe-label/c) #f]
               [#:enabled? enabled? (maybe-obs/c boolean?) #t]
               [#:style style (listof (or/c 'no-border 'control-border 'combo
                                            'resize-corner 'no-focus 'deleted
                                            'transparent)) null]
               [#:margin margin (maybe-obs/c margin/c) '(0 0)]
               [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
               [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
               [#:mixin mix (make-mixin-contract mrlib:snip-canvas%) values]) (is-a?/c view<%>)]{

  Returns the representation of an editor that holds a snip generated
  via @racket[make-snip].  The snip may be updated whenever
  @racket[data] changes via @racket[update-snip].
}

@subsection{Controls}

@defproc[(button [label (maybe-obs/c
                         (or/c gui:label-string?
                               (is-a?/c gui:bitmap%)
                               (list/c (is-a?/c gui:bitmap%)
                                       gui:label-string?
                                       (or/c 'left 'top 'right 'bottom))))]
                 [action (-> any)]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:style style (listof (or/c 'border 'multi-line 'deleted)) null]
                 [#:font font (is-a?/c gui:font%) gui:normal-control-font]
                 [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c stretch/c)
                            '(#t #t)]) (is-a?/c view<%>)]{

  Returns a representation of a button that calls @racket[action] when
  clicked.
}

@defproc[(checkbox [action (-> boolean? any)]
                   [#:label label (maybe-obs/c gui:label-string?) #f]
                   [#:checked? checked? (maybe-obs/c boolean?) #f]
                   [#:enabled? enabled? (maybe-obs/c boolean?) #f]) (is-a?/c view<%>)]{
  Returns a representation of a checkbox that calls @racket[action]
  when toggled.
}

@defproc[(choice [choices (maybe-obs/c (listof any/c))]
                 [action (-> (or/c #f any/c) any)]
                 [#:choice->label choice->label (-> any/c gui:label-string?) values]
                 [#:choice=? choice=? (-> any/c any/c boolean?) equal?]
                 [#:selection selection (maybe-obs/c any/c) #f]
                 [#:label label (maybe-obs/c maybe-label/c) #f]
                 [#:style style (listof (or/c 'horizontal-label 'vertical-label 'deleted)) null]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]) (is-a?/c view<%>)]{

  Returns a representation of a choice widget that calls
  @racket[action] whenever the current selection changes.

  The @racket[#:choice->label] argument controls how each choice is
  displayed and the @racket[#:choice=?] argument controls how the
  current @racket[#:selection] is compared against the list of choices
  to determine the selection index.
}

@defproc[(image [path-or-bitmap (maybe-obs/c (or/c path-string? (is-a?/c gui:bitmap%)))]
                [#:size size (maybe-obs/c size/c) '(#f #f)]
                [#:mode mode (maybe-obs/c (or/c 'fit 'fill)) 'fit]) (is-a?/c view<%>)]{

  Returns a representation of an image.

  The @racket[#:mode] argument controls how the image stretches to
  fill its container.  If the mode is @racket['fit], then the image
  will preserve its aspect ratio, otherwise it will stretch to fill
  the container.

  @history[
    #:changed "0.11.1" @elem{The canvas background is now
     @racket['transparent]. Now passes @racket[#t] to the
     @racket[#:try-@2x?] argument of @racket[gui:read-bitmap].}
    #:changed "0.17" @elem{The first argument may now be a
     @racket[gui:bitmap%].}]
}

@defproc[(input [value (maybe-obs/c any/c)]
                [action (-> (or/c 'input 'return) string? any) void]
                [#:label label (maybe-obs/c maybe-label/c) #f]
                [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                [#:background-color background-color (maybe-obs/c (or/c #f (is-a?/c gui:color%))) #f]
                [#:style style (listof (or/c 'single 'multiple 'hscroll 'password
                                             'vertical-label 'horizontal-label
                                             'deleted)) '(single)]
                [#:font font (is-a?/c gui:font%) gui:normal-control-font]
                [#:keymap keymap (is-a?/c gui:keymap%) (new gui:keymap%)]
                [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                [#:mixin mix (make-mixin-contract gui:text-field%) values]
                [#:value=? value=? (-> any/c any/c boolean?) equal?]
                [#:value->text value->text (-> any/c string?) values]) (is-a?/c view<%>)]{
  Returns a representation of a text field that calls @racket[action]
  on change.  The first argument to the @racket[action] is the type of
  event that caused the input to change and the second is the contents
  of the text field.

  The @racket[#:value=?] argument controls when changes to the input
  data are reflected in the contents of the field.  The contents of
  the input field only change when the new value of the underlying
  observable is not @racket[value=?] to the previous one.  The only
  exception to this is when the textual value (via
  @racket[#:value->text]) of the observable is the empty string, in
  which case the input is cleared regardless of the value of the
  underlying observable.

  The @racket[#:value->text] argument controls how the input values
  are rendered to strings.  If not provided, @racket[value] must be
  either a @racket[string?] or an observable of strings.
}

@defproc[(progress [value (maybe-obs/c gui:position-integer?)]
                   [#:label label (maybe-obs/c maybe-label/c) #f]
                   [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                   [#:style style (listof (or/c 'horizontal 'vertical 'plain
                                                'vertical-label 'horizontal-label
                                                'deleted)) '(horizontal)]
                   [#:range range (maybe-obs/c gui:positive-dimension-integer?) 100]
                   [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                   [#:stretch stretch
                              (maybe-obs/c stretch/c)
                              (list (memq 'horizontal style)
                                    (memq 'vertical   style))]) (is-a?/c view<%>)]{
  Returns a representation of a progress bar.
}

@defproc[(radios [choices (listof any/c)]
                 [action (-> (or/c #f any/c) any)]
                 [#:choice->label choice->label (-> any/c gui:label-string?) values]
                 [#:choice=? choice=? (-> any/c any/c boolean?) equal?]
                 [#:selection selection (maybe-obs/c any/c) #f]
                 [#:label label (maybe-obs/c maybe-label/c) #f]
                 [#:style style (listof (or/c 'horizontal-label 'vertical-label 'deleted)) null]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]) (is-a?/c view<%>)]{

  Returns a representation of a radio box widget that calls
  @racket[action] whenever the current selection changes.

  The @racket[#:choice->label] argument controls how each choice is
  displayed and the @racket[#:choice=?] argument controls how the
  current @racket[#:selection] is compared against the list of choices
  to determine the selection index.

  Unlike @racket[choice], the set of @racket[choices] cannot be changed.
}

@defproc[(slider [value (maybe-obs/c gui:position-integer?)]
                 [action (-> gui:position-integer? any)]
                 [#:label label (maybe-obs/c maybe-label/c) #f]
                 [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                 [#:style style (listof (or/c 'horizontal 'vertical 'plain
                                              'vertical-label 'horizontal-label
                                              'deleted)) '(horizontal)]
                 [#:min-value min-value gui:position-integer? 0]
                 [#:max-value max-value gui:position-integer? 100]
                 [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                 [#:stretch stretch
                            (maybe-obs/c stretch/c)
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
                            any)
                        void]
                [#:entry->row entry->row (-> any/c vector?) values]
                [#:selection selection
                             (maybe-obs/c
                              (or/c #f
                                    exact-nonnegative-integer?
                                    (listof exact-nonnegative-integer?)))
                             #f]
                [#:label label (maybe-obs/c maybe-label/c) #f]
                [#:enabled? enabled? (maybe-obs/c boolean?) #t]
                [#:style style
                         (listof (or/c 'single 'multiple 'extended
                                       'vertical-label 'horizontal-label
                                       'variable-columns 'column-headers
                                       'clickable-headers 'reorderable-headers
                                       'deleted))
                         '(single columnn-headers clickable-headers reorderable-headers)]
                [#:font font (is-a?/c gui:font%) gui:view-control-font]
                [#:margin margin (maybe-obs/c margin/c) '(0 0)]
                [#:min-size min-size (maybe-obs/c size/c) '(#f #f)]
                [#:stretch stretch (maybe-obs/c stretch/c) '(#t #t)]
                [#:column-widths column-widths (maybe-obs/c
                                                (listof
                                                 (or/c (list/c exact-nonnegative-integer?
                                                               gui:dimension-integer?)
                                                       (list/c exact-nonnegative-integer?
                                                               gui:dimension-integer?
                                                               gui:dimension-integer?
                                                               gui:dimension-integer?)))) null]
                [#:mixin mix (make-mixin-contract gui:list-box%) values]) (is-a?/c view<%>)]{

  Returns a representation of a table that calls @racket[action] when
  the selection changes or when one of its columns is clicked (if the
  @racket['clickable-headers] style is set).  The @racket[action]
  callback is called with the type of event that occurred, the set of
  entries at the time of the event and the current selection, if any.
  The current selection can either be a single index in the set of
  entries or a list of indices in the case of a @racket['multiple]
  selection table.

  The @racket[#:entry->row] argument converts each row in the input
  data for display in the table.

  The @racket[#:column-widths] argument controls the widths of the
  columns.  Column lengths can be specified either as a list of the
  column index (starting from 0) and the default width or a list of
  the column index, the column width, the minimum width and the
  maximum width.

  @history[#:changed "0.13" @elem{Added the @racket[#:mixin] argument.}]
}

@defproc[(text [s (maybe-obs/c gui:label-string?)]
               [#:color color (maybe-obs/c (or/c #f string? (is-a?/c gui:color%))) #f]
               [#:font font (is-a?/c gui:font%) gui:normal-control-font]) (is-a?/c view<%>)]{
  Returns a representation of a textual label.
}

@subsection{Combinators}

@defproc[(add-hooks [#:on-create create-proc (-> any) void]
                    [#:on-destroy destroy-proc (-> any) void]
                    [v (is-a?/c view<%>)]) (is-a?/c view<%>)]{
  Returns a proxy of @racket[v] that calls @racket[create-proc] and
  @racket[destroy-proc] when a GUI widget is created and destroyed,
  respectively, from the view.

  @history[#:added "0.14"]
}

@subsection{Interfaces}
@subsubsection{@tt{view<%>}}
@definterface[view<%> ()]{
  A @racket[view<%>] object is a wrapper around a GUI object that knows
  what its data dependecies are and how to respond to their changes.

  A single @racket[view<%>] object may be used to manage multiple GUI
  widgets. Consequently, when implementing custom views, it's best not
  to store any state within the view object itself. Instead, associate
  any internal state with the GUI widgets returned by @racket[create],
  possibly via @racket[context-mixin].

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

@subsubsection{@tt{window-view<%>}}
@definterface[window-view<%> (view<%>)]{
  A @racket[window-view<%>] is like a regular @racket[view<%>] but its
  @racket[create] method has additional constraints placed on it.

  @defmethod[(create [parent (or/c (is-a?/c gui:frame%)
                                   (is-a?/c gui:dialog%)
                                   #f)]) (is-a?/c gui:top-level-window<%>)]{
    Returns a new @racket[gui:top-level-window<%>] belonging to @racket[parent].
  }

  @defmethod[(is-dialog?) boolean?]{
    Returns @racket[#t] if this view is a dialog.
  }
}

@subsubsection{@tt{popup-menu-view<%>}}
@definterface[popup-menu-view<%> (view<%>)]{
  A @racket[popup-menu-view<%>] is like a regular @racket[view<%>] but
  its @racket[create] method has additional constraints placed on it.

  @defmethod[(create [parent #f]) (is-a?/c gui:popup-menu%)]{
    Returns a new @racket[gui:popup-menu%].
  }
}

@subsubsection{@tt{context<%>}}

@; Require the private module to avoid requiring racket/gui/base.
@(define ctxt-ev (make-base-eval '(require racket/class racket/gui/easy/private/view/view)))

@defmixin[context-mixin (view<%>) (context<%>)]{
  Specializes a class to implement the @racket[context<%>] interface.
  Compares keys using @racket[eq?].

  @examples[
    #:eval ctxt-ev
    (define ob (new (context-mixin object%)))
    (send ob set-context 'a 42)
    (send ob get-context 'a)
  ]
}

@definterface[context<%> ()]{
  A @racket[context<%>] object allows the user of an object to
  associate arbitrary values with it.  Many of the @racket[view<%>]s
  implemented by this library wrap their underlying GUI widgets using
  @racket[context-mixin] in order to associate internal state with
  them.

  @defmethod[(set-context [k any/c] [v any/c]) void?]{
    Stores @racket[v] under @racket[k] within the context, overriding
    any existing values.
  }

  @defmethod[(set-context* [k any/c] [v any/c] ... ...) void?]{
    Stores each @racket[v] under each @racket[k] within the context.
  }

  @defmethod[(get-context [k any/c]
                          [default any/c (λ () (error 'get-context "no entry for ~a" k))]) any/c]{

    Returns the value stored under @racket[k] from the context.  If
    there is no value, the result is determined by @racket[default]:

    @itemlist[
      @item{If @racket[default] is a @racket[procedure?], it is called
      with no arguments to produce a result.}
      @item{Otherwise, @racket[default] is returned unchanged.}
    ]
  }

  @defmethod[(get-context! [k any/c]
                           [default any/c]) any/c]{
    Like @racket[get-context], but if there is no value stored under
    @racket[k], the @racket[default] value is computed as in
    @racket[get-context], stored in the context under @racket[k] and
    then returned.
  }

  @defmethod[(remove-context [k any/c]) void?]{
    Removes the value stored under @racket[k] from the context.
  }

  @defmethod[(clear-context) void?]{
    Removes all stored values from the context.
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

@examples[
  #:eval obs-sb
  (define |@strs| (obs-map |@ints| number->string))
  |@strs|
  (eval:error (obs-update! |@strs| add1))
]

@(define impersonated-link
  (tech
   #:key "impersonators"
   #:doc '(lib "scribblings/reference/reference.scrbl")
   "impersonated"))

Internally, every observable has a unique handle and two observables
are @racket[equal?] when their handles are @racket[eq?].  This means
that equality (via @racket[equal?]) is preserved for
@impersonated-link observables, such as those guarded by
@racket[obs/c].

@defproc[(obs? [v any/c]) boolean?]{
  Returns @racket[#t] when @racket[v] is an @tech{observable}.
}

@(define printed-link
  (seclink
   #:doc '(lib "scribblings/reference/reference.scrbl")
   "printing"
   "printed"))

@defproc[(obs [v any/c]
              [#:name name symbol? 'anon]
              [#:derived? derived? boolean? #f]) obs?]{
  Returns a new observable, whose initial value is @racket[v].

  The @racket[#:name] of an observable is visible when the observable
  is @printed-link so using a custom name can come in handy while
  debugging code.

  The @racket[#:derived?] argument controls whether or not the
  observable may be updated.
}

@defproc[(obs-rename [o obs?]
                     [name symbol?]) obs?]{

  Returns an impersonator of @racket[o] whose name is changed to
  @racket[name].
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
  storing the result.  Returns the new value.  If @racket[o] is a
  @tech{derived observable}, raises an @racket[exn:fail:contract?]
  error.
}

@defproc[(obs-set! [o obs?]
                   [v any/c]) void?]{

  Sets the value of @racket[o] to @racket[v]. Equivalent to
  @racket[(void (obs-update! o (λ (old-v) v)))].

  @history[#:added "0.16"]
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

@defproc[(obs-filter-map [o obs?]
                         [p (-> any/c any/c)]
                         [d any/c #f]) obs?]{
  Returns a new @tech{derived observable} that applies @racket[p] to
  every new value of @racket[o]. The derived observable updates when
  the result of applying @racket[p] to the new value of @racket[o]
  is not @racket[#f]. The initial value of the derived observable is
  @racket[(or (p (obs-peek o)) d)].

  @history[#:added "0.11"]
}

@defproc[(obs-filter [o obs?]
                     [p (-> any/c any/c)]
                     [d any/c #f]) obs?]{
  Equivalent to @racket[(obs-filter-map o (λ (v) (and (p v) v)) d)].

  @history[#:added "0.11"]
}

@defproc[(obs-combine [f (-> any/c ...+ any/c)]
                      [o obs?] ...+) obs?]{
  Returns a new @tech{derived observable} whose value changes every
  time one of the @racket[o]s change.  The values held by the new
  observable are the values of the @racket[o]s combined via
  @racket[f].

  This combinator retains a strong reference to each of the last
  values of the respective observables that are being combined until
  they change.
}

@defproc[(obs-debounce [o obs?]
                       [#:duration duration-ms exact-nonnegative-integer? 200]) obs?]{
  Returns a new @tech{derived observable} based on @racket[o], whose
  value changes when there is at least a @racket[duration-ms]
  millisecond pause in changes to @racket[o].
}

@defproc[(obs-throttle [o obs?]
                       [#:duration duration-ms exact-nonnegative-integer? 200]) obs?]{
  Returns a new @tech{derived observable} based on @racket[o], whose
  values change at most once every @racket[duration-ms] milliseconds.
}


@section{View Helpers}

@defform[(case/dep what-expr
          [dep-expr body ...] ...+)
         #:contracts ([what-expr obs?]
                      [dep-expr  obs?])]{

  Executes the body of the first clause @racket[body] whose
  @racket[dep-expr] is @racket[equal?] to @racket[what-expr].  Logs
  the @racket[dep-expr] that matched to the @racket['gui-easy] topic.
  Use this form to implement @method[view<%> update] methods.
}


@section{Observable Operators}
@defmodule[racket/gui/easy/operator]

@defform[(define/obs name init-expr)]{
  Binds @racket[name] to an observable whose initial value is
  @racket[init-expr] and whose name is @racket['name].  If
  @racket[init-expr] is already an @tech{observable}, then it is
  locally renamed to @racket['name] then bound to @racket[name].
}

@defproc[(|@| [v any/c]) obs?]{
  Converts @racket[v] into an @tech{observable}.  If @racket[v] is
  already an observable, it is returned unchanged.
}

@defproc[(:= [o obs?]
             [v any/c]) any/c]{
  Changes the value of @racket[o] to @racket[v].
}

@defproc[((λ:= [o obs?]
               [f (-> any/c any/c) values]) [v any/c]) any/c]{
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

@defproc[(~#> [o obs?]
              [p (-> any/c any/c)]
              [d any/c #f]) obs?]{
  An alias for @racket[obs-filter].

  @history[#:added "0.11"]
}

@defproc[(λ<~ [o obs?]
              [f (-> any/c any/c)]) (-> any/c)]{
  Returns a function that updates @racket[o] using @racket[f] when
  applied.
}


@section{Contracts}
@defmodule[racket/gui/easy/contract]

@defthing[alignment/c (list/c (or/c 'left 'center 'right)
                              (or/c 'top  'center 'bottom))]{

  The contract for container child alignment.  Represents the
  horizontal and vertical alignment, respectively.
}

@defthing[margin/c (list/c gui:spacing-integer?
                           gui:spacing-integer?)]{

  The contract for margins.  Represents the horizontal and vertical
  margin, respectively.
}

@defthing[maybe-label/c (or/c #f gui:label-string?)]{
  The contract for optional labels.
}

@defthing[position/c (or/c 'center (list/c gui:position-integer?
                                           gui:position-integer?))]{
  The contract for positions.  The first places @racket[window]s and
  @racket[dialog]s in the center of the screen.
}

@defthing[size/c (list/c (or/c #f gui:dimension-integer?)
                         (or/c #f gui:dimension-integer?))]{

  The contract for sizes.  Represents the width and height,
  respectively.  If either value is false, the view is allowed to
  stretch in that direction.
}

@defthing[spacing/c gui:spacing-integer?]{
  The contract for spacing.
}

@defthing[stretch/c (list/c boolean? boolean?)]{
  The contract for stretch values.  Represents whether or not a view
  can stretch horizontally and vertically, respectively.
}

@defproc[(obs/c [c contract?]) contract?]{
  Returns a contract that accepts an @racket[obs?] whose values
  conform to @racket[c].  Checks the initial value of the observable
  as well as all subsequent updated values.
}

@defproc[(maybe-obs/c [c contract?]) contract?]{
  A shorthand for @racket[(or/c c (obs/c c))].
}
