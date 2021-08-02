#lang scribble/manual

@(require (for-label racket/base
                     racket/class
                     (prefix-in gui: racket/gui)
                     racket/gui/easy
                     racket/gui/easy/operator))

@title{Custom Views}

@(define canvas-list-link
  (link "https://github.com/massung/racket-canvas-list/" (tt "canvas-list<%>")))

You can create your own views by implementing the @racket[view<%>]
interface.

As an example, let's wrap Jeffrey Massung's @|canvas-list-link|.  I find
it helps to work backwards from the API you'd like to end up with.  In
this case, that would be:

@racketblock[
(canvas-list
 |@entries|
 (λ (item state dc w h)
   (draw-item ...))
 (λ (item)
   (printf "double-clicked ~s~n" item)))
]

A @racketid[canvas-list] should take an observable of a list of
entries, a function that knows how to draw each entry to a
@racket[gui:dc<%>] and a callback for when the user double-clicks an
entry.  The @racketid[canvas-list] function should probably then look
something like this:

@racketblock[
  (define (canvas-list |@entries| draw [action void])
    (new canvas-list-view%
         [|@entries| |@entries|]
         [draw draw]
         [action action]))
]

Next, we can define a skeleton implementation of
@racketid[canvas-list-view%]:

@racketblock[
  (define canvas-list-view%
    (class* object% (view<%>)
      (init |@entries| draw action)
      (super-new)

      (define/public (dependencies)
        (error 'create "not implemented"))

      (define/public (create parent)
        (error 'create "not implemented"))

      (define/public (update v what val)
        (error 'update "not implemented"))

      (define/public (destroy v)
        (error 'destroy "not implemented"))))
]

Views must communicate what @tech{observables} they depend on to their
parents.  In our case, that's straightforward:

@racketblock[
  (define canvas-list-view%
    (class* object% (view<%>)
      ...

      (define/public (dependencies)
        (list |@entries|))

      ...))
]

When a view is rendered, its parent is in charge of calling its
@method[view<%> create] method.  The create method must instantiate a
GUI object, associate it the passed-in @racketid[parent], perform any
initialization steps and then return it.  In our case:

@racketblock[
  (define canvas-list-view%
    (class* object% (view<%>)
      ...

      (define/public (create parent)
        (new canvas-list%
             [parent parent]
             [items (obs-peek |@entries|)]
             [paint-item-callback (λ (self entry state dc w h)
                                    (draw entry state dc w h))]
             [action-callback (λ (self item event)
                                (action item))]))

      ...))
]

When the @tech{observables} the view depends on change, its parent
will call its @method[view<%> update] method with the GUI object that
the view returned from its @method[view<%> create] method, the
observable that changed and the observable's value when it changed.
The view is then in charge of modifying its GUI object appropriately.

@racketblock[
  (define canvas-list-view%
    (class* object% (view<%>)
      ...

      (define/public (update v what val)
        (case/dep what
          [|@entries| (send v set-items val)]))

      ...))
]

Finally, when a view is no longer visible, its @method[view<%>
destroy] method is called to dispose of the GUI object and perform any
teardown actions.  In our case, there's nothing to do.  We can let
garbage collection take care of destroying the @racketid[canvas-list%]
object:

@racketblock[
  (define canvas-list-view%
    (class* object% (view<%>)
      ...

    (define/public (destroy v)
      (void))))
]

When the view becomes visible again, its @method[view<%> create]
method is called again and the whole cycle repeats itself.

That's all there is to it.  See the "hn.rkt" example for a program
that uses a custom view.
