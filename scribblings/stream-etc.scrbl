#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    racket/contract
                    racket/stream
                    stream-etc]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require racket/contract
               racket/function
               racket/list
               racket/stream
               stream-etc)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Stream Miscellanea}
@author{Cameron Moy}

@defmodule[stream-etc]

@margin-note{
This library is experimental;
compatibility may not be maintained.
}

@section{Operations}

@defproc[(stream-group-by [stream stream?]
                          [key (-> any/c any)]
                          [same? (-> any/c any/c boolean?) equal?])
         (listof list?)]{
  Groups the given stream into equivalence classes,
  with equivalence being determined by same?.
  Within each equivalence class,
  @racket[stream-group-by] preserves the ordering of the original list.
  Equivalence classes themselves are in order of first appearance in the input.

  @examples[#:eval evaluator
    (define (parity=? x y)
      (= (modulo x 2) (modulo y 2)))
    (stream-group-by '("1" "2" "3" "4") string->number parity=?)]
}

@defproc[(stream-map* [f procedure?] [stream stream?] ...) stream?]{
  Returns a stream this is the result of applying @racket[f]
  to the elements of the streams, just like @racket[map].
  The only difference from @racket[stream-map] is this one is
  variable arity.

  @examples[#:eval evaluator
    (stream->list (stream-map* + '(1 2 3) '(4 5 6)))]
}

@defproc[(stream-sum [stream (stream/c number?)])
         number?]{
  Returns the sum of numbers in a stream.

  @examples[#:eval evaluator
    (stream-sum '())
    (stream-sum '(1 2 3))]
}

@defproc[(stream-member? [stream stream?]
                         [elem any/c])
         boolean?]{
  Returns if @racket[elem] is in the given stream.

  @examples[#:eval evaluator
    (stream-member? '(1 2 3) 42)
    (stream-member? '(1 2 3) 2)]
}

@section{Threading}

Forms that provide functionality akin to that of the
@other-doc['(lib "scribblings/threading.scrbl")] library,
only specialized to stream predicates.

@defform[
  (stream~> stream-expr clause ...)
  #:grammar
  [(clause (code:line clause-expr)
           (code:line #:take clause-expr)
           (code:line #:repeat))
   (clause-expr (code:line (fn-expr pre-expr ... hole-marker post-expr ...))
                (code:line bare-expr))
   (hole-marker _)]]{
  Returns whether the given stream satisfies the property
  described by the clauses.
  @itemize[
  @item{
    An expression without a keyword should evaluate to a predicate
    that will be given the current prefix of the stream
    (starting off empty).
    If it's ever @racket[#false],
    the entire @racket[stream~>] will be @racket[#false].}
  @item{
    A @racket[#:take] clause expects a function
    that returns some prefix of the current stream
    and sets it as the current prefix of interest.
    The suffix then becomes the current stream.
    If the current stream is empty,
    the entire @racket[stream~>] will become @racket[#true].
  }
  @item{
    A @racket[#:repeat] clause starts processing over
    from the first clause using the current stream.
  }]
  A clause expression can be either a normal expression
  or an application with a hole.
  If it's an application with a hole,
  it is transformed into a one-argument procedure
  where the argument is located at the hole.

  @examples[#:eval evaluator
    (define (ok? s)
      (stream~> s
                #:take (takef _ odd?)
                (negate empty?)
                #:take (takef _ zero?)
                #:repeat))
    (ok? '(1 3 0 0 1 7))
    (ok? '(1 0 1 0 1 0))
    (ok? '(5 8))]
}

@deftogether[(@defform*[((lambda-stream~> clause ...))]
              @defform[(λ-stream~> clause ...)])]{
  Equivalent to @racket[(λ (st) (stream~> st clause ...))].
}
