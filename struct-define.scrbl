#lang scribble/manual
@(require (for-syntax racket/base)
          (for-label struct-define
                     racket/contract
                     racket/base)
          racket/sandbox
          scribble/example)

@title{struct-define: Short-hand accessors for struct fields}
@author{Jay McCarthy}

@defmodule[struct-define]

@racketmodname[struct-define] provides a short-hand way of accessing
the fields of structu by the name used in the definition of the
structure.

@defform[(struct-define some-struct some-instance)]{If
@racket[some-struct] is defined with @racket[(struct some-struct (f0
... fN))], then defines @racket[f0] (through @racket[fN]) as macros
that expand to @racket[(some-struct-f0 some-instance)]. If a field
@racket[f0] is mutable, then @racket[(set! f0 x)] expands to
@racket[(set-some-struct-f0! some-instance x)]. (Note: This
explanation implies that @racket[struct-define] makes assumption about
the name of the accessors, but it actually uses whatever the real
identifiers embedded in the static struct record.)}

@defform[(define-struct-define the-struct-define the-struct)]{Defines
@racket[the-struct-define] such that @racket[(the-struct-define
the-instance)] expands to @racket[(struct-define the-struct
the-instance)].}

@examples[(require struct-define)
          (struct pie (flavor [temp #:mutable]))
          (define p1 (pie 'cherry 'warm))
          (list (pie-flavor p1) (pie-temp p1))
          (let ()
            (struct-define pie p1)
            (list flavor temp))
          (define-struct-define pie-define pie)
          (let ()
            (pie-define p1)
            (set! temp 'cold)
            (list flavor temp))
          (list (pie-flavor p1) (pie-temp p1))]
