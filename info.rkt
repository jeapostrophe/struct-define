#lang info
(define collection "struct-define")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "scribble-lib"))
(define version "0.1")
(define pkg-authors '(jeapostrophe))
(define scribblings '(("struct-define.scrbl" () ("Syntax Extensions"))))
