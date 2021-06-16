#lang racket

;; Uncomment to test.  Commented to prevent `raco setup` from
;; complaining about a missing dep on `typed-racket`, which we don't
;; want to depend on.

#;
(module+ test
  (require rackunit
           struct-define)

  (module typed typed/racket
    (provide (all-defined-out))
    (struct A ((x : Integer)
               (y : Integer))))

  (require 'typed (rename-in 'typed [A AB]))

  (let ()
    (struct-define A (A 1 2))
    (check-equal? x 1)
    (check-equal? y 2))

  (let ()
    (struct-define AB (AB 2 3))
    (check-equal? x 2)
    (check-equal? y 3)))
