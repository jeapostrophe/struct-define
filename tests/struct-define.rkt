#lang racket/base

(module+ test
  (require rackunit
           "../main.rkt")

  (struct point (x y)
    #:transparent)

  (define (add-xy p)
    (struct-define point p)
    (+ x y))

  (define (add-xy-dot p)
    (struct-define point p #:prefix p)
    (+ p.x p.y))

  (define (add-xy-colon p)
    (struct-define point p #:prefix p #:separator ":")
    (+ p:x p:y))

  (define (add-xy-expr a b)
    (struct-define point (point a b))
    (+ x y))

  (check-equal? (add-xy       (point 1 2)) 3)
  (check-equal? (add-xy-dot   (point 1 2)) 3)
  (check-equal? (add-xy-colon (point 1 2)) 3)
  (check-equal? (add-xy-expr 1 2) 3))
