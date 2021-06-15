#lang racket/base

(module+ test
  (require racket/math
           rackunit
           "../main.rkt")

  (struct point (x y)
    #:transparent)

  (define-struct-define point-define       point)
  (define-struct-define point-define-dot   point #:prefix)
  (define-struct-define point-define-colon point #:prefix #:separator ":")

  (define (add-xy p)
    (point-define p)
    (+ x y))

  (define (add-xy-dot p)
    (point-define-dot p)
    (+ p.x p.y))

  (define (add-xy-colon p)
    (point-define-colon p)
    (+ p:x p:y))

  (check-equal? (add-xy       (point 1 2)) 3)
  (check-equal? (add-xy-dot   (point 1 2)) 3)
  (check-equal? (add-xy-colon (point 1 2)) 3)

  (define (add-xy-expr a b)
    (point-define (point a b))
    (+ x y))

  (check-equal? (add-xy-expr 1 2) 3)

  ;;
  ;; This might be a more practical use of #:prefix
  ;;
  (define (dist p q)
    (point-define-dot p)
    (point-define-dot q)
    (sqrt
      (+ (sqr (- p.x q.x))
         (sqr (- p.y q.y)))))

  (check-equal? (dist (point 0 0)
                      (point 3 4)) 5))
