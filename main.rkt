#lang racket/base
(require (for-syntax racket/base
                     racket/function
                     racket/match
                     racket/struct-info
                     syntax/parse))

(begin-for-syntax
  (define (make-field-name-transformer instance-id-stx field-ref-stx field-set!-stx)
    (make-set!-transformer
     (lambda (stx)
       (syntax-parse stx
         #:track-literals
         [(set! id v)
          (if (syntax->datum field-set!-stx)
              (quasisyntax/loc stx
                (#,field-set!-stx #,instance-id-stx v))
              (raise-syntax-error 'set! "field not mutable" stx #'id))]
         [id (identifier? #'id)
             (quasisyntax/loc stx
               (#,field-ref-stx #,instance-id-stx))]
         [(id . args)
          (quasisyntax/loc stx
            ((#,field-ref-stx #,instance-id-stx) . args))])))))

(define-for-syntax (stx-transform-name stx transformer)
  (datum->syntax stx
                 (string->symbol
                  (transformer
                   (symbol->string
                    (syntax->datum stx))))))

(define-for-syntax (stx-add-prefix prefix separator stx)
  (match stx
    [#false #false]
    [stx (stx-transform-name stx (curry string-append prefix separator))]))

(define-syntax (struct-define stx)
  (syntax-parse stx
    [(_ the-struct
        the-instance:expr
        (~optional (~seq (~and #:prefix prefix-kw)))
        (~optional (~seq #:separator separator-kw)))
     #:declare the-struct
     (static struct-info? "structure type transformer binding")

     #:do [(define struct-info (extract-struct-info (attribute the-struct.value)))
           (define instance-name (symbol->string (syntax->datum #'the-instance)))
           (define field-names (struct-field-info-list (syntax-local-value #'the-struct)))
           (define field-refs (list-ref struct-info 3))
           (define field-sets (list-ref struct-info 4))
           (define prefix? (if (attribute prefix-kw) #true #false))
           (define separator (if (attribute separator-kw) (syntax->datum #'separator-kw) "."))]

     #:with ([field-name field-ref field-set!] ...)
     (for/list ([field-name (in-list field-names)]
                [field-ref (in-list field-refs)]
                [field-set (in-list field-sets)])
       (define field-name-stx (datum->syntax stx field-name))
       (define prefixed-name-stx (stx-add-prefix instance-name separator field-name-stx))
       (list (if prefix? prefixed-name-stx field-name-stx)
             field-ref
             field-set))

     #:with (field-val-id ...)
     (generate-temporaries #'(field-name ...))

     (syntax/loc stx
       (begin (define the-instance-id the-instance)
              (define-syntax field-name
                (make-field-name-transformer #'the-instance-id
                                             #'field-ref #'field-set!))
              ...))]))

(define-syntax-rule (define-struct-define the-struct-define the-struct)
  (define-syntax-rule (the-struct-define instance-id)
    (struct-define the-struct instance-id)))

(provide struct-define
         define-struct-define)

(module+ test
  (require rackunit)
  (struct point (x [y #:mutable]) #:transparent)
  (define p (point 2 3))
  (check-equal? (let ()
                  (struct-define point p)
                  (set! y 6)
                  (+ x y))
                8)
  (check-equal? (let ()
                  (struct-define point p #:prefix)
                  (set! p.y 7)
                  (+ p.x p.y))
                9)
  (check-equal? (let ()
                  (struct-define point p #:prefix #:separator ":")
                  (set! p:y 2)
                  (+ p:x p:y))
                4))
