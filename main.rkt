#lang racket/base
(require (for-syntax racket/base
                     racket/struct-info
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
  (define (make-field-name-transformer instance-id-stx field-ref-stx field-set!-stx)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
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

(define-syntax (struct-define stx)
  (syntax-parse stx
    [(_ the-struct
        the-instance:expr
        (~optional (~seq #:prefix prefix-id:id))
        (~optional (~seq #:separator separator-expr:expr)))

     #:declare the-struct
     (static struct-info? "structure type transformer binding")

     #:do [(define struct-info (extract-struct-info (attribute the-struct.value)))
           (define field-names (struct-field-info-list (syntax-local-value #'the-struct)))
           (define field-refs (list-ref struct-info 3))
           (define field-sets (list-ref struct-info 4))
           (define prefix? (if (attribute prefix-id) #true #false))
           (define prefix-name (and prefix? (syntax->datum #'prefix-id)))
           (define separator (if (attribute separator-expr) (syntax->datum #'separator-expr) "."))]

     #:with ([field-name field-ref field-set!] ...)
     (for/list ([field-name (in-list field-names)]
                [field-ref (in-list field-refs)]
                [field-set (in-list field-sets)])
       (list (if prefix?
                 (format-id #'the-instance "~a~a~a" prefix-name separator field-name)
                 (datum->syntax #'the-instance field-name))
             field-ref
             field-set))

     (syntax/loc stx
       (begin (define inst the-instance)
              (define-syntax field-name
                (make-field-name-transformer #'inst #'field-ref #'field-set!))
              ...))]))

(define-syntax (define-struct-define stx)
  (syntax-parse stx
    [(_ the-struct-define:id the-struct:id)
     #'(define-syntax-rule (the-struct-define the-instance)
         (struct-define the-struct the-instance))]

    [(_ the-struct-define:id the-struct:id #:prefix)
     #'(define-syntax (the-struct-define stx)
         (syntax-parse stx
           [(_ the-instance:id)
            #'(the-struct-define the-instance #:prefix the-instance)]
           [(_ the-instance:expr #:prefix the-prefix-id:id)
            #'(struct-define the-struct the-instance #:prefix the-prefix-id)]))]

    [(_ the-struct-define:id the-struct:id #:prefix #:separator separator-expr:expr)
     #'(define-syntax (the-struct-define stx)
         (syntax-parse stx
           [(_ the-instance:id)
            #'(the-struct-define the-instance #:prefix the-instance)]
           [(_ the-instance:expr #:prefix the-prefix-id:id)
            #'(struct-define the-struct the-instance #:prefix the-prefix-id #:separator separator-expr)]))]))

(provide struct-define
         define-struct-define)
