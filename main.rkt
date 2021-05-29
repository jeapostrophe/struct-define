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
        the-instance:id
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
       (define field-name-stx (datum->syntax #'the-instance field-name))
       (define prefixed-name-stx (format-id #'the-instance "~a~a~a" instance-name separator field-name))
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

(define-syntax (define-struct-define stx)
  (syntax-parse stx
    [(_ the-struct-define:id the-struct:id)
     #'(define-syntax-rule (the-struct-define instance-id)
         (struct-define the-struct instance-id))]

    [(_ the-struct-define:id the-struct:id #:prefix)
     #'(define-syntax-rule (the-struct-define instance-id)
         (struct-define the-struct instance-id #:prefix))]

    [(_ the-struct-define:id the-struct:id #:prefix #:separator separator-kw)
     #'(define-syntax-rule (the-struct-define instance-id)
         (struct-define the-struct instance-id #:prefix #:separator separator-kw))]))

(provide struct-define
         define-struct-define)

