#lang racket/base
(require (for-syntax racket/base
                     racket/struct-info
                     syntax/parse))

(begin-for-syntax
  (define (make-field-name-transformer instace-id-stx field-ref-stx field-set!-stx)
    (make-set!-transformer
     (lambda (stx)
       (syntax-case stx (set!)
         [(set! id v)
          (if (syntax->datum field-set!-stx)
            (quasisyntax/loc stx
              (#,field-set!-stx #,instace-id-stx  v))
            (raise-syntax-error 'set! "field not mutable" stx #'id))]
         [id (identifier? #'id)
             (quasisyntax/loc stx
               (#,field-ref-stx #,instace-id-stx))]
         [(id . args)
          (quasisyntax/loc stx
            ((#,field-ref-stx #,instace-id-stx) . args))])))))

(define-syntax (struct-define stx)
  (syntax-parse stx
    [(_ the-struct the-instance:expr)
     #:declare the-struct
     (static struct-info? "structure type transformer binding")
     #:do [(define struct-info (extract-struct-info (attribute the-struct.value)))
           (define field-names (struct-field-info-list (syntax-local-value #'the-struct)))
           (define field-refs (list-ref struct-info 3))
           (define field-sets (list-ref struct-info 4))]
     #:with ([field-name field-ref field-set!] ...)
     (for/list ([field-name (in-list field-names)]
                [field-ref (in-list field-refs)]
                [field-set (in-list field-sets)])
       (define field-name-stx (datum->syntax stx field-name))
       (list field-name-stx field-ref field-set))
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
