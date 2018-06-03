(use apropos)
(use test)

;FIXME need #:split tests

;;

(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))
(define (car-symbol<? a b) (symbol<? (car a) (car b)))
(define (cdar-symbol<? a b) (symbol<? (cdar a) (cdar b)))

;;;

(test-begin "apropos")

;; build test symbols

(define (foobarproc0) 'foobarproc0)
(define (foobarproc1 a) 'foobarproc1)
(define (foobarproc2 a b) 'foobarproc2)
(define (foobarprocn a b . r) 'foobarprocn)

(define foobarprocx (lambda (a b c) 'foobarprocx))

;RQRD due to use of macro identifiers
(declare (compile-syntax))

(define-syntax (foobarmacro1 f r c)
  'foobarmacro1 )

(define-syntax foobarmacro2
  (syntax-rules ()
    ((_) 'foobarmacro1 ) ) )

(define foobarvar1 'foobarvar1)
(define foobarvar2 'foobarvar2)

(define Foobarvar1 'Foobarvar1)
(define Foobarvar2 'Foobarvar2)

#;(define (foocoreinline flag) (##core#inline "C_set_gc_report" flag))
#;(define fooprimitive (##core#primitive "C_get_memory_info"))

(define ##foo#bar1 '##foo#bar1)
(define ##foo#bar2 (lambda () '##foo#bar2))

(define ##bar#foo1 '##bar#foo1)
(define ##bar#foo2 (lambda () '##bar#foo2))

;; test for symbols

(define (apropos-list-test lst val)
  (test "apropos-list" (sort lst symbol<?) (sort val symbol<?)) )

(apropos-list-test
  '(foobarvar2 foobarvar1 foobarprocx foobarprocn foobarproc2 foobarproc1 foobarproc0)
  (apropos-list 'foobar))
(apropos-list-test
  '(foobarvar2 foobarvar1 foobarprocx foobarprocn foobarproc2 foobarproc1 foobarproc0)
  (apropos-list "foobar"))
(apropos-list-test
  '(##bar#foo1 ##bar#foo2 foobarmacro1 foobarmacro2 foobarproc0 foobarproc1 foobarproc2 foobarprocn foobarprocx foobarvar1 foobarvar2)
  (apropos-list 'foo #:macros? #t #:qualified? #t))
(apropos-list-test
  '(foobarvar2 foobarvar1 foobarprocx foobarprocn foobarproc2 foobarproc1 foobarproc0 foobarmacro1 foobarmacro2)
  (apropos-list 'foobar #:macros? #t))
(apropos-list-test
  '(foobarvar2 foobarvar1 foobarprocx foobarprocn foobarproc2 foobarproc1 foobarproc0 Foobarvar2 Foobarvar1)
  (apropos-list 'foobar #:case-insensitive? #t))
(apropos-list-test
  '(foobarvar2 foobarvar1 Foobarvar1 Foobarvar2)
  (apropos-list ''".+barvar[12]"))

;;

(define (apropos-information-list-test lst val)
  (test "apropos-information-list" (sort lst cdar-symbol<?) (sort val cdar-symbol<?)) )

#|
#;14> (define foobarprocx (lambda (a b c) 'foobarprocx))
#;15> '(((|| . foobarmacro1) . macro))
(((||: . foobarmacro1) . macro))
#;16> '(((||: . foobarmacro1) . macro))
(((: . foobarmacro1) . macro))
#;17> ||
||:
#;18> ||:

Error: unbound variable: :
#;19> #:||
||:
#;20> (eq? #:|| #:||)
#t
#;21> (caaar (apropos-information-list 'foobarproc))
||:
#;22> (eq? #:|| (caaar (apropos-information-list 'foobarproc)))
#f
|#

(cond-expand
  (compiling
    ;reads (|| . foobarmacro1) as ( . foobarmacro1)
    )
  (else
    ;oh , my : #:|| from reader is not eq? #:|| from symbol-table
    (apropos-information-list-test
      '(
        ((|| . foobarmacro1) . macro)
        ((|| . foobarmacro2) . macro)
        ((|| . foobarproc0) procedure)
        ((|| . foobarproc1) procedure a)
        ((|| . foobarproc2) procedure a b)
        ((|| . foobarprocn) procedure a b . r)
        ((|| . foobarprocx) procedure a b c)
        ((|| . foobarvar1) . variable)
        ((|| . foobarvar2) . variable) )
      (apropos-information-list 'foobar #:macros? #t #:qualified? #t))
    (test "apropos-information-list"
      '(((|| . foobarproc0) procedure)
        ((|| . foobarproc1) procedure a)
        ((|| . foobarproc2) procedure a b)
        ((|| . foobarprocn) procedure a b . r)
        ((|| . foobarprocx) procedure a b c))
      (apropos-information-list 'foobarproc #:macros? #t #:qualified? #t #:sort #:module)) ) )

#| ;UNSUPPORTED
;;

(use environments)

(define tstenv1 (make-environment #t))

(environment-extend! tstenv1 'foobarprocx (lambda (a b c) 'foobarprocx))
(environment-extend! tstenv1 'foobarvar1 'foobarvar1)
(environment-extend! tstenv1 'foobarvar2 'foobarvar2)
(environment-extend! tstenv1 '##bar#foo1 '##bar#foo1)
(environment-extend! tstenv1 '##bar#foo1 (lambda () '##bar#foo1))

;make-environment cannot create a syntax-environment
;apropos always uses the ##sys#macro-environment for macro lookup

(test '(foobarprocx foobarvar2 foobarvar1 ##bar#foo1)
      (apropos-list 'foo tstenv1 #:qualified? #t))
|#

(test-end)

;;;

(test-exit)
