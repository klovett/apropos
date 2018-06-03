;;;; apropos.scm -*- Hen -*-
;;;; From the Chicken 4 core, Version 4.0.0x5 - SVN rev. 13662
;;;; Kon Lovett, Mar '09
;;;; Kon Lovett, Oct '17

;; Issues
;;
;; - Use of 'global-symbol' routines is just wrong when an
;; evaluation-environment (##sys#environment?) is not the
;; interaction-environment.
;;
;; - Doesn't show something similar to procedure-information for macros. And
;; how could it.
;;
;; - Could be re-written to use the "environments" extension. Which in turn would
;; need to support syntactic environments, at least for lookup opertations.
;;
;; - The Chicken 'environment' object does not hold the (syntactic) bindings
;; for any syntactic keywords from the R5RS. The public API of 'apropos'
;; attempts to hide this fact.
;;
;; - old csi option
;; ; {{search|mode pre[fix]|suff[ix]|#t}} : {{#:search-mode #:prefix|#:suffix|#t}}
;; ; {{SEARCH-MODE}} : Either {{#:prefix}}, {{#:suffix}}, or {{#t}} for contains. The default is {{#t}}.

(module apropos

(;export
  ;
  apropos-interning apropos-default-options
  ;Original
  apropos apropos-list apropos-information-list
  ;Crispy
  ;apropos/environment apropos-list/environment apropos-information-list/environment
  ;Extra Crispy
  ;apropos/environments apropos-list/environments apropos-information-list/environments
)

(import scheme chicken foreign)

(import (only csi toplevel-command))  ;wtf?

(use
  (only data-structures
    sort! any?
    alist-ref alist-update!
    butlast
    string-split)
  (only ports
    with-input-from-string)
  (only extras
    read-file read-line)
  (only srfi-1
    cons*
    reverse! append!
    last-pair)
  (only srfi-13
    string-join
    string-trim-both
    string-contains string-contains-ci
    string-drop string-take string-index)
  (only irregex
    sre->irregex
    irregex irregex?
    irregex-num-submatches
    irregex-search irregex-match
    irregex-match-data? irregex-match-num-submatches
    irregex-replace)
  (only memoized-string
    make-string*)
  (only symbol-utils
    symbol->keyword
    symbol-printname=? symbol-printname<?
    symbol-printname-length max-symbol-printname-length )
  (only type-checks
    check-fixnum define-check+error-type)
  (only type-errors
    define-error-type error-argument-type)
  miscmacros)

(declare
  (bound-to-procedure
    ##sys#symbol-has-toplevel-binding?
    ##sys#qualified-symbol?
    ##sys#macro-environment
    ##sys#current-environment
    ##sys#macro? ) )

;;; Support

;;

(define-constant *CHICKEN-MAXIMUM-BASE* 16)

;;

(define (->boolean obj)
  (and
    obj
    #t ) )

;; irregex extensions

(define (irregex-submatches? mt #!optional ire)
  (and
    (irregex-match-data? mt)
    (or
      (not ire)
      (fx=
        (irregex-match-num-submatches mt)
        (if (fixnum? ire) ire (irregex-num-submatches ire))) ) ) )

;; raw access renames

(define system-current-environment ##sys#current-environment)

(define system-macro-environment ##sys#macro-environment)

(define (global-symbol-bound? sym)
  (##sys#symbol-has-toplevel-binding? sym) )

(define (global-symbol-ref sym)
  (##sys#slot sym 0) )

(define (symbol-macro-in-environment? sym macenv)
  (and sym macenv (##sys#macro? sym macenv)) )

(define (qualified-symbol? sym)
  (and sym (##sys#qualified-symbol? sym)) )

;; Constants

(define-constant CSI-HELP-HEAD (string-append ",a PATT ARG..." (make-string 4 #\space)))

;rmvd ", raw, base [#]"
(define-constant CSI-HELP-BODY
  "Apropos of PATT with ARG from ?, mac, split [nam|mod|#f], qual, ci, sort [nam|mod|typ|#f]")

(define-constant CSI-HELP (string-append CSI-HELP-HEAD CSI-HELP-BODY))

(define-constant HELP-TEXT
#<<EOS
Pattern:

 The Pattern PATT is a symbol, string, sre (see irregex), or quoted. Symbols &
 strings are interpreted as a substring match. The quoted object is described
 below.

 Use "?" to list symbols containing a `?`.

 The form '(PATT . _) is a synonym for `PATT split module`; '(_ . PATT) is
 `PATT split name`.

 Otherwise use the form '... to force interpretation of `...` as an irregex.

Arguments:

 macros            Include macro bound symbols
 qualified         Include "qualified" symbols
 ci | case-insensitive
                   Pattern has no capitals
 sort [name | module | type | #f]
                   Order items; optional when last argument
 split [name | module | #f]
                   Pattern match component; optional when last argument
                   (also see the '(_ . _) pattern)
 all               Means `ci qual mac`
 krl               Means `all sort mod`
 base              For number valued pattern
 raw               No listing symbol interpretation (i.e. x123 ~> x)
EOS
)

;; Types

(define (search-pattern? obj)
  (or
    (keyword? obj)
    (symbol? obj)
    (string? obj)
    (irregex? obj)
    (pair? obj)) )

(define (sort-key? obj)
  (or
    (not obj)
    (eq? #:name obj)
    (eq? #:module obj)
    (eq? #:type obj)) )

;; Errors

(define (error-argument loc arg)
  (if (keyword? arg)
    (error loc "unrecognized keyword argument" arg)
    (error loc "unrecognized argument" arg) ) )

;; Argument Checking

(define-check+error-type search-pattern search-pattern?
  "symbol/keyword/string/irregex/irregex-sre/quoted")

(define-check+error-type sort-key sort-key? "#:name, #:module, #:type or #f")

#; ;UNSUPPORTED
(define-check+error-type environment system-environment?)

;; Number Base

(define-constant *APROPOS-DEFAULT-BASE* 10)

(define (number-base? obj)
  (and (fixnum? obj) (fx<= 2 obj) (<= obj *CHICKEN-MAXIMUM-BASE*)) )

(define *number-base-error-message*
  (string-append "fixnum in 2.." (number->string *CHICKEN-MAXIMUM-BASE*)))

(define (check-number-base loc obj #!optional (var 'base))
  (unless (number-base? obj)
    (error-argument-type loc obj *number-base-error-message* var) )
  obj )

(define (check-split-component loc obj #!optional (var 'split))
  (case obj
    ((#f)
      obj )
    ((#:module #:name)
      obj )
    (else
      (error-argument-type loc obj "invalid identifier component" var)) ) )

;; Symbols

(define (string-irregex-match? str patt)
  (irregex-search patt str) )

(define (string-exact-match? str patt)
  (string-contains str patt) )

(define (string-ci-match? str patt)
  (string-contains-ci str patt) )

(define (symbol-irregex-match? sym patt)
  (string-irregex-match? (symbol->string sym) patt) )

(define (symbol-exact-match? sym patt)
  (string-exact-match? (symbol->string sym) patt) )

(define (symbol-ci-match? sym patt)
  (string-ci-match? (symbol->string sym) patt) )

(define *TOPLEVEL-MODULE-SYMBOL* '||)
(define *TOPLEVEL-MODULE-STRING* "" #;(symbol->string *TOPLEVEL-MODULE-SYMBOL*))

(: split-prefixed-symbol (symbol --> string string))
;
(define (split-prefixed-symbol sym)
  (let* (
    (str (symbol->string sym))
    ;assume # not part of module name
    (idx (string-index str #\#))
    (mod (if idx (string-take str idx) *TOPLEVEL-MODULE-STRING*))
    (nam (if idx (string-drop str (fx+ 1 idx)) str)) )
    ;
    (values mod nam) ) )

;; special stuff from the runtime & scheme API

#>
#define ROOT_SYMBOL_TABLE_NAME  "."

#define raw_symbol_table_size( stable )       ((stable)->size)
#define raw_symbol_table_chain( stable, i )   ((stable)->table[ (i) ])

#define raw_bucket_symbol( bucket )   (C_block_item( (bucket), 0 ))
#define raw_bucket_link( bucket )     (C_block_item( (bucket), 1 ))

static C_regparm C_SYMBOL_TABLE *
find_root_symbol_table()
{
  return C_find_symbol_table( ROOT_SYMBOL_TABLE_NAME );
}

static C_regparm C_SYMBOL_TABLE *
remember_root_symbol_table()
{
  static C_SYMBOL_TABLE *root_symbol_table = NULL;
  if(!root_symbol_table) {
    root_symbol_table = find_root_symbol_table();
  }

  return root_symbol_table;
}

//FIXME root_symbol_table re-allocated?
//#define use_root_symbol_table   find_root_symbol_table
#define use_root_symbol_table    remember_root_symbol_table
<#

(: root-symbol-table-size (--> fixnum))
;
(define root-symbol-table-size
  (foreign-lambda* int ()
    "C_return( raw_symbol_table_size( use_root_symbol_table() ) );") )

(: root-symbol-table-element (fixnum --> pair))
;
(define root-symbol-table-element
  (foreign-lambda* scheme-object ((int i))
    "C_return( raw_symbol_table_chain( use_root_symbol_table(), i ) );") )

(: bucket-symbol (pair --> symbol))
;
(define bucket-symbol
  (foreign-lambda* scheme-object ((scheme-object bucket))
    "C_return( raw_bucket_symbol( bucket ) );"))

(: bucket-link (pair --> list))
;
(define bucket-link
  (foreign-lambda* scheme-object ((scheme-object bucket))
    "C_return( raw_bucket_link( bucket ) );"))

(: bucket-last? (list --> boolean))
;
(define bucket-last? null?)

;;

(define-type <symbol-table-cursor> (or boolean pair))

(: make-symbol-table-cursor (* * --> <symbol-table-cursor>))
;
(define make-symbol-table-cursor cons)

(: symbol-table-cursor-active? (* --> boolean))
;
(define symbol-table-cursor-active? pair?)

(: symbol-table-cursor? (* --> boolean))
;
(define (symbol-table-cursor? obj)
  (or
    (not obj)
    (symbol-table-cursor-active? obj)) )

(: symbol-table-cursor-index (<symbol-table-cursor> --> *))
;
(define symbol-table-cursor-index car)

(: set-symbol-table-cursor-index! (<symbol-table-cursor> * -> void))
;
(define set-symbol-table-cursor-index! set-car!)

(: symbol-table-cursor-bucket (<symbol-table-cursor> --> *))
;
(define symbol-table-cursor-bucket cdr)

(: set-symbol-table-cursor-bucket! (<symbol-table-cursor> * -> void))
;
(define set-symbol-table-cursor-bucket! set-cdr!)

(: symbol-table-cursor (--> <symbol-table-cursor>))
;
(define (symbol-table-cursor)
  (make-symbol-table-cursor -1 '()) )

;;

(: search-interaction-environment-symbols (* procedure --> list))
;
(define (search-interaction-environment-symbols env optarg?)
  (let loop ((cursor (initial-symbol-table-cursor)) (syms '()))
    (let ((sym (root-symbol cursor)))
      (if (not sym)
        syms
        (let ((syms (if (optarg? sym) (cons sym syms) syms)))
          (loop (next-root-symbol cursor) syms) ) ) ) ) )

(: search-list-environment-symbols (list procedure --> list))
;
(define (search-list-environment-symbols env optarg?)
  (foldl
    (lambda (syms cell)
      (let ((sym (car cell)))
        (if (optarg? sym)
          (cons sym syms)
          syms ) ) )
    '()
    env) )

(: search-macro-environment-symbols (list procedure --> list))
;
(define (search-macro-environment-symbols env optarg?)
  (search-list-environment-symbols env optarg?) )

(: search-system-environment-symbols (list procedure --> list))
;
(define (search-system-environment-symbols env optarg?)
  (if env
    (search-list-environment-symbols env optarg?)
    (search-interaction-environment-symbols env optarg?) ) )

;;

(: next-root-symbol (<symbol-table-cursor> --> <symbol-table-cursor>))
;
(define (next-root-symbol cursor)
  (and
    (symbol-table-cursor-active? cursor)
    (let loop (
      (bkt (bucket-link-ref (symbol-table-cursor-bucket cursor)))
      (idx (symbol-table-cursor-index cursor)))
      ;gotta bucket ?
      (if (and bkt (not (bucket-last? bkt)))
        ;then found something => where we are
        (make-symbol-table-cursor idx bkt)
        ;else try next hash-root slot
        (let ((idx (fx+ 1 idx)))
          (and
            ;more to go ?
            (< idx (root-symbol-table-size))
            ;this slot
            (loop (root-symbol-table-element idx) idx) ) ) ) ) ) )

(: initial-symbol-table-cursor (--> <symbol-table-cursor>))
;
(define (initial-symbol-table-cursor)
  (next-root-symbol (symbol-table-cursor)) )

(: root-symbol (<symbol-table-cursor> --> (or boolean symbol)))
;
(define (root-symbol cursor)
  (and
    (symbol-table-cursor-active? cursor)
    (bucket-symbol-ref (symbol-table-cursor-bucket cursor)) ) )

(: bucket-symbol-ref (list --> (or boolean symbol)))
;
(define (bucket-symbol-ref bkt)
  (and
    (not (bucket-last? bkt))
    (bucket-symbol bkt) ) )

(: bucket-link-ref (list --> (or boolean list)))
;
(define (bucket-link-ref bkt)
  (and
    (not (bucket-last? bkt))
    (bucket-link bkt)) )

;;

;;

#; ;UNSUPPORTED
(define (system-environment? obj)
  (or (##sys#environment? obj) (sys::macro-environment? obj)) )

;; Environment Search

(define (*apropos-list/macro-environment loc symbol-match? macenv qualified?)
  (let (
    (optarg?
      (if qualified?
        any?
        (lambda (x)
          (not (qualified-symbol? x))))))
    (search-macro-environment-symbols macenv
      (lambda (sym)
        (and
          (symbol-match? sym)
          (optarg? sym)))) ) )

(define (*apropos-list/environment loc symbol-match? env qualified?)
  (let (
    (optarg?
      (if qualified?
        global-symbol-bound?
        (lambda (x)
          (and
            (not (qualified-symbol? x))
            (global-symbol-bound? x))))))
    ;
    (search-system-environment-symbols env
      (lambda (sym)
        (and
          (symbol-match? sym)
          (optarg? sym)))) ) )

;;

; => (envsyms . macenvsyms)
(define (*apropos-list loc symbol-match? env macenv qualified?)
  (append
    (*apropos-list/environment loc symbol-match? env qualified?)
    (if macenv
      (*apropos-list/macro-environment loc symbol-match? macenv qualified?)
      '())) )

;; Argument List Parsing

(define default-environment system-current-environment)
(define default-macro-environment system-macro-environment)

(define (make-apropos-matcher loc patt
            #!optional (case-insensitive? #f) (split #f) (force-regexp? #f))
  ;
  (define (gen-irregex-options-list)
    (if case-insensitive? '(case-insensitive) '()) )
  ;
  (define (gen-irregex patt)
    (apply irregex patt (gen-irregex-options-list)) )
  ;
  (define (gen-irregex-matcher irx)
    (cond
      ((eq? #:module split)
        (lambda (sym)
          (let-values (
            ((mod nam) (split-prefixed-symbol sym)) )
            (string-irregex-match? mod irx) ) ) )
      ((eq? #:name split)
        (lambda (sym)
          (let-values (
            ((mod nam) (split-prefixed-symbol sym)) )
            (string-irregex-match? nam irx) ) ) )
      ((not split)
        (cut symbol-irregex-match? <> irx) ) ) )
  ;
  (define (gen-string-matcher str)
    (if (not split)
      ;no split
      (cut (if case-insensitive? symbol-ci-match? symbol-exact-match?) <> str)
      ;splitting
      (let (
        (matcher (if case-insensitive? string-ci-match? string-exact-match?)) )
        (cond
          ((eq? #:module split)
            (lambda (sym)
              (let-values (
                ((mod nam) (split-prefixed-symbol sym)) )
                (matcher mod str) ) ) )
          ((eq? #:name split)
            (lambda (sym)
              (let-values (
                ((mod nam) (split-prefixed-symbol sym)) )
                (matcher nam str) ) ) ) ) ) ) )
  ;
  (cond
    ((symbol? patt)
      (make-apropos-matcher loc
        (symbol->string patt)
        case-insensitive? split force-regexp?) )
    ((string? patt)
      (if force-regexp?
        (gen-irregex-matcher (gen-irregex patt))
        (gen-string-matcher patt)) )
    ((irregex? patt)
      (gen-irregex-matcher patt) )
    ((pair? patt)
      (if (not (eq? 'quote (car patt)))
        ;then assume an irregex
        (gen-irregex-matcher (gen-irregex patt))
        ;else some form of pattern
        (let (
          (quoted (cadr patt)) )
          (if (pair? quoted)
            ;then could be a split (name|module) pattern
            (cond
              ;name split?
              ((eq? '_ (car quoted))
                (make-apropos-matcher loc
                  (cdr quoted)
                  case-insensitive? #:name force-regexp?) )
              ;module split?
              ((eq? '_ (cdr quoted))
                (make-apropos-matcher loc
                  (car quoted)
                  case-insensitive? #:module force-regexp?) )
              ;else force interpretation as irregex
              (else
                (make-apropos-matcher loc
                  quoted
                  case-insensitive? split #t) ) )
            ;else force interpretation as irregex
            (make-apropos-matcher loc
              quoted
              case-insensitive? split #t) ) ) ) )
    (else
      (error loc "invalid apropos pattern form" patt) ) ) )

;;

; => (values val args)
(define (keyword-argument args kwd #!optional val)
  (let loop ((args args) (oargs '()))
    (if (null? args)
      (values val (reverse! oargs))
      (let ((arg (car args)))
        (cond
          ((eq? kwd arg)
            (set! val (cadr args))
            (loop (cddr args) oargs) )
          (else
            (loop (cdr args) (cons arg oargs)) ) ) ) ) ) )

; => (values sort-key args)
(define (parse-sort-key-argument loc args)
  (receive (sort-key args) (keyword-argument args #:sort #:type)
    (values (check-sort-key loc sort-key #:sort) args) ) )

;;

;#!optional (env (default-environment)) macenv #!key macros? qualified? base (split #:all)
;
;macenv is #t for default macro environment or a macro-environment object.
;
;=> (values apropos-ls macenv)
(define (parse-arguments-and-match loc patt iargs)
  (let-values (
    ((env macenv qualified? case-insensitive? base raw? split)
      (parse-rest-arguments loc iargs)))
    ;
    (let* (
      (patt
        (check-search-pattern loc (fixup-pattern-argument patt base) 'pattern) )
      (matcher
        (make-apropos-matcher loc patt case-insensitive? split) )
      (als
        (*apropos-list loc matcher env macenv qualified?) ) )
      ;
      (values als macenv raw?) ) ) )
;;

;=> (values env macenv qualified? base)
(define (parse-rest-arguments loc iargs)
  (let (
    (env #f)        ;(default-environment)
    (macenv #f)
    (qualified? #f)
    (raw? #f)
    (case-insensitive? #f)
    (split #f)
    (base *APROPOS-DEFAULT-BASE*)
    (1st-arg? #t) )
    ;
    (let loop ((args iargs))
      (if (null? args)
        ;seen 'em all
        (values env macenv qualified? case-insensitive? base raw? split)
        ;process potential arg
        (let ((arg (car args)))
          ;keyword argument?
          (cond
            ;
            ((eq? #:split arg)
              (set! split (check-split-component loc (cadr args)))
              (loop (cddr args)) )
            ;
            ((eq? #:raw? arg)
              (set! raw? (cadr args))
              (loop (cddr args)) )
            ;
            ((eq? #:base arg)
              (when (cadr args)
                (set! base (check-number-base loc (cadr args))) )
              (loop (cddr args)) )
            ;
            ((eq? #:macros? arg)
              ;only flag supported
              (when (cadr args)
                (set! macenv (default-macro-environment)) )
              (loop (cddr args)) )
            ;
            ((eq? #:qualified? arg)
              (set! qualified? (cadr args))
              (loop (cddr args)) )
            ;
            ((eq? #:case-insensitive? arg)
              (set! case-insensitive? (cadr args))
              (loop (cddr args)) )
            ;environment argument?
            (1st-arg?
              ;FIXME need real 'environment?' predicate
              (unless (list? arg)
                (error-argument loc arg) )
              (set! 1st-arg? #f)
              (set! env arg)
              (loop (cdr args)) )
            ;unkown argument
            (else
              (error-argument loc arg) ) ) ) ) ) ) )

;;

(define (fixup-pattern-argument patt #!optional (base *APROPOS-DEFAULT-BASE*))
  (cond
    ((boolean? patt)
      (if patt "#t" "#f") )
    ((char? patt)
      (string patt) )
    ((number? patt)
      (number->string patt base) )
    ;? pair vector ... ->string , struct use tag as patt ?
    (else
      patt ) ) )

#| ;UNSUPPORTED ;FIXME case-insensitive support
;;

(define (macro-environment obj)
  (and
    (sys::macro-environment? obj)
    obj) )

;;

; => (values envsyms macenv)

(define (parse-arguments/environment loc patt env qualified?)
  (check-search-pattern loc patt 'pattern)
  (let ((macenv (macro-environment (check-environment loc env 'environment))))
    (values
      (*apropos-list/environment loc (make-apropos-matcher loc patt) env macenv qualified?)
      macenv) ) )

;;

; #!key qualified?
;
; => (... (macenv . syms) ...)

(define (parse-arguments/environments loc patt args)
  ;
  (define (parse-rest-arguments)
    (let ((qualified? #f))
      (let loop ((args args) (envs '()))
        (if (null? args)
          (values (reverse! envs) qualified?)
          (let ((arg (car args)))
            ;keyword argument?
            (cond
              ((eq? #:qualified? arg)
                (when (cadr args) (set! qualified? #t))
                (loop (cddr args) envs) )
              ;environment argument?
              (else
                (unless (##sys#environment? arg)
                  (error-argument loc arg) )
                (loop (cdr args) (cons arg envs)) ) ) ) ) ) ) )
  ;
  (let ((patt (fixup-pattern-argument patt)))
    (check-search-pattern loc patt 'pattern)
    (receive (envs qualified?) (parse-rest-arguments)
      (let ((regexp (make-apropos-matcher loc patt)))
        (let loop ((envs envs) (envsyms '()))
          (if (null? envs)
            (reverse! envsyms)
            (let* ((env (car envs))
                   (macenv (macro-environment (check-environment loc env 'environment)))
                   (make-envsyms
                     (lambda ()
                       (cons
                         macenv
                         (*apropos-list/environment loc regexp env macenv qualified?)) ) ) )
              (loop (cdr envs) (cons (make-envsyms) envsyms)) ) ) ) ) ) ) )
|#

;;; Display

;;

(define apropos-interning (make-parameter #t (lambda (x)
  (if (boolean? x)
    x
    (begin
      (warning 'apropos-interning "not a boolean: " x)
      (apropos-interning))))))

(define (string->display-symbol str)
  ((if (apropos-interning) string->symbol string->uninterned-symbol) str) )

;;

;;

#| ;A Work In Progress

; UNDECIDEDABLE - given the data available from `procedure-information',
; serial nature of `gensym', and serial nature of argument coloring by
; compiler.

; `pointer+' is an example of a `foreign-lambda*', here all info is lost & the
; gensym identifiers can just be colored using a base of 1.

;best guess:
;
;here `(cs1806 cs2807 . csets808)'        `(cs1 cs2 . csets)'
;here `(foo a1 b2)'                       `(foo a1 b2)'
;here `(a380384 a379385)'                 `(arg1 arg2)'
;here `(=1133 lis11134 . lists1135)'      `(= lis1 . lists)'

(define apropos-gensym-suffix-limit 1)

;When > limit need to keep leading digit

; un-qualified symbols only!
(define (scrub-gensym-taste sym #!optional (limit apropos-gensym-suffix-limit))
  (let* (
    (str (symbol->string sym))
    (idx (string-skip-right str char-set:digit))
    (idx (and idx (fx+ 1 idx))) )
    ;
    (cond
      ((not idx)
        sym )
      ((fx< (fx- (string-length str) idx) limit)
        sym )
      (else
        (string->display-symbol (substring str 0 idx)) ) ) ) )

; arg-lst-template is-a pair!
(define (scrub-gensym-effect arg-lst-template)
  (let (
    (heads (butlast arg-lst-template))
    (tailing (last-pair arg-lst-template)) )
    ;
    (append!
      (map scrub-gensym-taste heads)
      (if (null? (cdr tailing))
        (list (scrub-gensym-taste (car tailing)))
        (cons
          (scrub-gensym-taste (car tailing))
          (scrub-gensym-taste (cdr tailing)))) ) ) )
|#

(define (identifier-components sym raw?)
  (cond
    (raw?
      (cons *TOPLEVEL-MODULE-SYMBOL* sym) )
    ((qualified-symbol? sym)
      (cons *TOPLEVEL-MODULE-SYMBOL* sym) )
    (else
      (let-values (
        ((mod nam) (split-prefixed-symbol sym)) )
        (cons (string->display-symbol mod) (string->display-symbol nam)) ) ) ) )

;FIXME make patt a param ?
(define *GENSYM_SRE* (sre->irregex '(: bos (>= 2 any) (>= 2 num) eos) 'utf8 'fast))
(define *GENSYM_DEL_SRE* (sre->irregex '(: (* num) eos) 'utf8 'fast))

(define (canonical-identifier-name id raw?)
  (if raw?
    id
    (let* (
      (pname (symbol->string id) )
      (mt (irregex-match *GENSYM_SRE* pname) ) )
      ;
      (if (irregex-submatches? mt *GENSYM_SRE*)
        (string->display-symbol (irregex-replace *GENSYM_DEL_SRE* pname ""))
        id ) ) ) )

(define (canonicalize-identifier-names form raw?)
  (cond
    (raw?
      form )
    ((symbol? form)
      (canonical-identifier-name form raw?) )
    ((pair? form)
      (cons
        (canonicalize-identifier-names (car form) raw?)
        (canonicalize-identifier-names (cdr form) raw?)) )
    (else
      form ) ) )

; => 'procedure | (procedure . <symbol>) | (procedure . <list>) | (procedure . <string>)
(define (procedure-details proc raw?)
  (let ((info (procedure-information proc)))
    (cond
      ((not info)
        'procedure )
      ((pair? info)
        `(procedure . ,(canonicalize-identifier-names (cdr info) raw?)) )
      (else
        ;was ,(symbol->string info) (? why)
        `(procedure . ,(canonical-identifier-name info raw?)) ) ) ) )

; => 'macro | 'keyword | 'variable | <procedure-details>
(define (identifier-type-details sym #!optional macenv raw?)
  (cond
    ((symbol-macro-in-environment? sym macenv)
      'macro )
    ((keyword? sym)
      'keyword )
    (else
      (let ((val (global-symbol-ref sym)))
        (if (procedure? val)
          (procedure-details val raw?)
          'variable ) ) ) ) )

;;

(define (make-information sym macenv raw?)
  (cons
    (identifier-components sym raw?)
    (identifier-type-details sym macenv raw?)) )

(define (*make-information-list syms macenv raw?)
  (map (cut make-information <> macenv raw?) syms) )

(define (identifier-information-module ident-info)
  (car ident-info) )

(define (identifier-information-name ident-info)
  (cdr ident-info) )

(define (detail-information-kind dets-info)
  (car dets-info) )

(define (detail-information-arguments dets-info)
  (cdr dets-info) )

(define (information-identifiers info)
  (car info) )

(define (information-module info)
  (identifier-information-module (information-identifiers info)) )

(define (information-name info)
  (identifier-information-name (information-identifiers info)) )

(define (information-details info)
  (cdr info) )

(define (information-identifier<? info1 info2 #!optional (sort-key #:name))
  (receive
    (field-1-ref field-2-ref)
      (if (eq? #:name sort-key)
        (values information-name information-module)
        (values information-module information-name) )
    (let (
      (sym-1-1 (field-1-ref info1) )
      (sym-1-2 (field-1-ref info2) ) )
      (if (not (symbol-printname=? sym-1-1 sym-1-2))
        (symbol-printname<? sym-1-1 sym-1-2)
        (symbol-printname<? (field-2-ref info1) (field-2-ref info2)) ) ) ) )

(define (information-kind info)
  (let ((d (information-details info)))
    (if (symbol? d) d (car d)) ) )

(define (information-kind=? info1 info2)
  (symbol-printname=?
    (information-kind info1)
    (information-kind info2)) )

(define (information-kind<? info1 info2)
  (symbol-printname<?
    (information-kind info1)
    (information-kind info2)) )

(define (information<? info1 info2 #!optional (sort-key #:name))
  (if (information-kind=? info1 info2)
    (information-identifier<? info1 info2 sort-key)
    (information-kind<? info1 info2) ) )

;;

(define (make-sorted-information-list syms macenv sort-key raw?)
  (let (
    (lessp
      (case sort-key
        ((#:name #:module)
          (cut information-identifier<? <> <> sort-key) )
        ((#:type)
          (cut information<? <> <> #:name) )
        (else
          #f ) ) )
    (ails
      (*make-information-list syms macenv raw?) ) )
    ;
    (if lessp
      (sort! ails lessp)
      ails ) ) )

(define (symbol-pad-length sym maxsymlen)
  (let* (
    (len (symbol-printname-length sym) )
    (maxlen (fxmin maxsymlen len) ) )
    ;
    (fx- maxsymlen maxlen) ) )

(define (display-apropos isyms macenv sort-key raw?)
  ;
  (let* (
    (ails (make-sorted-information-list isyms macenv sort-key raw?) )
    (mods (map information-module ails) )
    (syms (map information-name ails) )
    (maxmodlen (max-symbol-printname-length mods) )
    (maxsymlen (max-symbol-printname-length syms) ) )
    ;
    (define (display-symbol-information info)
      ;<sym><tab>
      (let* (
        (sym (information-name info) )
        (sym-padlen (symbol-pad-length sym maxsymlen) ) )
        ;
        (display sym)
        (display (make-string* (fx+ 2 sym-padlen))) )
      ;<mod><tab>
      (let* (
        (mod (information-module info) )
        (mod-padlen (symbol-pad-length mod maxmodlen) ) )
        ;
        (if (eq? *TOPLEVEL-MODULE-SYMBOL* mod)
          (display (make-string* mod-padlen))
          (begin
            (display mod)
            (display (make-string* (fx+ 2 mod-padlen))) ) ) )
      ;<details>
      (let ((dets (information-details info)))
        (cond
          ((symbol? dets)
            (display dets) )
          (else
            (display (detail-information-kind dets))
            (display #\space)
            (write (detail-information-arguments dets)) ) ) )
      ;d'oy
      (newline) )
    ;
    (for-each display-symbol-information ails) ) )

;;; API

(define-constant KRL-OPTIONS '(
  #:sort #:module #:case-insensitive? #t #:qualified? #t #:macros? #t))

(define apropos-default-options (make-parameter '() (lambda (x)
  (cond
    ((boolean? x)
      (or
        (and x KRL-OPTIONS)
        '() ) )
    ((list? x)
      x )
    (else
      (warning 'apropos-default-options "not a list of options" x)
      (apropos-default-options))))))

;; Original

(define (apropos patt . args)
  (let (
    (args (if (null? args) (apropos-default-options) args)) )
    (let*-values (
      ((sort-key args) (parse-sort-key-argument 'apropos args) )
      ((syms macenv raw?) (parse-arguments-and-match 'apropos patt args) ) )
      ;
      (display-apropos syms macenv sort-key raw?) ) ) )

(define (apropos-list patt . args)
  (let (
    (args (if (null? args) (apropos-default-options) args)) )
    (let*-values (
      ((sort-key args) (parse-sort-key-argument 'apropos-list args) )
      ((syms macenv raw?) (parse-arguments-and-match 'apropos-list patt args) ) )
      ;
      syms ) ) )

(define (apropos-information-list patt . args)
  (let (
    (args (if (null? args) (apropos-default-options) args)) )
    (let*-values (
      ((sort-key args) (parse-sort-key-argument 'apropos-information-list args) )
      ((syms macenv raw?) (parse-arguments-and-match 'apropos-information-list patt args) ) )
      ;
      (make-sorted-information-list syms macenv sort-key raw?) ) ) )

;;;
;;; REPL Integeration
;;;

(define (interp-split-arg loc arg)
  (case arg
    ((n nam name)
      #:name )
    ((m mod module)
      #:module )
    (else
      (if (not arg)
        #f
        (error-sort-key loc "unknown split key" arg) ) ) ) )

(define (interp-sort-arg loc arg)
  (case arg
    ((n nam name)
      #:name )
    ((m mod module)
      #:module )
    ((t typ type)
      #:type )
    (else
      (if (not arg)
        #f
        (error-sort-key loc "unknown sort key" arg) ) ) ) )

(define (display-apropos-help)
  (print CSI-HELP)
  (print)
  (print HELP-TEXT) )

(define (parse-csi-apropos-arguments iargs)
  (let loop ((args iargs) (oargs '()))
    ;
    (define (restargs next optarg?)
      (cond
        ((null? next)
          '() )
        (optarg?
          (cdr next) )
        (else
          next ) ) )
    ;
    (define (arg-next kwd init #!optional optarg?)
      ;
      (define (thisargs next kwd init optarg?)
        (cond
          ((null? next)
            (cons* init kwd oargs) )
          (optarg?
            (cons* (optarg? (car next)) kwd oargs) )
          (else
            (cons* init kwd oargs) ) ) )
      ;
      (let* (
        (next (cdr args) )
        (args (restargs next optarg?) )
        (oargs (thisargs next kwd init optarg?) ) )
        ;
        (loop args oargs) ) )
    ;
    (if (null? args)
      ; original ordering
      (reverse! oargs)
      ;csi-apropos-syntax => keyword-apropos-syntax
      (let ((arg (car args)))
        (case arg
          ;
          ((krl)
            (loop
              (restargs (cons* 'all (cdr args)) #f)
              (cons* #:module #:sort oargs)) )
          ;
          ((all)
            (loop
              (restargs (cdr args) #f)
              (cons* #t #:case-insensitive? #t #:qualified? #t #:macros? oargs)) )
          ;
          ((mac macros)
            (arg-next #:macros? #t) )
          ;
          ((qual qualified)
            (arg-next #:qualified? #t) )
          ;
          ((ci case-insensitive)
            (arg-next #:case-insensitive? #t) )
          ;
          ((raw)
            (arg-next #:raw? #t) )
          ;
          ((base)
            (arg-next #:base *APROPOS-DEFAULT-BASE* (cut check-number-base ',a <>)) )
          ;
          ((sort)
            (arg-next #:sort #:type (cut interp-sort-arg ',a <>)) )
          ;
          ((split)
            (arg-next #:split #f (cut interp-split-arg ',a <>)) )
          ;
          ((?)
            (loop '() '()) )
          ;
          (else
            (loop (cdr args) (cons arg oargs)) ) ) ) ) ) )

(define (csi-apropos-command)
  (let* (
    (cmdlin (read-line) )
    (istr (string-trim-both cmdlin) )
    (iargs (with-input-from-string istr read-file) )
    (aargs (parse-csi-apropos-arguments iargs)))
    ;NOTE will not dump the symbol-table unless explicit ; use '(: (* any))
    (cond
      ((null? aargs)
        (display-apropos-help) )
      ((null? (cdr aargs))
        (apply apropos (car aargs) (apropos-default-options)) )
      (else
        (apply apropos aargs) ) ) ) )

;;; Main

(when (feature? csi:)
  (toplevel-command 'a csi-apropos-command CSI-HELP) )

) ;module apropos

#| ;UNSUPPORTED ;FIXME case-insensitive support

;; Crispy

==== apropos/environment

<procedure>(apropos/environment PATTERN ENVIRONMENT (#:qualified? QUALIFIED?) (#:sort SORT))</procedure>

Displays information about identifiers matching {{PATTERN}} in the
{{ENVIRONMENT}}.

Like {{apropos}}.

; {{ENVIRONMENT}} : An {{environment}} or a {{macro-environment}}.

==== apropos-list/environment

<procedure>(apropos-list/environment PATTERN ENVIRONMENT (#:qualified? QUALIFIED?))</procedure>

Like {{apropos-list}}.

==== apropos-information-list/environment

<procedure>(apropos-information-list/environment PATTERN ENVIRONMENT (#:qualified? QUALIFIED?))</procedure>

Like {{apropos-information-list}}.

(define (apropos/environment patt env #!key qualified? (sort #:name))
  (check-sort-key 'apropos/environment sort #:sort)
  (receive
    (syms macenv)
      (parse-arguments/environment 'apropos/environment patt env qualified?)
    ;
    (newline)
    (display-apropos syms macenv sort-key) ) )

(define (apropos-list/environment patt env #!key qualified?)
  (receive
    (syms macenv)
      (parse-arguments/environment 'apropos/environment patt env qualified?)
    ;
    syms ) )

(define (apropos-information-list/environment patt env #!key qualified?)
  (receive
    (syms macenv)
      (parse-arguments/environment 'apropos/environment patt env qualified?)
    ;
    (*make-information-list syms macenv) ) )

;; Extra Crispy

==== apropos/environments

<procedure>(apropos/environments PATTERN (#:qualified? QUALIFIED?) (#:sort SORT) ENVIRONMENT...)</procedure>

Displays information about identifiers matching {{PATTERN}} in each
{{ENVIRONMENT}}.

Like {{apropos}}.

; {{PATTERN}} : A {{symbol}}, {{string}} or {{regexp}}. When symbol or string substring matching is performed.

==== apropos-list/environments

<procedure>(apropos-list/environments PATTERN (#:qualified? QUALIFIED?) ENVIRONMENT...)</procedure>

Like {{apropos-list}}.

==== apropos-information-list/environments

<procedure>(apropos-information-list/environments PATTERN (#:qualified? QUALIFIED?) ENVIRONMENT...)</procedure>

Like {{apropos-information-list}}.

(define (apropos/environments patt . args)
  (let-values (((sort-key args) (parse-sort-key-argument 'apropos/environments args)))
    (let ((i 0))
      (for-each
        (lambda (macenv+syms)
          (set! i (fx+ 1 i))
          (newline) (display "** Environment " i " **") (newline) (newline)
          (display-apropos (cdr macenv+syms) (car macenv+syms) sort-key) )
        (parse-arguments/environments 'apropos/environments patt args)) ) ) )

(define (apropos-list/environments patt . args)
  (map cdr (parse-arguments/environments 'apropos-list/environments patt args)) )

(define (apropos-information-list/environments patt . args)
  (map
    (lambda (macenv+syms) (*make-information-list (cdr macenv+syms) (car macenv+syms)))
    (parse-arguments/environments 'apropos-information-list/environments patt args)) )
|#
