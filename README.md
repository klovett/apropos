
[[tags: egg]]

== apropos

https://github.com/klovett-chicken/apropos

[[toc:]]


== Documentation

An ''apropos'' facility for Chicken Scheme.

When loaded by the interpreter {{csi}} {{apropos}} provides the interpreter
command {{a}}.

=== Apropos API

==== apropos

<procedure>(apropos PATTERN #!key macros? qualified? sort case-insensitive? raw? base split)</procedure>

Displays information about symbols matching {{PATTERN}} in the toplevel
environment.

; {{PATTERN}} : A {{symbol}}, {{string}}, {{irregex}}, {{irregex-sre}}, {{(quote symbol)}}, {{(quote string)}}. When unquoted symbol or string substring matching is performed. When quoted the string value is taken as an irregex regular expression string for use with search. Should {{PATTERN}} be a namespace qualified symbol the namespace will be dropped before conversion to a regular-expression.
; {{MACROS?}} : Either {{#t}} to include macros, or {{#f}} to skip them. Default is {{#f}}.
; {{QUALIFIED?}} : Either {{#t}} to include qualified symbols or {{#f}} to skip them. Default is {{#f}}.
; {{CASE-INSENSITIVE?}} : Either {{#t}} to use match case-insensitivity for the {{PATTERN}} or {{#f}} to be case-sensitive. Default is {{#f}}.
; {{SORT}} : Either {{#:name}} for an symbol sort, {{#:module}} for an module symbol sort, or {{#:type}} for a type+indentifier sort, or {{#f}} for unsorted. Default is {{#:type}}.
; {{SPLIT}} : Either {{#:name}} for an symbol match, {{#:module}} for an module match, or {{#f}} for any. Default is {{#f}}.
; {{RAW}} : Do not strip identifier symbols. The default is {{#f}}. Use {{#:raw? #t}} when ''apropos'' is suspected of hiding information.
; {{BASE}} : {{fixnum}} in {{2..16}}. The default is {{10}}. {{BASE}} is used to convert a {{number}} to a string {{PATTERN}}. Of dubious utility. But can ''fix'' the situation of entering a hex number, say {{#x1c}}, for the pattern using the REPL and matching against {{28}}!

==== apropos-list

<procedure>(apropos-list PATTERN #!key macros? qualified? case-insensitive? raw? base) => list</procedure>

Like {{apropos}} but returns an, unsorted, list of matching symbols.

==== apropos-information-list

<procedure>(apropos-information-list PATTERN #!key macros? qualified? sort case-insensitive? raw? base) => list</procedure>

Like {{apropos}} but returns a list key'ed by {{(MODULE . NAME)}}.

{{MODULE}} is the module symbol or {{||}} for the null module.

{{NAME}} is the base symbol.

The associated information is either:
; {{'macro}} :
; {{'keyword}} :
; {{'variable}} :
; {{'procedure}} :
; {{'(procedure . <lambda-list-specification>)}} :
; {{'(procedure . <core procedure name>)}} :

==== apropos-default-options

<parameter>(apropos-default-options [OPTIONS]) -> list</parameter>

{{OPTIONS}} is an {{apropos}} #!key argument list.

Default {{'()}}.

==== apropos-interning

<parameter>(apropos-interning [FLAG]) -> boolean</parameter>

Use {{string->symbol}} - {{#t}} - or {{string->uninterned-symbol}} - {{#f}}.

Default {{#t}}.

=== Interpreter Usage

A {{csi}} toplevel-command is added when the {{apropos}} extension is loaded:
{{,a PATTERN ARGUMENT...}}.

{{ARGUMENT}} is interpreted as:

; {{mac[ros]}} : {{#:macros? #t}}
; {{qual[ified]}} : {{#:qualified? #t}}
; {{sort [name|module|type|#f]}} : {{#:sort #:name|#:module|#:type|#f}}
; {{split [name|module|#f]}} : {{#:split #:name|#:module|#f}}
; {{case-insensitve|ci}} : {{#:case-insensitive? #t}}
; {{base 2..16}} : {{#:base 2..16}}

The command is interpreted by the {{apropos}} procedure, but sort by type is
the default.

Example:

<enscript language=scheme>
#;1> ,a print qualified macros sort name
char-set:printing                            variable
define-record-printer                        macro
flonum-print-precision                       procedure tmp
fprintf                                      procedure (port fstr . args)
max-symbol-printname-length    symbol-utils  procedure (syms)
pretty-print                                 procedure (obj . opt)
  ...
##sys#register-record-printer                procedure (type proc)
##sys#repl-print-hook                        procedure (x port)
##sys#repl-print-length-limit                variable
##sys#user-print-hook                        procedure (x readable port)
##sys#with-print-length-limit                procedure (limit thunk)
</enscript>

<enscript language=scheme>
#;1> ,a '"w.*e" macros
eval-when                                    macro
when                                         macro
bitwise-and                                  procedure xs
bitwise-ior                                  procedure xs
bitwise-not                                  procedure (x)
bitwise-xor                                  procedure xs
call-with-current-continuation               procedure (proc)
  ...
write-string                                 procedure (s . more)
char-set:lower-case                          variable
char-set:whitespace                          variable
</enscript>

<enscript language=scheme>
#;1> ,a '(: (* any)) qualified macros sort name
*                                                           procedure C_times
  ... 1600 lines + ...
##sys#write-char/port                                       procedure (c port)
</enscript>


== Usage

<enscript language=scheme>
(require-extension apropos)
</enscript>


== Examples

* YMMV

<enscript language=scheme>
> (apropos 'print)
flonum-print-precision                     procedure tmp
fprintf                                    procedure (port fstr . args)
max-symbol-printname-length  symbol-utils  procedure (syms)
pretty-print                               procedure (obj . opt)
pretty-print-expand*         expand-full   procedure (form . tmp)
pretty-print-width                         procedure args
print                                      procedure args
print*                                     procedure args
print-call-chain                           procedure tmp
print-error-message                        procedure (ex . args)
printf                                     procedure (fstr . args)
sprintf                                    procedure (fstr . args)
symbol-printname-details     symbol-utils  procedure (sym)
symbol-printname-length      symbol-utils  procedure (sym)
symbol-printname<?           symbol-utils  procedure (x y)
symbol-printname=?           symbol-utils  procedure (x y)
char-set:printing                          variable
</enscript>

<enscript language=scheme>
> (pp (apropos-list 'print))
((expand-full#pretty-print-expand*
  symbol-utils#max-symbol-printname-length
  symbol-utils#symbol-printname-length
  print-error-message
  print-call-chain
  sprintf
  symbol-utils#symbol-printname-details
  printf
  print*
  char-set:printing
  print
  fprintf
  symbol-utils#symbol-printname=?
  symbol-utils#symbol-printname<?
  pretty-print
  flonum-print-precision
  pretty-print-width))
</enscript>

<enscript language=scheme>
> (pp (apropos-information-list 'print))
((((||: . flonum-print-precision) procedure . tmp)
 ((||: . fprintf) procedure port fstr . args)
 ((symbol-utils . max-symbol-printname-length) procedure syms)
 ((||: . pretty-print) procedure obj . opt)
 ((expand-full . pretty-print-expand*) procedure form . tmp)
 ((||: . pretty-print-width) procedure . args)
 ((||: . print) procedure . args)
 ((||: . print*) procedure . args)
 ((||: . print-call-chain) procedure . tmp)
 ((||: . print-error-message) procedure ex . args)
 ((||: . printf) procedure fstr . args)
 ((||: . sprintf) procedure fstr . args)
 ((symbol-utils . symbol-printname-details) procedure sym)
 ((symbol-utils . symbol-printname-length) procedure sym)
 ((symbol-utils . symbol-printname<?) procedure x y)
 ((symbol-utils . symbol-printname=?) procedure x y)
 ((||: . char-set:printing) . variable)))
</enscript>

<enscript language=scheme>
> (apropos 'print #:qualified? #t)
printer:                                     keyword
flonum-print-precision                       procedure tmp
fprintf                                      procedure (port fstr . args)
max-symbol-printname-length    symbol-utils  procedure (syms)
pretty-print                                 procedure (obj . opt)
pretty-print-expand*           expand-full   procedure (form . tmp)
pretty-print-width                           procedure args
print                                        procedure args
print*                                       procedure args
print-call-chain                             procedure tmp
print-error-message                          procedure (ex . args)
printf                                       procedure (fstr . args)
sprintf                                      procedure (fstr . args)
symbol-printname-details       symbol-utils  procedure (sym)
symbol-printname-length        symbol-utils  procedure (sym)
symbol-printname<?             symbol-utils  procedure (x y)
symbol-printname=?             symbol-utils  procedure (x y)
##sys#print                                  procedure (x readable port)
##sys#print-length-limit                     procedure args
##sys#print-to-string                        procedure (xs)
##sys#really-print-call-chain                procedure (port chain header)
##sys#register-record-printer                procedure (type proc)
##sys#repl-print-hook                        procedure (x port)
##sys#user-print-hook                        procedure (x readable port)
##sys#with-print-length-limit                procedure (limit thunk)
char-set:printing                            variable
##sys#record-printers                        variable
##sys#repl-print-length-limit                variable
</enscript>


== Requirements

[[regex]]
[[check-errors]]
[[miscmacros]]
[[symbol-utils]]
[[string-utils]]


== Notes

* Use {{"?"}} as the {{PATT}} to list symbols containing a {{#\?}}.

<enscript language=scheme>
#;1> ,a "?"
abandoned-mutex-exception?                            procedure (x)
                                ...
zero?                                                 procedure (n)
</enscript>


== Bugs and Limitations

* Doesn't show something similar to procedure-information for macros. Requires
meta-information beyond that supplied by the transformer procedure.

* The support for macros is ''brittle'' due to subtle details of the core
macro machinary assumed by this extension. Submit a bug report if it breaks.


== Author

[[/users/kon-lovett|Kon Lovett]]


== Version history

; 2.7.2 : Expanded help text.
; 2.7.1 : Fix {{apropos-list}} unrecognized {{#:sort}}.
; 2.7.0 : Add {{apropos-default-options}}. Add {{#:split}} option.
; 2.6.5 : Fix identifier sorting.
; 2.6.4 : Cosmetic.
; 2.6.3 : '''CSI:Help'''.
; 2.6.2 : Better (some?) support for identifiers with #s.
; 2.6.1 : Added ''Notes''
; 2.6.0 : check {{,?}}.
; 2.5.2 : Fix doc.
; 2.5.1 : Add {{apropos-interning}}.
; 2.5.0 : Strip {{gensym}} contribution to identifiers (EXPERIMENTAL).
; 2.4.0 : Add {{base}}.
; 2.3.0 : Identifiers are now {{(<module> . <name>)}} for the {{information-list}}.
; 2.2.5 : Re-flow.
; 2.2.4 : Less C.
; 2.2.3 : Fix only boolean/char REPL bug, re-flow.
; 2.2.2 : Fix only number REPL bug, re-flow.
; 2.2.1 : .
; 2.2.0 : correct interpretation of unquoted string/symbol as a literal string, fix sorting support for apropos-information-list, add case-insensitive option
; 2.1.3 : fix for ticket #1211, incorrect load of regex when irregex is actual dependency
; 2.1.2 : fix for removed core support routine
; 2.1.1 : Fix for ticket #987
; 2.1.0 : Added irregex sre & POSIX string patterns.
; 2.0.0 : Chicken 4.8 only.
; 1.4.3 : Explicit {{regex}} dependency.
; 1.4.2 : Using utility libraries, rather than own routines.
; 1.4.1 : Bugfix for sorting symbols by printname. {{kind:}} is now {{type:}}.
; 1.4.0 : Macros are back but '''brittle'''
; 1.3.0 :
; 1.1.2 :
; 1.1.1 :
; 1.1.0 : Needs "check-errors" extension.
; 1.0.0 : Chicken 4 release.


== License

(The BSD License)

Copyright (C) 2009-2018 Kon Lovett.  All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the Software),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED ASIS, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
