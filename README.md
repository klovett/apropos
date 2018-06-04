
## apropos

CHICKEN Scheme Apropos API & REPL Command


### Documentation

An *apropos* facility for Chicken Scheme.

When loaded by the interpreter {{csi}} {{apropos}} provides the interpreter
command {{a}}.


#### Apropos API

##### apropos

##### apropos-list

##### apropos-information-list

##### apropos-default-options

##### apropos-interning


#### Interpreter Usage


### REPL Examples

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


### API Examples

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


### Notes

* Use {{"?"}} as the {{PATT}} to list symbols containing a {{#\?}}.

<enscript language=scheme>
#;1> ,a "?"
abandoned-mutex-exception?                            procedure (x)
                                ...
zero?                                                 procedure (n)
</enscript>


### Bugs and Limitations

* Doesn't show something similar to procedure-information for macros. Requires
meta-information beyond that supplied by the transformer procedure.

* The support for macros is *brittle* due to subtle details of the core
macro machinary assumed by this extension. Submit a bug report if it breaks.
