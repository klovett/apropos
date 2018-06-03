
(define EGG-NAME "apropos")

;chicken-install invokes as "<csi> -s run.scm <eggnam> <eggdir>"

(use files)

;no -disable-interrupts
(define *csc-options* "-inline-global -scrutinize -optimize-leaf-routines -local -inline -specialize -unsafe -no-trace -no-lambda-info -clustering -lfa2")

(define *args* (argv))

(define (test-name #!optional (eggnam EGG-NAME))
  (string-append eggnam "-test") )

(define (egg-name #!optional (def EGG-NAME))
  (cond
    ((<= 4 (length *args*))
      (cadddr *args*) )
    (def
      def )
    (else
      (error 'test "cannot determine egg-name") ) ) )

;;;

(set! EGG-NAME (egg-name))

(define (run-test #!optional (eggnam EGG-NAME) (cscopts *csc-options*))
  (let ((tstnam (test-name eggnam)))
    (print "*** csi ***")
    (system (string-append "csi -s " (make-pathname #f tstnam "scm")))
    (newline)
    (print "*** csc (" cscopts ") ***")
    (system (string-append "csc" " " cscopts " " (make-pathname #f tstnam "scm")))
    (system (make-pathname (cond-expand (unix "./") (else #f)) tstnam)) ) )

(define (run-tests eggnams #!optional (cscopts *csc-options*))
  (for-each (cut run-test <> cscopts) eggnams) )

;;;

(run-test)
