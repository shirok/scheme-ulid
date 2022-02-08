(define-library (ulid)
  (import (scheme base)
          (scheme char)
          (scheme comparator)
          (scheme bitwise)
          (scheme vector)
          (srfi 13)                     ;string
          (srfi 19)                     ;time & date
          (srfi 27)                     ;random
          (srfi 145))                   ;assume
  ;; srfi-227 is relatively new, so we provide alternatives.
  (cond-expand
   ((library (srfi 227))                ;optional arguments
    (import (srfi 227)
            (srfi 227 definitions)))
   (else
    ;; We only need a subset of srfi-227 (not a compatibile definition, but
    ;; this is enough for us).
    (begin
      (define-syntax define-optionals*
        (syntax-rules ()
          ((_ (name . opt-formals) . body)
           (define name (opt*-lambda opt-formals . body)))))
      (define-syntax opt*-lambda
        (syntax-rules ()
          ((_ "loop" args () () body) (begin . body))
          ((_ "loop" args (var . vars) (dfl . dfls) body)
           (let ((var (if (null? args) dfl (car args)))
                 (args (if (null? args) '() (cdr args))))
             (opt*-lambda "loop" args vars dfls body)))
          ((_ ((var default) ...) . body)
           (lambda args
             (opt*-lambda "loop" args (var ...) (default ...) body))))))
    ))
  (cond-expand
   (gauche (import (gauche base)))
   (else))
  (export make-ulid-generator
          ulid
          ulid?
          ulid-timestamp
          ulid-randomness
          ulid=?
          ulid<?
          ulid-hash
          ulid-comparator
          ulid->string
          ulid->integer
          ulid->bytevector
          string->ulid
          integer->ulid
          bytevector->ulid)
  (include "ulid-impl.scm"))
