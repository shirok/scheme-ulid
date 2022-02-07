(define-library (ulid)
  (import (scheme base)
          (scheme char)
          (scheme comparator)
          (scheme bitwise)
          (scheme vector)
          (scheme vector u8)
          (srfi 13)                     ;string
          (srfi 19)                     ;time & date
          (srfi 27)                     ;random
          (srfi 145)                    ;assume
          (srfi 227)                    ;optional arguments
          (srfi 227 definitions))
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
          ulid->u8vector
          string->ulid
          integer->ulid
          u8vector->ulid)
  (include "ulid-impl.scm"))
