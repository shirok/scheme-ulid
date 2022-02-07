(define-library (ulid)
  (import (scheme base)
          (scheme vector u8)
          (scheme bitwise)
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
          ulid->string
          ulid->integer
          ulid->u8vector)
  (include "ulid.scm"))
