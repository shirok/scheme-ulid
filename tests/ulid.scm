(import (scheme base))
(import (ulid))
(import (srfi 78))                      ;check

(import (scheme vector u8))

(define g (make-ulid-generator))

(check (ulid? (g)) => #t)

(let ((u (g)))
  (check (integer? (ulid-timestamp u)) => #t)
  (check (integer? (ulid-randomness u)) => #t)

  (let ((v (ulid->u8vector u)))
    (check (u8vector? v) => #t)
    (check (u8vector-length v) => 16)
    (check (ulid=? u (u8vector->ulid v)) => #t))

  (let ((s (ulid->string u)))
    (check (string? s) => #t)
    (check (string-length s) => 26)
    (check (ulid=? u (string->ulid s)) => #t))

  (let ((n (ulid->integer u)))
    (check (integer? n) => #t)
    (check (< 0 n (expt 2 128)) => #t)
    (check (ulid=? u (integer->ulid n)) => #t))

  ;; uniqueness
  (let ((us (list (g) (g))))
    (check (ulid=? (car us) (cadr us)) => #f)
    (check (ulid<? (car us) (cadr us)) => #t)
    (check (ulid<? (cadr us) (car us)) => #f))
  )
