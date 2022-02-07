(import (scheme base))
(import (ulid))
(import (srfi 78))

(define g (make-ulid-generator))

(check (ulid? (g)) => #t)

(let ((u (g)))
  (check (integer? (ulid-timestamp u)) => #t)
  (check (integer? (ulid-randomness u)) => #t)
  )
