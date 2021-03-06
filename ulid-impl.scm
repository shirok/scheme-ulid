;;
;; ULID library for R7RS Scheme
;;
;; https://github.com/ulid/spec
;;
;; Copyright 2022 by Shiro Kawai <shiro@acm.org>

;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.


;; Obtain milliseconds from Unix Epoch.
;; NB: If srfi-170 (POSIX API) becomes widespread, we may use posix-time
;; portably.
(cond-expand
 (gauche
  (define (milliseconds-from-epoch)
    (receive (secs usecs) (sys-gettimeofday)
      (+ (* secs 1000) (quotient usecs 1000)))))
 (else
  ;; NB: We assume current-date has enough resolution
  (define milliseconds-from-epoch
    (let ((epoch (date->time-utc (make-date 0 0 0 0 1 1 1970 0))))
      (lambda ()
        (let ((dt (time-difference (current-time time-utc) epoch)))
          (+ (* (time-second dt) 1000)
             (quotient (time-nanosecond dt) 1000000))))))))

;; Wait for a bit.
;; If the implementation have system interface to wait around 0.5ms,
;; use that.    Otherwise, we have no choice but spend time with wasteful
;; calculation and busy loop.  RI is a random integer generator so
;; call it to kill some time.  This routine is called in extremely
;; rare event.
(cond-expand
 (gauche
  (define (wait-a-bit ri) (sys-nanosleep 500000)))
 (else
  (define (wait-a-bit ri)
    (do ((n 0 (+ n 1)))
        ((= n 10))
      (ri *randomness-range*)))))

(define *randomness-range* (expt 2 80))
(define *digits* (string->vector "0123456789ABCDEFGHJKMNPQRSTVWXYZ"))

;; API
;; ulid record
(define-record-type ulid %make-ulid ulid?
  (timestamp ulid-timestamp)
  (randomness ulid-randomness))

;; API
;; NB: The ULID spec says ULID generation may fail if randomness is very close
;; to 2^80 and lots of ULIDs are generated in the same millisecond, causing
;; randomness field to overflow.  We can, however, wait just one millisecond
;; to overcome that situation.
(define-optionals* (make-ulid-generator (random-source default-random-source))
  (let ((randomness (random-source-make-integers random-source))
        (last-ts 0)
        (last-rn 0))
    (define (make-it ts rn)
      (set! last-ts ts)
      (set! last-rn rn)
      (%make-ulid ts rn))
    (opt*-lambda ((timestamp #f))
      (let retry ()
        (let ((ts (or timestamp (milliseconds-from-epoch))))
          (if (not (= ts last-ts))
            (make-it ts (randomness *randomness-range*))
            (if (< last-rn (- *randomness-range* 1))
              (make-it ts (+ last-rn 1))
              (begin
                (wait-a-bit randomness)
                (retry)))))))))

;; API
(define (ulid=? u1 u2)
  (assume (ulid? u1))
  (assume (ulid? u2))
  (and (= (ulid-timestamp u1) (ulid-timestamp u2))
       (= (ulid-randomness u1) (ulid-randomness u2))))

;; API
(define (ulid<? u1 u2)
  (assume (ulid? u1))
  (assume (ulid? u2))
  (or (< (ulid-timestamp u1) (ulid-timestamp u2))
      (and (= (ulid-timestamp u1) (ulid-timestamp u2))
           (< (ulid-randomness u1) (ulid-randomness u2)))))

;; API
(define (ulid-hash u)
  (bitwise-xor (default-hash (ulid-timestamp u))
               (default-hash (ulid-randomness u))))

;; API
(define ulid-comparator
  (make-comparator ulid? ulid=? ulid<? ulid-hash))

;; API
(define (ulid->bytevector ulid)
  (assume (ulid? ulid))
  (let ((uv (make-bytevector 16))
        (ts (ulid-timestamp ulid))
        (rn (ulid-randomness ulid)))
    (bytevector-u8-set! uv  0 (arithmetic-shift ts -40))
    (bytevector-u8-set! uv  1 (bitwise-and (arithmetic-shift ts -32) #xff))
    (bytevector-u8-set! uv  2 (bitwise-and (arithmetic-shift ts -24) #xff))
    (bytevector-u8-set! uv  3 (bitwise-and (arithmetic-shift ts -16) #xff))
    (bytevector-u8-set! uv  4 (bitwise-and (arithmetic-shift ts  -8) #xff))
    (bytevector-u8-set! uv  5 (bitwise-and ts #xff))

    (bytevector-u8-set! uv  6 (arithmetic-shift rn -72))
    (bytevector-u8-set! uv  7 (bitwise-and (arithmetic-shift rn -64) #xff))
    (bytevector-u8-set! uv  8 (bitwise-and (arithmetic-shift rn -56) #xff))
    (bytevector-u8-set! uv  9 (bitwise-and (arithmetic-shift rn -48) #xff))
    (bytevector-u8-set! uv 10 (bitwise-and (arithmetic-shift rn -40) #xff))
    (bytevector-u8-set! uv 11 (bitwise-and (arithmetic-shift rn -32) #xff))
    (bytevector-u8-set! uv 12 (bitwise-and (arithmetic-shift rn -24) #xff))
    (bytevector-u8-set! uv 13 (bitwise-and (arithmetic-shift rn -16) #xff))
    (bytevector-u8-set! uv 14 (bitwise-and (arithmetic-shift rn  -8) #xff))
    (bytevector-u8-set! uv 15 (bitwise-and rn #xff))
    uv))

;; API
(define (ulid->integer ulid)
  (+ (arithmetic-shift (ulid-timestamp ulid) 80)
     (ulid-randomness ulid)))

;; API
(define (ulid->string ulid)
  (do ((i  0 (+ i 1))
       (v  (ulid->integer ulid) (arithmetic-shift v -5))
       (cs '() (cons (vector-ref *digits* (bitwise-and v #x1f)) cs)))
      ((= i 26) (list->string cs))))


;; API
(define (bytevector->ulid uv)
  (define (get-uint start size)
    (do ((i start (+ i 1))
         (size size (- size 1))
         (v 0 (+ (arithmetic-shift v 8) (bytevector-u8-ref uv i))))
        ((zero? size) v)))
  (assume (and (bytevector? uv) (= (bytevector-length uv) 16)))
  (%make-ulid (get-uint 0 6) (get-uint 6 10)))

;; API
(define (integer->ulid n)
  (assume (and (exact? n) (positive? n) (< n (expt 2 128))))
  (%make-ulid (arithmetic-shift n -80)
              (bitwise-and n (- (expt 2 80) 1))))

;; API
(define (string->ulid s)
  (assume (and (string? s) (= (string-length s) 26)))
  (let ((val
         (string-fold (lambda (ch n)
                        (let ((d (vector-index (lambda (c) (char-ci=? ch c))
                                               *digits*)))
                          (unless d
                            (error "Invalid character as ULID:" ch))
                          (+ (arithmetic-shift n 5) d)))
                      0 s)))
    (when (>= val (expt 2 128))
      (error "ULID string representation out of range:" s))
    (integer->ulid val)))

;; Fancier printing, if possible
(cond-expand
 (gauche
  (define-method write-object ((u ulid) port)
    (format port "#<ulid ~a>" (ulid->string u))))
 (else))
