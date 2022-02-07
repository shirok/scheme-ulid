;;
;; https://github.com/ulid/spec
;;

;; Obtain milliseconds from Unix Epoch.
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

(define *randomness-range* (expt 2 80))
(define *digits* (string->vector "0123456789ABCDEFGHJKMNPQRSTVWXYZ"))

;; API
;; ulid record
(define-record-type ulid %make-ulid ulid?
  (timestamp ulid-timestamp)
  (randomness ulid-randomness))

;; API
(define-optionals (make-ulid-generator (random-source default-random-source))
  (let ((randomness (random-source-make-integers random-source))
        (last-ts 0)
        (last-rn 0))
    (opt-lambda ((timestamp #f))
      (let* ((ts (or timestamp (milliseconds-from-epoch)))
             (rn (if (= ts last-ts)
                   (if (= last-rn (- *randomness-range* 1))
                     (error "No more ULID in the same timestamp")
                     (+ last-rn 1))
                   (randomness *randomness-range*))))
        (set! last-ts ts)
        (set! last-rn rn)
        (%make-ulid ts rn)))))

;; API
(define (ulid->u8vector ulid)
  (assume (ulid? ulid))
  (let ((uv (make-u8vector 16))
        (ts (ulid-timestamp ulid))
        (rn (ulid-randomness ulid)))
    (u8vector-set! uv  0 (arithmetic-shift ts -40))
    (u8vector-set! uv  1 (bitwise-and (arithmetic-shift ts -32) #xff))
    (u8vector-set! uv  2 (bitwise-and (arithmetic-shift ts -24) #xff))
    (u8vector-set! uv  3 (bitwise-and (arithmetic-shift ts -16) #xff))
    (u8vector-set! uv  4 (bitwise-and (arithmetic-shift ts  -8) #xff))
    (u8vector-set! uv  5 (bitwise-and ts #xff))

    (u8vector-set! uv  6 (arithmetic-shift rn -72))
    (u8vector-set! uv  7 (bitwise-and (arithmetic-shift rn -64) #xff))
    (u8vector-set! uv  8 (bitwise-and (arithmetic-shift rn -56) #xff))
    (u8vector-set! uv  9 (bitwise-and (arithmetic-shift rn -48) #xff))
    (u8vector-set! uv 10 (bitwise-and (arithmetic-shift rn -40) #xff))
    (u8vector-set! uv 11 (bitwise-and (arithmetic-shift rn -32) #xff))
    (u8vector-set! uv 12 (bitwise-and (arithmetic-shift rn -24) #xff))
    (u8vector-set! uv 13 (bitwise-and (arithmetic-shift rn -16) #xff))
    (u8vector-set! uv 14 (bitwise-and (arithmetic-shift rn  -8) #xff))
    (u8vector-set! uv 15 (bitwise-and rn #xff))
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


;; Fancier printing, if possible
(cond-expand
 (gauche
  (define-method write-object ((u ulid) port)
    (format port "#<ulid ~a>" (ulid->string u))))
 (else))
