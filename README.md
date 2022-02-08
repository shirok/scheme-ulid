# ULID for R7RS Scheme

This is an implementation of [ULID](https://github.com/ulid/spec) in portable
R7RS Scheme.

Requires the following libraries:

- `(scheme base)`
- `(scheme bitwise)`
- `(scheme comparator)`
- `(scheme vector)`
- `(scheme vector u8)`
- `(srfi 13)`  (string)
- `(srfi 19)`  (time & date)
- `(srfi 27)`  (random numbers)
- `(srfi 145)`  (assumptions)
- `(srfi 227)`  (optional arguments) - Optional

## Usage

```
(define gen-ulid (make-ulid-generator))
```

Creates a new ULID generator.  You can pass a random source (srfi-27)
to `make-ulid-generator` to use your random source; if no randon source
is passed, `default-random-source` is used.

```
(gen-ulid)  ;⇒ #<ulid>
```

Calling the generator procedure created with `make-ulid-generator`
returns a new ULID object.  You can retrieve its timestamp and randomness
fields by `ulid-timestamp` and `ulid-randomness`, both in exact
nonnegative integers, respectively.

Note: According to ULID spec, if more than one ULID is generated within
the same millisecond timestamp, the subsequent ULIDs gets randomness
field incremented from the previous one.  There's a extremely small chance
that the randomness field overflows, in such case ULID spec says the generation
fails.  Our implementation wait for the next millisecond timestamp instead.
Theoretically this will be an issue if the caller wants to generate
close to 2^80 ULIDs per milliseconds constantly; in reality it's very unlikely.

The ULID generator can take an optional timestamp argument; it is useful
when importing existing data keeping timestamp part.

```
(define u (gen-ulid))
(ulid->string u)   ;⇒ "01FV9V6CVKT28VPQ12NTFFDSZ1"
(ulid->u8vector u) ;⇒ #u8(1 126 211 179 51 115 208 145 187 92 34 174 158 246 231 225)
(ulid->integer u)  ;⇒ 1987751186151511620525844062631552993
```

Convert ULID to a string, a u8vector, or an exact integer, respectively.

```
(string->ulid "01FV9V6CVKT28VPQ12NTFFDSZ1")
  ;⇒ #<ulid>
(u8vector->ulid '#u8(1 126 211 179 51 115 208 145 187 92 34 174 158 246 231 225))
  ;⇒ #<ulid>
(integer->ulid 1987751186151511620525844062631552993)
  ;⇒ #<ulid>
```

Convert a string, a u8vector, or an exact integer to ULID, respectively.

```
(ulid=? ulid1 ulid2)
(ulid<? ulid1 ulid2)
(ulid-hash ulid)
```

Equality predicate, ordering predicate, and hash function.

```
ulid-comparator
```

A comparator suitable for ULID.

## Testing

Run `tests/ulid.scm`.  For Gauche, you can run `tests/gauche.scm`.

## Installation

Copy `ulid.sld` and `ulid-impl.scm` to wherever your load-path can see.

## Porting

There're a couple of `cond-expand` in `ulid-impl.scm` that an implementation
may have better ways than the portable code.  Feel free to add your
implementation-specific support and send me PR.
