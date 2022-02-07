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
- `(srfi 227)`  (optional arguments)
- `(srfi 227 definitions)`

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

Creates an ULID object.  You can retrieve its timestamp and randomness
fields by `ulid-timestamp` and `ulid-randomness`, both in exact
nonnegative integers, respectively.

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
