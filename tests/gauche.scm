;; Tests for Gauche

(add-load-path ".." :relative)

(use gauche.test)
(use ulid)

(test-start "ULID")
(test-module 'ulid)

(load "tests/ulid.scm")

(test-end)
