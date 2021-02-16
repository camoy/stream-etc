#lang info

;; General

(define collection "stream-etc")
(define pkg-desc "Miscellaneous stream operations.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/stream-etc.scrbl" ())))

;; Dependencies

(define deps
  '("base"))

(define build-deps
  '("chk-lib"
    "sandbox-lib"
    "threading-doc"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"
    ))
