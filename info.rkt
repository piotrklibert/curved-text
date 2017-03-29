#lang info

(define collection "curved-text")
(define deps '("base" "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
;; (define scribblings '(("scribblings/curved-text.scrbl" ())))
(define pkg-desc
  "A simple library for drawing text along the (cubic) Bezier curves.")
(define version "0.0")
(define pkg-authors '(PiotrKlibert))
