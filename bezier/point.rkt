#lang racket
;;
;; This module implements a point struct, which is used by every other modules
;; in this library. Additionaly defined here are procedures for converting from
;; and to a point struct.
;;

(require
 racket/performance-hint
 (only-in curved-text/bezier/math distance))

(struct point (x y)
  #:transparent)

;; constants
(define zero-point (point 0.0 0.0))

;; contracts
(define coord?  (or/c point? pair? complex? list?))
(define coords? (or/c (vectorof coord?) (listof coord?)))

(provide
 coord?
 coords?

 ;; not contracted because of contracts performance overhead
 points-distance ; (-> point? point? number?)

 (contract-out
  [struct point ((x flonum?)
                 (y flonum?))]
  [make-point (-> real? real? point?)]

  [list->point (-> (list/c number? number?) point?)]
  [pair->point (-> (cons/c number? number?) point?)]
  [complex->point (-> complex? point?)]

  [point->list (-> point? (list/c number? number?))]
  [point->pair (-> point? (cons/c number? number?))]
  [point->complex (-> point? complex?)]

  [coord->point (-> coord? point?)]
  [coords->points (-> coords? (listof point?))]

  [rotate-point (->* (point? number?) (point? #:type (or/c 'degrees 'radians))
                     point?)]
  [scale-point (->* (point? number?) (point?) point?)]))


;;================================================================================

;; Point constructor

(define (make-point x y)
  (point (real->double-flonum x)
         (real->double-flonum y)))


;;================================================================================

;; Converting to and from popular representations of a point.

(define (list->point lst)  (make-point (first lst)   (second lst)))
(define (pair->point pair) (make-point (car pair)    (cdr pair)))
(define (complex->point c) (make-point (real-part c) (imag-part c)))

(define (point->list pt)    (list (point-x pt) (point-y pt)))
(define (point->pair pt)    (cons (point-x pt) (point-y pt)))
(define (point->complex pt) (make-rectangular (point-x pt) (point-y pt)))


;; A convenience wrapper that dispatches to one of the above functions depending
;; on the type of its argument.
(define (coord->point coord)
  (cond
    [(point? coord)   coord]
    [(list? coord)    (list->point coord)]
    [(pair? coord)    (pair->point coord)]
    [(complex? coord) (complex->point coord)]))

(define (coords->points coords-seq)
  (sequence->list
   (sequence-map coord->point coords-seq)))


;;================================================================================

;; Additional utility functions.

(begin-encourage-inline
  (define (points-distance p1 p2)
    (distance (point-x p1) (point-y p1)
              (point-x p2) (point-y p2))))

(define (rotate-point point rotation [origin zero-point]
                      #:type [angle-type 'radians])
  (define angle-converter (case angle-type
                            ['radians identity]
                            ['degrees degrees->radians]))
  (let ([transform (make-polar 1 (angle-converter rotation))])
    (transform-point point origin transform)))

(define (scale-point point scale [origin zero-point])
  (transform-point point origin (make-polar scale 0)))

(define (transform-point point origin transform)
  (let ([point (point->complex point)]
        [origin (point->complex origin)])
   (complex->point (+ origin (* transform (- point origin))))))

;;================================================================================

(module+ test
  (require rackunit)

  (test-case
   "Test distance between points computation."
   ;; '(3 4 5) being pythagorean triple
   (check-equal? (points-distance (make-point 0 0)
                                  (make-point 3 4))
                 5.0)))
