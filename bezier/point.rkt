#lang racket
;;
;; This module implements a point struct, which is used by every other
;; modules in this library. Additionaly defined here are procedures
;; for converting from and to a point struct.
;; 

(require 
 racket/performance-hint
 (only-in "math.rkt" distance))


(struct point
  (x y) 
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
  [point->list (-> point? (list/c number? number?))]
  
  [pair->point (-> (cons/c number? number?) point?)]
  [point->pair (-> point? (cons/c number? number?))]
  
  [point->complex (-> point? complex?)]
  [complex->point (-> complex? point?)]
  
  [coords->points (-> coords? (listof point?))]
  
  [rotate-point (->* (point? number?) 
                     (point? #:type (or/c 'degrees 'radians)) 
                     point?)]
  [scale-point (->* (point? number?) (point?) point?)]))


;;================================================================================

;; Point constructor

(define (make-point x y)
  (point (real->double-flonum x)
         (real->double-flonum y)))


;;================================================================================

;; Converting to and from popular representations of a point.

(define (list->point lst)
  (make-point (first lst)
              (second lst)))

(define (point->list point)
  (list (point-x point) 
        (point-y point)))


(define (pair->point pair)
  (make-point (car pair)
              (cdr pair)))

(define (point->pair points)
  (cons (point-x point) 
        (point-y point)))


(define (point->complex p)
  (make-rectangular (point-x p) 
                    (point-y p)))

(define (complex->point c)
  (make-point (real-part c) 
              (imag-part c)))


;; A convenience wrapper that dispatches to one of the above functions
;; depending on the type of it's argument.
(define (coords->points coords-seq)
  (for/list 
      ([coords coords-seq])
    (cond 
      [(point? coords) coords]
      [(list? coords) (list->point coords)]
      [(pair? coords) (pair->point coords)]
      [(complex? coords) (complex->point coords)])))


;;================================================================================

;; Additional utility functions.

(begin-encourage-inline
  
  (define (points-distance p1 p2)
    (distance (point-x p1) 
              (point-y p1)
              (point-x p2) 
              (point-y p2)))
  )


;; TODO: abstract over these two functions (somehow)
(define (rotate-point point angle 
                      [origin zero-point] 
                      #:type [angle-type 'radians])
  (define angle-converter (if (equal? angle-type 'radians) 
                              values ; acts as identity function
                              degrees->radians))
  
  (let
      ([point  (point->complex point)]
       [origin (point->complex origin)]
       [angle  (make-polar 1 (angle-converter angle))])
    (complex->point (+ origin (* angle (- point origin))))))


(define (scale-point point scale [origin zero-point])
  (let
      ([point  (point->complex point)]
       [origin (point->complex origin)]
       [scale  (make-polar scale 0)])
    
    (complex->point (+ origin (* scale (- point origin))))))


;;================================================================================

(module+ test
  (require rackunit)
  
  (test-case 
   "Test distance between points computation."
   ;; '(3 4 5) being pythagorean triple
   (check-equal? (points-distance (make-point 0 0) 
                                  (make-point 3 4)) 
                 5.0))
  )