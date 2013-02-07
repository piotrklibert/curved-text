#lang racket
;;
;; This module implements a point struct, which is used by every other
;; modules in this library. Additionaly defined here are procedures
;; for converting from and to a point struct.
;; 


(struct point
  (x y) 
  #:transparent)


;; constants
(define zero-point (point 0 0))


(define coord?  (or/c point? pair? complex? list?))
(define coords? (or/c (vectorof coord?) (listof coord?)))

(provide
 coord?
 coords?
 (contract-out 
  [struct point ((x number?)
                 (y number?))]
  
  [list->point (-> (list/c number? number?) point?)]
  [point->list (-> point? (list/c number? number?))]
  
  [pair->point (-> (cons/c number? number?) point?)]
  [point->pair (-> point? (cons/c number? number?))]
  
  [point->complex (-> point? complex?)]
  [complex->point (-> complex? point?)]
  
  [coords->points (-> coords? (listof point?))]
  
  [points-distance (-> point? point? number?)]
  [rotate-point (->* (point? number?) 
                     (point? #:type (or/c 'degrees 'radians)) 
                     point?)]
  ))


(require 
 ;; we need this because we want to provide a version of 
 ;; distance procedure that operates on points
 (only-in "bezier-math.rkt" distance))


;;
;; Converting to and from popular representations of a point.
;;
(define (list->point lst)
  (point (first lst)
         (second lst)))

(define (point->list point)
  (list (point-x point) 
        (point-y point)))


(define (pair->point pair)
  (point (car pair)
         (cdr pair)))

(define (point->pair points)
  (cons (point-x point) 
        (point-y point)))


(define (point->complex p)
  (make-rectangular (point-x p) 
                    (point-y p)))

(define (complex->point c)
  (point (real-part c) 
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


;;
;; Additional utility functions.
;;
(define (points-distance p1 p2)
  (distance (point-x p1) (point-y p1)
            (point-x p2) (point-y p2)))


(define (rotate-point point angle [origin zero-point] #:type [angle-type 'radians])
  (define angle-converter (if (equal? angle-type 'radians) values degrees->radians))
  (let
      ([point  (point->complex point)]
       [origin (point->complex origin)]
       [angle  (make-polar 1 (angle-converter angle))])
    (complex->point (+ origin (* angle (- point origin))))))
