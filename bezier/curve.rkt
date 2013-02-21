#lang racket

;;
;; A structure that describes cubic (for now) Bezier curve
;; and some of the operations that can be performed on a curve.
;; 
;; The curve provides means of retrieving points that belong to
;; the curve and of calculating the tangent angle at given point.
;;

(require
 srfi/26
 "point.rkt"
 "math.rkt"
 "binary-search.rkt")


;;================================================================================

(struct point-and-angle point
  (angle)
  #:transparent)

(struct control-points
  (points xs ys))

(struct curve
  (controls points cumlens ts len))


;;================================================================================

(provide
 (struct-out point-and-angle)
 (contract-out
  [struct control-points ((points (vectorof point?))
                          (xs     (vectorof flonum?))
                          (ys     (vectorof flonum?)))]
  [make-control-points (-> (listof point?) control-points?)]
  
  [struct curve ((controls control-points?)
                 (points   (vectorof point?))
                 (cumlens  (vectorof number?))
                 (ts       (vectorof number?))
                 (len      number?))]
  
  [make-curve (-> coords? number? curve?)]
  [get-point-at-dist (-> curve? number? point-and-angle?)]
  ))


;;================================================================================

;; Math wrappers

(define (point-at controls-x controls-y t)
  (make-point (apply/bez t controls-x)
              (apply/bez t controls-y)))

(define (angle-at controls-x controls-y t)
  (let ([x (apply/deriv t controls-x)]
        [y (apply/deriv t controls-y)])
    ;; x/y would yield a tangent angle, we need it rotated 
    ;; by 90 degrees, hence the change of sign and order of coords
    (atan (- y) x)))


;;================================================================================

(define (make-control-points points)
  (let ([points (list->vector points)])
    (control-points points
                    (vector-map (compose real->double-flonum point-x) points)
                    (vector-map (compose real->double-flonum point-y) points))))


(define (make-curve controls resolution)
  (let* 
      ([ts       (make-ts resolution)]
       [controls (make-control-points (coords->points controls))]
       [points   (make-points controls ts)]
       [lengths  (make-lengths points)]
       [length   (vector-ref lengths (sub1 (vector-length lengths)))])
    
    (curve controls points lengths ts length)))


;;================================================================================

;; Curve creation helpers

(define (make-ts n)
  ;; returns a vector of numbers in 0..1 range (inclusive)
  ;; n can be either < 1 and then specifies a step or > 1
  ;; and then spcifies how many numbers to generate
  (let* 
      ([n (if (exact? n) (exact->inexact n) n)]
       [n (if (> n 1) (/ 1.0 n) n)])
    (list->vector (range 0.0 (+ 1 n) n))))


(define (make-points controls ts)
  ;; returns a vector of points that lie on a curve
  ;; described with given control points
  (define xs (control-points-xs controls))
  (define ys (control-points-ys controls))
  
  (vector-map (cut point-at xs ys <>) ts))


(define (make-lengths points)
  ;; returns a vector of cumulative sums of curve lengths
  (define sum (box 0))
  (define cumsums (for/vector
                      ([i (in-range 1 (vector-length points))])
                    (let
                        ([current-dist (+ (unbox sum)
                                          (dist-helper points i))])
                      (set-box! sum current-dist)
                      current-dist)))
  ;; insert the first distance so that resulting vector 
  ;; length is equal to length of points vector
  (vector-append #(0) cumsums))

(define (dist-helper vec i)
  ;; get distance between i-th point and the previous one
  (points-distance (vector-ref vec (sub1 i)) 
                   (vector-ref vec i)))


;;================================================================================

;; Other curve methods

(define (get-point-at-dist curve dist)
  ;; returns a point on a curve which distance from the start is
  ;; equal (or near) dist
  (define get-angle (cut angle-at (control-points-xs (curve-controls curve))
                                  (control-points-ys (curve-controls curve)) <>))
  (define search-res (binarysearch (curve-cumlens curve) dist))
  ;; binary search can return -1 as position, we need to adjust the
  ;; value if it happens
  (define pos (if (and (car search-res)
                       (< (second search-res) 0))
                  0
                  (second search-res)))
  (point-and-angle
   (point-x (vector-ref (curve-points curve) pos)) 
   (point-y (vector-ref (curve-points curve) pos))
   (get-angle (vector-ref (curve-ts curve) pos))))


;;
;; compute-length below is a translation of a "Gauss quadrature"
;; method of computing the length of a curve without the need to
;; "flatten" (chop into lots of small lines) it. I saw it first
;; described and implemented in Processing here: http://processingjs.nihongoresources.com/bezierinfo/#intoffsets_gss
;; (with the source here: http://processingjs.nihongoresources.com/bezierinfo/sketchsource.php?sketch=cubicGaussQuadrature)
;; and decided to implement it, even though I still need to flatten
;; the curve to make "arc-length parameterization".
;;
;; This algorithm relies on a table of precomputed values, 
;; which I copied to tables.rkt and exported from there.
;;
;; This is not done yet - it needs to be optimized!
;;

(require "tables.rkt")

(define (compute-length z n x1 y1 x2 y2 x3 y3 x4 y4)
  (define z2 (/ z 2.0))
  (* z2 (for/fold 
            ([sum 0])
            ([i (in-range n)]
             [tval (vector-ref tvals n)]
             [cval (vector-ref cvals n)])
          (define corrected_t (+ (* z2 tval) z2))
          (+ sum (* cval (cubicF corrected_t x1 y1 x2 y2 x3 y3 x4 y4))))))

(define (cubicF t x1 y1 x2 y2 x3 y3 x4 y4)
  (define xbase (base3 t x1 x2 x3 x4))
  (define ybase (base3 t y1 y2 y3 y4))
  (sqrt (+ (* xbase xbase)
           (* ybase ybase))))

(define (base3 t p1 p2 p3 p4)
  (define t1 (+ (* -3 p1) 
                (*  9 p2) 
                (- (* 9 p3)) 
                (* 3 p4)))
  (define t2 (+ (* t t1)
                (* 6 p1)
                (- (* 12 p2))
                (* 6 p3)))
  (+ (* t t2)
     (- (* 3 p1))
     (* 3 p2)))

(module+ test
  (require rackunit)
  
  (define-syntax-rule (flat-points curve)
    (let 
        ([pts (vector->list (control-points-points (curve-controls curve)))])
      (flatten (map point->list pts))))
  
  (define curve (make-curve '((40 40) (5 200) (400 5) (550 350)) 200))
  
  (check-= (apply compute-length 1.0 10 (flat-points curve)) (curve-len curve) 0.01))
