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
  [get-nearest-point (-> curve? number? point-and-angle?)]
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

(define (get-nearest-point curve dist)
  ;; returns a point on a curve which distance from the start is
  ;; equal (or near) dist
  (define get-angle (cut angle-at (control-points-xs (curve-controls curve))
                                  (control-points-ys (curve-controls curve)) <>))
  (let* 
      ([points     (curve-points curve)]
       [lens       (curve-cumlens curve)]
       [ts         (curve-ts curve)]
       [search-res (binarysearch lens dist)] 
       [pos        (if (first search-res) 
                       (second search-res)
                       ;; it is possible for binary-search to return -1
                       (if (> (second search-res) 0)
                           (sub1 (second search-res))
                           0))]
       [point      (vector-ref points pos)])
    
      (point-and-angle (point-x point) (point-y point)
                       (get-angle (vector-ref ts pos)))))


#|
make-lengths, previous version:
(for/fold 
      ([sums (list 0)])
      ([i (in-range 1 (vector-length points))])

    (cons (+ (car sums)
             (points-distance (vector-ref points (sub1 i)) 
                              (vector-ref points i))) 
          sums))
|#