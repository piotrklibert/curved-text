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
 "bezier-point.rkt"
 "bezier-math.rkt"
 "binary-search.rkt")


(struct point-and-angle point
  (angle)
  #:transparent)

(struct control-points
  (points xs ys))

(struct curve
  (controls points cumlens ts len))


(provide
 (struct-out point-and-angle)
 (contract-out
  [struct control-points ((points (vectorof point?))
                          (xs (vectorof flonum?))
                          (ys (vectorof flonum?)))]
  [make-control-points (-> (listof point?) control-points?)]
  
  [struct curve ((controls control-points?)
                 (points   (vectorof point?))
                 (cumlens  (vectorof number?))
                 (ts       (vectorof number?))
                 (len      number?))]
  
  [make-curve (-> coords? number? curve?)]
  [get-nearest-point (-> curve? number? point-and-angle?)]
  ))



(define (make-control-points points)
  (let ([points (list->vector points)])
    (control-points points
                    (vector-map (compose real->double-flonum point-x) points)
                    (vector-map (compose real->double-flonum point-y) points))))


;;
;; Math wrappers.
;;
(define (point-at controls-x controls-y t)
  (point (apply/bez t controls-x)
         (apply/bez t controls-y)))

(define (angle-at controls-x controls-y t)
  (let ([x (apply/deriv t controls-x)]
        [y (apply/deriv t controls-y)])
    ;; x/y would yield a tangent angle, we need it rotated 
    ;; by 90 degrees, hence the change of sign and order of coords
    (atan (- y) x)))


;;
;; Curve creation helpers
;;
(define (make-ts n)
  ;; returns a vector of numbers in 0..1 range (inclusive)
  (let* 
      ([n (if (exact? n) (exact->inexact n) n)]
       [n (if (> n 1) (/ 1.0 n) n)])
    (list->vector (range 0.0 (+ 1 n) n))))

(define (make-points controls ts)
  (vector-map (cut point-at (control-points-xs controls) 
                            (control-points-ys controls) <>) ts))

(define (make-lengths points)
  ;; TODO: we can do this in even more imperative way (for/vector ...)
  (for/fold 
      ([sums (list 0)])
      ([i (in-range 1 (vector-length points))])

    (cons (+ (car sums)
             (points-distance (vector-ref points (sub1 i)) 
                              (vector-ref points i))) 
          sums)))

;;
;; Actual curve constructor
;;
(define (make-curve controls resolution)
  (let* 
      ([ts       (make-ts resolution)]
       [controls (make-control-points (coords->points controls))]
       [points   (make-points controls ts)]
       [lengths  (make-lengths points)]
       [vlens    (list->vector (reverse lengths))])
    
    (curve controls points vlens ts (first lengths))))



;;
;; Curve methods
;;

(define (get-nearest-point curve dist)
  (let 
      ([get-angle (curry angle-at (control-points-xs (curve-controls curve))
                                  (control-points-ys (curve-controls curve)))]
       [points     (curve-points curve)]
       [lens       (curve-cumlens curve)]
       [ts         (curve-ts curve)])
    (match-let*
        ([(list found? pos) (binarysearch lens dist)]
         [pos   (if found? pos (sub1 pos))]
         [point (vector-ref points pos)])
      (point-and-angle (point-x point)
                       (point-y point)
                       (get-angle (vector-ref ts pos))))))
