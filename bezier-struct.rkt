#lang racket
(require srfi/26)
(require racket/draw)

(require "binary-search.rkt")
(require "bezier-math.rkt")

;; TODO: provide real contracts!
#|(provide make-curve get-letter-positions (struct-out point) (struct-out letter-pos) (struct-out bcurve) point->pair points->pairs)|#

(provide (all-defined-out))


;;
;;
;; STRUCTURE DEFINITIONS
;;
;;
(struct point  
  (x y) 
  #:transparent)

(struct bcurve 
  (controls points lengths ts angles len))

(struct letter-pos 
  (p angle)
  #:transparent)

(struct curved-letter
  (letter position angle))

(struct curved-text
  (curve word letters))

;;
;; Structs helpers
;;
(define (point->pair point)
  (list (point-x point) (point-y point)))
(define (points->pairs points)
  (map point->pair (make-controls points)))

(define (pair->point pair) 
  (apply point pair))
(define (pairs->points points)
  (map pair->point points))

;; get coordinates straight from curved-letter
(define (letter-x curved-letter)
  (point-x (curved-letter-position curved-letter)))
(define (letter-y curved-letter)
  (point-y (curved-letter-position curved-letter)))

;;
;; Math wrappers
;;
(define (point-at controls-x controls-y t)
  (point (apply/bez t controls-x)
         (apply/bez t controls-y)))

(define (angle-at controls-x controls-y t)
  (let ([x (apply/deriv t controls-x)]
        [y (apply/deriv t controls-y)])
    ;; x/y would yield a tangent angle, we need it rotated 
    ;; by 90 degrees, hence change of sign and order of coords
    (atan (- y) x)))

(define (points-distance p1 p2)
  (distance (point-x p1) (point-y p1)
            (point-x p2) (point-y p2)))


;;
;; Curve creation helpers
;;
(define (make-ts n)
  ;; returns a list of numbers in 0..1 range (inclusive)
  (let 
      ([n (if (exact? n) (exact->inexact n) n)])
    (if (n . > . 1)
        (let ([n (/ 1.0 n)])
          (range 0.0 (+ 1 n) n))
        (range 0.0 (+ 1 n) n))))

(define (make-controls points-or-pairs)
  ;; Deals with different formats in which control points can be defined
  ;; and return a list of point structs.
  (let
      ([points-or-pairs (sequence->list points-or-pairs)])
    (cond 
      [((listof list?)  points-or-pairs) (map (curry apply point) points-or-pairs)]
      [((listof point?) points-or-pairs) points-or-pairs]
      [((listof pair?)  points-or-pairs) (map (match-lambda 
                                                [(cons a b) (point a b)]) 
                                              points-or-pairs)])))

(define-syntax-rule (define-maker name kind)
  (define (name controls ts)
    (let
        ([controls-x (map point-x controls)]
         [controls-y (map point-y controls)])
      (map (cut kind controls-x controls-y <>) ts))))

(define-maker make-points point-at)
(define-maker make-angles angle-at)


(define (make-lengths points)
  (let loop ([last-point (first points)]
             [points     (rest points)]
             [sums       '(0)])
    (if (null? points) sums
        (let* ([current-point    (first points)]
               [remaining-points (rest points)]
               [last-distance    (first sums)]
               [current-distance (+ last-distance
                                    (points-distance last-point
                                                     current-point))])
          (loop current-point
                remaining-points
                (cons current-distance sums))))))

;;
;; Actual BCURVE constructor
;;
(define (make-curve controls resolution)
  (let* 
      ([ts       (make-ts resolution)]
       [controls (make-controls controls)]
       [points   (make-points controls ts)]
       [angles   (make-angles controls ts)]
       [lengths  (make-lengths points)])
    (bcurve (list->vector controls)
            (list->vector points)
            (list->vector (reverse lengths))
            (list->vector ts)
            (list->vector angles)
            (first lengths))))


;;
;; Curve methods
;;

(define (get-nearest-point curve dist)
  (let 
      ([lens   (bcurve-lengths curve)]
       [points (bcurve-points curve)]
       [angles (bcurve-angles curve)])
    (match 
        (binarysearch lens dist)
      [(list #t i) (letter-pos (vector-ref points i)
                               (vector-ref angles i))]
      [(list #f i) (letter-pos (vector-ref points (sub1 i))
                               (vector-ref angles (sub1 i)))])))


(define (get-letter-positions curve word)
  (let* 
      ([wlen             (string-length word)]
       [letter-spacing   (/ (bcurve-len curve) wlen)]
       [letter-distances (map (curry * letter-spacing) 
                              (range wlen))])
    (for/list 
        ([dist letter-distances])
      (get-nearest-point curve dist))))


;;
;; CURVED-TEXT constructor
;;
(define (make-curved-text curve-or-controls word [res 1000])
  (let ([curve (if (bcurve? curve-or-controls) 
                   curve-or-controls
                   (make-curve curve-or-controls res))])
    (make-curved-text-with-curve curve word)))
  
;; helper for transforming word into a vector of letters with positions and angles
(define (make-curved-text-with-curve curve word)
  (let 
      ([letters (for/vector ([letter word]
                             [posn   (get-letter-positions curve word)])
                  (curved-letter (string letter) 
                                 (letter-pos-p posn) 
                                 (letter-pos-angle posn)))])
    (curved-text curve word letters)))



(define (draw-curved-text curved-text dc)
  (for ([letter (curved-text-letters curved-text)])
    (let ([x     (letter-x letter)]
          [y     (letter-y letter)]
          [angle (curved-letter-angle letter)]
          [char  (curved-letter-letter letter)])
      (send dc draw-text char x y #f 0 angle))))


(module* demo #f
  ;;
  ;; Demo with Racket-gui and a little benchmark
  ;;
  (require "bezier-gui.rkt")
  (define test-control-points '((20 20) (5 200) (400 5) (550 350)))
  (define (test [word "testowytest"] 
                [control-points (points->pairs test-control-points)])
    (let* 
        ([bmp   (make-empty-bitmap)]
         [dc    (get-dc bmp)]
         [path  (new dc-path%)]
         [font  (make-font #:size 24 #:family 'roman #:weight 'bold)]
         [curved-text (time
                       (make-curved-text (make-curve control-points 1000) word))]) 
      (send dc set-smoothing 'smoothed)
      ;; draw curve
      (send/apply path move-to  (car control-points))
      (send/apply path curve-to (flatten (cdr control-points)))
      (send dc draw-path path)
      ;; draw curved text
      (send dc set-text-foreground "blue")
      (send dc set-font font)
      (draw-curved-text curved-text dc)
      ;; display
      (show-plot bmp)))
  
  (define (test-perf)
    (let 
        ([control-points (points->pairs test-control-points)])
      (time 
       (for 
           ([x (in-range 100)])
         (make-curved-text (make-curve control-points 1000) 
                           "performance-test-word")))))
  
  (provide test 
           test-perf))

;(test)
;(test "10Clouds" '((10 90) (100 15) (300 470) (340 40)))