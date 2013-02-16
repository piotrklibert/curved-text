#lang racket

;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO
;;
;; *** Use Typed Racket for this module! ***
;;
;; Because: I wanted to use contracts for procedures defined 
;; here but it turns out contracts come with non-trivial
;; overhead (2x slower execution). Typed Racket will let me 
;; get the benefits of contracts without runtime overhead.
;;
;; TODO TODO TODO TODO TODO TODO TODO TODO TODO TODO

(require racket/flonum)

;; TODO: define-inline?
(provide 
 apply/bez 
 apply/deriv
 distance)

(define-syntax-rule (square n) (* n n))
(define-syntax-rule (inv n) (- 1.0 n))

(define-syntax-rule (distance x1 y1 x2 y2)
  (let 
      ([x (- x1 x2)]
       [y (- y1 y2)])
  (sqrt (+ (square x) 
           (square y)))))


;;
;; Quadratic and cubic bezier formulas, naive version
;; (without any optimizations).
;;

(define (naive-bezier^2 t a b c)
  (let* 
      ([one-minus-t (inv t)])
    (+ (* (square one-minus-t) a)
       (* one-minus-t t b 2)
       (* (square t) c))))

(define (naive-bezier^3 t a b c d)
  (+ (* (inv t) (naive-bezier^2 t a b c))
     (* t       (naive-bezier^2 t b c d))))


;;
;; Quadratic and cubic bezier formulas, optimized through
;; flonum specific functions (about 10x faster).
;;

(define (bezier^2 t a b c)
  (define 1-t (fl- 1.0 t))
  (fl+ (fl+ (fl* (fl* 1-t 1-t) a) (fl* (fl* 1-t t) (fl* b 2.0))) (fl* (fl* t t) c)))

(define (bezier^3 t a b c d)
  (define 1-t   (fl- 1.0 t))
  (define 1-t^2 (fl* 1-t 1-t))
  (define t^2   (fl* t t))

  (fl+ (fl* 1-t (fl+ (fl+ (fl* 1-t^2 a) (fl* (fl* 1-t t) (fl* b 2.0))) (fl* t^2 c)))
       (fl* t (fl+ (fl+ (fl* 1-t^2 b) (fl* (fl* 1-t t) (fl* c 2.0))) (fl* t^2 d)))))





;; First derivative of quadratic Bezier
(define (deriv2 t a b c) 
  (+ (* 2 (inv t) (- b a))
     (* 2      t  (- c b))))

;; First derivative of cubic Bezier - thx to WolframAlpha.
;;
;; TODO: optimization similar to the one in bezier^3
;; TODO: maybe a macro is not neccessary after all?
(define-syntax-rule (deriv3 t a b c d)
  (let 
      ([t-minus-one (- t 1.0)])
    (* (- 3)
       (+ (* a (square t-minus-one))
          (* b (+ (* (- 3.0) 
                     (square t))
                  (* 4.0 t)
                  (- 1.0)))
          (* t
             (- (* 3.0 c t)
                (* 2.0 c)
                (* d t)))))))


;;
;; Wrappers accepting a sequence of points instead of points directly,
;; because the points are usually stored in a sequence of some kind. 
;;
;; WARNING: work in progress, changing sequence type from list to vector.
;; Take a look at the end of this file for rationale.
;;
;; They delegated to `apply` at first but "unrolling" arguments manually 
;; proved to be much more performant.
;;
(define-syntax-rule (nth vec pos) 
  ;; just to shorten next two definitions :)
  (vector-ref vec pos))

(define (apply/bez t pts)
  (bezier^3 t (nth pts 0) (nth pts 1) (nth pts 2) (nth pts 3)))

(define-syntax-rule (apply/deriv t pts) 
  (deriv3 t (nth pts 0) (nth pts 1) (nth pts 2) (nth pts 3)))



(module+ test
  (require rackunit)

  (test-case 
   "Test basic mathematical operations"
   (check-equal? (square 45)   2025)
   (check-equal? (inv 0.45) 0.55)
   ;; '(3 4 5) being pythagorean triple
   (check-equal? (distance 0 0 3 4) 5))
  
  (test-case 
   "bezier^3 degenerate case"
   (check-equal? (bezier^2 0.25 0.0 5.0 10.0) 2.5))
  
  (test-case 
   "bezier^3 degenerate case"
   (check-equal? (bezier^3 0.5 0.0 5.0 10.0 15.0) 7.5))
  
  (test-case
   "Are bezier^2 results the same as naive-bezier^2?"
   (for 
       ([t (in-range 0 1 0.001)])
     (let 
         ([t (real->double-flonum t)])
       (check-equal? (inexact->exact (bezier^2 t 20.0 30.0 40.0 ))
                     (inexact->exact (naive-bezier^2 t 20.0 30.0 40.0 ))))))
  
  (test-case
   "Are bezier^3 results the same as naive-bezier^3?"
   (for 
       ([t (in-range 0 1 0.001)])
     (let 
         ([t (real->double-flonum t)])
       (check-equal? (inexact->exact (bezier^3 t 20.0 30.0 40.0 120.0))
                     (inexact->exact (naive-bezier^3 t 20.0 30.0 40.0 120.0))))))
  
  (displayln "Computing bezier^3 10000000 times took:")
  (time (for ([_ (in-range 10000000)])
          (bezier^3 0.2345 20.0 30.0 40.0 120.0))))


#|
Using MATCH is not the best idea and using list instead of vector is similarly bad.
Here's why:

(define l (list 4 5 6 7))
(define v (vector 4 5 6 7))

(define (t1 l)
  (match l
    [(list a b c d) 
     (values a b c d)]))

(define (t2 l)
  (values (list-ref l 0)
          (list-ref l 1)
          (list-ref l 2)
          (list-ref l 3)))

(define (t3 v)
  (values (vector-ref v 0)
          (vector-ref v 1)
          (vector-ref v 2)
          (vector-ref v 3)))

(time (for ([_ (in-range 10000000)])
          (t1 l)))
(time (for ([_ (in-range 10000000)])
          (t2 l)))
(time (for ([_ (in-range 10000000)])
          (t3 v)))
|#