#lang racket


(require 
 racket/flonum
 racket/performance-hint)


(provide
 ;; not really contracted for performance, don't fix
 apply/bez   ; (flonum? (vector/c flonum? flonum? flonum? flonum?) . -> . flonum?)
 apply/deriv ; (flonum? (vector/c flonum? flonum? flonum? flonum?) . -> . flonum?)
 distance    ; (flonum? flonum? flonum? flonum? . -> . flonum? )
 )

;;================================================================================

;; Everything defined inside a begin-encourage-inline should
;; be inlined, even when called from outside of this module.
(begin-encourage-inline

  ;; I have no idea why is that, but when I place this function in 
  ;; the bezier-point  module (it's used only there) then it's 
  ;; performance drops significantly...
  ;; TODO: figure out what is happenning.
  (define (distance x1 y1 x2 y2)
    (let 
        ([x (fl- x1 x2)]
         [y (fl- y1 y2)])
      (flsqrt (fl+ (fl* x x) 
                   (fl* y y))))))


(begin-encourage-inline
    
  ;; Early, non-optimised functions for computing bezier curve points and 
  ;; angles (derivative). They are used in unit tests where their results 
  ;; are compared with newer, optimised ones.
  (define-syntax-rule (square n) (* n n))
  
  (define (naive-bezier^2 t a b c)
    (let* ([one-minus-t (- 1 t)])
      (+ (* (square one-minus-t) a) (* one-minus-t t b 2) (* (square t) c))))
  
  (define (naive-bezier^3 t a b c d)
    (+ (* (- 1 t) (naive-bezier^2 t a b c))
       (* t       (naive-bezier^2 t b c d))))

  (define (naive-deriv^2 t a b c) 
    (+ (* 2 (- 1 t) (- b a))
       (* 2      t  (- c b))))
  
  (define (naive-deriv^3 t a b c d)
    (let 
        ([t-minus-one (- t 1.0)])
      (* (- 3) (+ (* a (square t-minus-one)) (* b (+ (* (- 3.0) (square t)) (* 4.0 t) (- 1.0)))
                  (* t (- (* 3.0 c t) (* 2.0 c) (* d t)))))))  
  
  
  ;;================================================================================
  
  ;; Optimised functions for bezier curve computations. These are actually used and 
  ;; exported.
  
  (define (bezier^2 t a b c)
    (define 1-t (fl- 1.0 t))
    (fl+ (fl+ (fl* (fl* 1-t 1-t) a) (fl* (fl* 1-t t) (fl* b 2.0))) (fl* (fl* t t) c)))  
  
  (define (bezier^3 t a b c d)
    (let*
        ([1-t   (fl- 1.0 t)]
         [1-t^2 (fl* 1-t 1-t)]
         [t^2   (fl* t t)])
      
      (fl+ (fl* 1-t (fl+ (fl+ (fl* 1-t^2 a) (fl* (fl* 1-t t) (fl* b 2.0))) (fl* t^2 c)))
           (fl* t (fl+ (fl+ (fl* 1-t^2 b) (fl* (fl* 1-t t) (fl* c 2.0))) (fl* t^2 d))))))
  
  (define (deriv^3 t a b c d)
    (let 
        ([t-minus-1 (fl- t 1.0)])
      (fl* -3.0
           (fl+ (fl+ (fl* a (fl* t-minus-1 t-minus-1))
                     (fl* b (fl- (fl+ (fl* -3.0 (fl* t t)) (fl* 4.0 t)) 1.0)))
                (fl* t (fl- (fl- (fl* 3.0 (fl* c t)) (fl* 2.0 c)) (fl* d t)))))))  

  
  ;;================================================================================
  
  ;; Wrappers accepting a vector of points instead of points directly,
  ;; because the points are usually stored in a vector. 
  
  (define-syntax-rule (nth vec pos) (vector-ref vec pos))
  
  (define (apply/bez t pts)
    (bezier^3 t 
              (nth pts 0) 
              (nth pts 1) 
              (nth pts 2) 
              (nth pts 3)))
  
  (define (apply/deriv t pts) 
    (deriv^3 t 
             (nth pts 0) 
             (nth pts 1) 
             (nth pts 2) 
             (nth pts 3)))
  )


(module+ test
  (require rackunit)

  (test-case 
   "Test basic mathematical operations"
   (check-equal? (square 45)   2025)
   ;; '(3 4 5) being pythagorean triple
   (check-equal? (distance 0.0 0.0 3.0 4.0) 5.0))
  
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
  
  (test-case
   "Naive and optimized bezier derivative"
   (for 
       ([t (in-range 0 1 0.001)])
     (let 
         ([t (real->double-flonum t)])
       #;(displayln (format "~a    ~a" 
                          (exact->inexact (deriv^3 t 20.0 30.0 40.0 120.0))
                          (exact->inexact (naive-deriv^3 t 20.0 30.0 40.0 120.0))))
       ;; TODO: why do they differ by such a tiny value? Are they of different types 
       ;; like flonum vs. single flonum or something?
       (check-= (exact->inexact (deriv^3 t 20.0 30.0 40.0 120.0))
                (exact->inexact (naive-deriv^3 t 20.0 30.0 40.0 120.0))
                0.0001))))
  
  
  (displayln "Computing bezier^3 10000000 times took:")
  (time (for ([_ (in-range 10000000)])
          (bezier^3 0.2345 20.0 30.0 40.0 120.0)))
  
  (displayln "Computing naive-bezier^3 1000000 times took:")
  (time (for ([_ (in-range 1000000)])
          (naive-bezier^3 0.2345 20.0 30.0 40.0 120.0)))
  
  (displayln "Computing deriv^3 10000000 times took:")
  (time (for ([_ (in-range 10000000)])
          (deriv^3 0.2345 20.0 30.0 40.0 120.0)))
  
  (displayln "Computing naive-deriv^3 10000000 times took:")
  (time (for ([_ (in-range 10000000)])
          (naive-deriv^3 0.2345 20.0 30.0 40.0 120.0)))
  )


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