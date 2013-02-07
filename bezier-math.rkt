#lang racket

;; TODO: write contracts and sourcedocs
;; TODO: define-inline?
(provide apply/bez
         apply/deriv
         pow
         distance)

(define-syntax-rule (pow n) (* n n))
(define-syntax-rule (inv n) (- 1.0 n))

(define-syntax-rule (distance x1 y1 x2 y2)
  (let 
      ([x (- x1 x2)]
       [y (- y1 y2)])
  (sqrt (+ (pow x) (pow y)))))

;; Quadratic Bezier curve equation
(define (bezier2 t a b c)
  (let* 
      ([one-minus-t (inv t)])
    (+ (* (pow one-minus-t) a)
       (* one-minus-t t b 2)
       (* (pow t) c))))

;; Cubic Bezier curve equation
(define (bezier3 t a b c d)
  (+ (* (inv t) (bezier2 t a b c))
     (* t       (bezier2 t b c d))))

;; First derivative of quadratic Bezier
(define (deriv2 t a b c) 
  (+ (* 2 (inv t) (- b a))
     (* 2      t  (- c b))))

;; First derivative of cubic Bezier - thx to WolframAlpha.
;;
;; This was a function at first and I sincerely hoped that it 
;; would get inlined by the compiler. I was wrong. changing
;; `define` to `define-syntax-rule` alone yielded 500% of a 
;; speed up.
(define-syntax-rule (deriv3 t a b c d)
  (let 
      ([t-minus-one (- t 1.0)])
    (* (- 3)
       (+ (* a (pow t-minus-one))
          (* b (+ (* (- 3.0) 
                     (pow t))
                  (* 4.0 t)
                  (- 1.0)))
          (* t
             (- (* 3.0 c t)
                (* 2.0 c)
                (* d t)))))))


;;
;; Wrappers accepting a list of points instead of points
;; directly. They delegated to `apply` at first but "unrolling"
;; arguments manually proved to be much more performant.
;;

(define-syntax-rule (apply/bez t pts)
  (match pts
    [(list a b c d) (bezier3 t a b c d)]))

(define-syntax-rule (apply/deriv t pts) 
  (match pts
    [(list a b c d) (deriv3 t a b c d)]))

(module+ test
  (require rackunit)

  (time (for ([_ (in-range 10000000)])
        (bezier3 0.2345 20.0 30.0 40.0 120.0)))
  
  (test-case 
   "Test basic mathematical operations"
   (check-equal? (pow 45)   2025)
   (check-equal? (inv 0.45) 0.55)
   ;; '(3 4 5) being pythagorean triple
   (check-equal? (distance 0 0 3 4) 5))
  
  (test-case 
   "Quadratic bezier tests"
   ;; if curve's control points form a line then
   ;; the curve is a line too
   (check-equal? (bezier2 0.25 0 5 10) 2.5)))

