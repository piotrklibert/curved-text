#lang racket

;; TODO: write contracts and sourcedocs
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
  ;; I have no idea if this `let` helps or hurts performance.
  ;; I have no idea why I wrote this like that either.
  (let* 
      ([one-minus-t (inv t)]
       [one-minus-t-squared (pow one-minus-t)]
       [t-squared (pow t)])
    (+ (* one-minus-t-squared a)
       (* one-minus-t t b 2)
       (* t-squared c))))

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