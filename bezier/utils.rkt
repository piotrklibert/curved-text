#lang racket

(require
 racket/gui
 (only-in "point.rkt" coord?))

(provide

 ;; GUI helpers
 (contract-out
  [show-plot (-> (is-a?/c bitmap%) void?)]
  [make-empty-bitmap (->* () (number? number?) (is-a?/c bitmap%))]
  [get-dc (-> (is-a?/c bitmap%) (is-a?/c dc<%>))]

  [draw-circle (->* ((is-a?/c dc<%>) number? number?) (number?) void?)]
  [draw-path (-> (is-a?/c dc<%>) (list/c coord? coord? coord? coord?) void?)])

 ;; Helpers for measuring execution time

 ;; (repeat n body ...)
 ;; Repeats the body given number of times.
 repeat

 ;; execute thunk a number of times and return the sum of running times
 timeit-apply

 ;; return a string representation of running times
 format-running-time

 ;; (timeit times body ...)
 ;; macro version of timeit-apply which prints directly to current
 ;; output-port
 timeit)


;;================================================================================

;; Drawing helpers.

(define (make-empty-bitmap [w 1000] [h 600])
  (make-bitmap w h))

(define (get-dc bmp%)
  (send bmp% make-dc))

(define (draw-circle dc x y [r 15])
  ;; Draw a circle centered at x,y with r radius.
  (define old-brush (send dc get-brush))
  (define half-r (quotient r 2))
  (define-values (x1 y1) (values (- x half-r) (- y half-r)))
  (send* dc
    (set-brush "white" 'transparent)
    (draw-ellipse x1 y1 r r)
    (set-brush old-brush)))

(define (draw-path dc controls)
  (define path (new dc-path%))
  (send/apply path move-to (car controls))
  (send/apply path curve-to (flatten (cdr controls)))
  (send dc draw-path path))

(define (show-plot bitmap)
  ;; there is something similar in slideshow already?
  (let*
      ([bmp% bitmap]
       [w (+ 10 (send bmp% get-width))]
       [h (+ 80 (send bmp% get-height))]
       [frame (new frame% [label "Example"] [width w] [height h])]
       [paint (λ (canvas% dc%) (send dc% draw-bitmap bmp% 0 0))]
       [mycanvas (new canvas% [parent frame] [paint-callback paint])])
    (send frame show #t)))


;;================================================================================

(define-syntax repeat
  (syntax-rules ()
    [(repeat n body ...) (for
                             ([_ (in-range n)])
                           body ...)]))

(define (timeit-apply times thunk)
  (define-values (real cpu gc)
    (for/fold
      ([real 0] [cpu 0] [gc 0])
      ([z (in-range times)])
    (let-values
        ([(_ a b c) (time-apply thunk '())])
      (values (+ real a) (+ cpu b) (+ gc c)))))
  (list real cpu gc))

(define (format-running-time times-list)
  (match-let
      ([(list cpu real gc) times-list])
    (format "cpu: ~a real: ~a gc: ~a" cpu real gc)))

(define-syntax-rule (timeit times body ...)
    (displayln (format-running-time (timeit-apply times
                                                  (λ () body ...)))))


;;================================================================================

(module+ test
  (require rackunit)

  (test-case
   "Check if repeat really repeats it's body given number of times"
   (let ([acc 0])
     (repeat 5
       (set! acc (add1 acc)))
     (check-eq? acc 5))))
