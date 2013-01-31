#lang racket/gui

;; TODO: provide real contracts!
(provide 
 (contract-out [show-plot (-> (is-a?/c bitmap%) 
                              (values (is-a?/c frame%) 
                                      (is-a?/c canvas%)))])
 make-empty-bitmap get-dc draw-circle draw-path)


;;
;; Drawing helpers.
;;
(define (make-empty-bitmap [w 600] [h 400])
  (make-bitmap w h))

(define (get-dc bmp%)
  (send bmp% make-dc))

(define (draw-circle dc x y [r 15])
  (let* 
      ([old-brush (send dc get-brush)]
       [half-r (/ r 2)]
       [x (- x half-r)]
       [y (- y half-r)])
    (send dc set-brush "white" 'transparent)
    (send dc draw-ellipse x y r r)
    (send dc set-brush old-brush)))

(define (draw-path dc controls)
  (let 
      ([path  (new dc-path%)])
    (send/apply path move-to  (car controls))
    (send/apply path curve-to (flatten (cdr controls)))
    (send dc draw-path path)))
  
(define (show-plot bitmap)
  (let* 
      ([bmp% bitmap]
       [w (+ 10 (send bmp% get-width))]
       [h (+ 80 (send bmp% get-height))]
       [frame (new frame% [label "Example"] [width w] [height h])]
       [paint (λ (canvas% dc%) (send dc% draw-bitmap bmp% 0 0))]
       [mycanvas (new canvas% [parent frame] [paint-callback paint])])
    (send frame show #t)
    (values frame mycanvas)))

#|
;; this shouldn't in theory but it slows down startup time
;; considerably :(
(module* example #false
  (require framework)
  (provide test)
  (define (test)
    (define b (make-bitmap 40 40))
    (send b load-file (finder:get-file))
    (show-plot b)))
|#