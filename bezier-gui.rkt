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
    (send* dc 
      (set-brush "white" 'transparent)
      (draw-ellipse x y r r)
      (set-brush old-brush))))
  
(define (draw-path dc controls)
  (define path (new dc-path%))
  (match 
      controls
    [(list (list x0 y0) (list x1 y1) (list x2 y2) (list x3 y3))
     (send* path 
       (move-to x0 y0)
       (curve-to x1 y1 x2 y2 x3 y3))
     (send dc draw-path path)]))
  
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