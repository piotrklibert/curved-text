#lang racket/gui
(require srfi/26)

(require "bezier-struct.rkt")
(require "bezier-gui.rkt")

(define default-controls (list->vector 
                          (pairs->points '((10 90) 
                                           (100 15) 
                                           (300 470) 
                                           (340 40)))))
(define default-font (make-font #:size 28
                                #:weight 'bold
                                #:family 'roman))
(define curved-text-editor%
  (class canvas%
    (init-field [text           "10Clouds"] 
                [font           default-font]
                [control-radius 15])
    
    (define controls       default-controls)
    (define active-control #false)
    (define control-margin 5)
    
    ;; this is an attempt to reduce the number of redraws, but
    ;; I don't think it helped much
    (define prev-mouse-pos     (point 0 0))
    (define min-animation-dist 3)
    (define (worth-animating? cur-pos)
      (> (points-distance prev-mouse-pos cur-pos) 
         min-animation-dist))
    ;;
    ;; Private helper methods.
    ;;
    (define/private (get-own-dc) ;; and set smoothing by the way
      (let ([dc (send this get-dc)])
        (send dc set-smoothing 'smoothed)
        dc))
    
    (define/private (move-active-control where) 
      (vector-set! controls active-control where)) 
    
    (define/private (which-control-clicked click-pos)
      (let
          ([controls-count (vector-length controls)]
           [get-control    (cut vector-ref controls <>)]
           [dist-to-point  (cut points-distance click-pos <>)])
        (let check-controls ([i 0])
          (if (>= i controls-count) 
              #false ;; none of control points is near
              (if (< (dist-to-point (get-control i)) 
                     (+ control-radius control-margin)) 
                  i ;; return index of nearby control
                  (check-controls (add1 i)))))))
    
   
    ;;
    ;; Private drawing helpers.
    ;;
    (define/private (draw-supporting-curve dc) 
      (draw-path dc (points->pairs controls)))
    
    (define/private (draw-controls dc)
      (for ([control-point controls])
        (match (point->pair control-point)
          [(list x y) (draw-circle dc x y control-radius)])))
    
    (define/private (draw-text dc)
      (send* dc 
        (set-text-foreground "blue")
        (set-font font))
      (draw-curved-text (make-curved-text controls text) dc))
    
    
    ;;
    ;; Public methods - GUI callbacks.
    ;;
    (define/override (on-event event)
      (let* 
          (;; click position
           [mouse-pos (point (send event get-x) 
                             (send event get-y))]
           ;; click event properties
           [down      (send event button-down?)]
           [up        (send event button-up?)]
           [dragging  (send event dragging?)]) 
        (cond
          [(and active-control dragging) (when 
                                             (worth-animating? mouse-pos)
                                           (set! prev-mouse-pos mouse-pos)
                                           (move-active-control mouse-pos) 
                                           (send this refresh-now))]
          [down (let 
                    ([control (which-control-clicked mouse-pos)])
                  (when control 
                    (set! active-control control)
                    (set! prev-mouse-pos mouse-pos)))]
          [up (set! active-control #false)])))
    
    (define/override (on-paint)
      (let 
          ([dc (get-own-dc)])
        (draw-supporting-curve dc)
        (draw-controls dc)
        (draw-text dc)))
    
    (super-new)))


(module+ main
  (define frame  (new frame% [label "Curved text example"] 
                             [height 600] [width 1000]))
  (define canvas (new curved-text-editor% [text "10Clouds"]
                                          [parent frame]))
  (send frame show #t))