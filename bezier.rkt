#lang racket/gui

(require "bezier-struct.rkt")
(require "bezier-gui.rkt")

(define initial-controls (list->vector 
                          (pairs->points '((10 90) 
                                           (100 15) 
                                           (300 470) 
                                           (340 40)))))

(define curved-text-editor%
  (class canvas%
    (init word)
    (define text word)
    (define font (make-font #:size 28 
                            #:family 'roman 
                            #:weight 'bold))
    
    (define active-control #false)
    (define controls       initial-controls)
    
    ;; this is an attempt to reduce number of redraws, but
    ;; I don't think it helped much
    (define prev-mouse-pos     (point 0 0))
    (define min-animation-dist 3)
    (define (worth-animating? cur-pos)
      (> (points-distance prev-mouse-pos cur-pos) 
         min-animation-dist))
    ;;
    ;; Private helper methods.
    ;;
    (define (get-own-dc) ;; and set smoothing by the way
      (let ([dc (send this get-dc)])
        (send dc set-smoothing 'smoothed)
        dc))
    
    (define (move-active-control where) 
      (vector-set! controls active-control where)) 
    
    (define (which-control-clicked? click-pos)
      (let
          ([controls-count (vector-length controls)]
           [get-control    (curry vector-ref controls)]
           [dist-to-point  (curry points-distance click-pos)])
        (let check-controls ([i 0])
          (if (>= i controls-count) 
              #false ;; none of control points is near
              (if (< (dist-to-point (get-control i)) 20) 
                  i ;; return index of nearby control
                  (check-controls (add1 i)))))))
    
   
    ;;
    ;; Private drawing helpers.
    ;;
    (define (draw-supporting-curve dc) 
      (draw-path dc (points->pairs controls)))
    
    (define (draw-controls dc)
      (for ([control-point controls])
        (apply draw-circle dc (point->pair control-point))))
    
    (define (draw-text dc)
      (send dc set-text-foreground "blue")
      (send dc set-font font)
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
          [down (let ([control (which-control-clicked? mouse-pos)])
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
  (define frame (new frame% [label "Curved text example"] 
                            [height 600] 
                            [width 1000]))
  (new curved-text-editor% [word "10Clouds"]
                           [parent frame])
  (send frame show #t))