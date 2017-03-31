#lang racket/gui


(require
 srfi/26
 (only-in racket/draw font%)
 curved-text/bezier/point
 curved-text/bezier/curved-text
 curved-text/bezier/utils)

(provide curved-text-editor%)

(define default-controls
  (for/vector
      ([coord '((102 316) (152 16) (610 27) (696 242))])
    (list->point coord)))

(define default-font
  (make-font #:size 28 #:weight 'bold #:family 'roman))

(define curved-text-editor%
  (class canvas%
    ;; automatically initialized public fields
    (init-field [text           "Some curved text"]
                [font           default-font]
                [control-radius 15]
                [center-radius  20])
    (super-new)

    ;; private fields
    (define controls       default-controls)
    (define active-control #false) ;; an index in controls vector or #false
    (define active-center  #false)
    (define control-margin 5)

    (define min-animation-dist 3)
    (define prev-mouse-pos (make-point 0 0))
    (define (worth-animating? cur-pos)
      (> (points-distance prev-mouse-pos cur-pos)
         min-animation-dist))


    (define/private (get-control-radius) (+ control-radius control-margin))
    (define/private (get-control n)      (vector-ref controls n))
    (define/public  (get-endpoints)      (vector->list controls))

    ;; compute the "center of mass" for the four controls
    (define/public (get-center)
      (let ([sum (apply + (map point->complex (get-endpoints)))])
        (complex->point (/ sum 4))))

    (define/private (get-own-dc)
      (let ([dc (send this get-dc)])
        (send dc set-smoothing 'smoothed)
        dc))

    ;; Helpers for handling mouse interaction with control points.
    (define/private (nth-clicked? n pos)
      (if (control-clicked? (get-control n) pos) n #f))

    (define/private (control-clicked? ctrl pos)
      (< (points-distance ctrl pos) (get-control-radius)))

    (define/private (center-clicked? pos)
      (< (points-distance (get-center) pos)
         (+ center-radius control-margin)))

    ;; get-clicked-control: point -> (or/c number? #f)
    (define/private (get-clicked-control click-pos)
      (index-where (get-endpoints) (λ (c) (control-clicked? c click-pos))))

    ;; move-active-control!: point? -> void?
    (define/private (move-active-control! where)
      (move-control! active-control where))


    ;; Drawing helpers.

    (define/private (draw-supporting-curve dc)
      (draw-path dc (map point->list (get-endpoints))))

    (define/private (draw-center dc)
      (let* ([center (get-center)])
        (draw-circle dc (point-x center) (point-y center) center-radius)))

    (define/private (draw-controls dc)
      (for ([ctrl controls])
        (draw-circle dc (point-x ctrl) (point-y ctrl) control-radius)))

    (define/private (draw-text dc)
      (send* dc
        (set-text-foreground "blue")
        (set-font font))
      (draw-curved-text (make-curved-text controls text) dc))


    ;;
    ;; Public methods
    ;;

    (define/public (move-control! which new-pos)
      (vector-set! controls which new-pos))

    ;; move-controls!: complex? -> void?
    (define/public (move-curve! direction)
      (set! controls
        (vector-map (λ (x) (complex->point
                            (+ direction (point->complex x))))
                    controls)))

    ;; Rotate controls by an angle given as a number of degrees.
    (define/public (rotate-controls! direction [angle 5])
      (define center (get-center))
      (define degrees (if (equal? direction 'left) (- angle) angle))
      (define transform (cut rotate-point <> degrees center #:type 'degrees))
      (set! controls (vector-map transform controls))
      (send this refresh))

    ;; Move all controls away from or towards the center by a small distance.
    (define/public (scale-controls! direction)
      (define scale (if (member direction '(add #\+)) 1.2 0.8))
      (define transform (cut scale-point <> scale (get-center)))
      (set! controls (vector-map transform controls))
      (send this refresh))


    ;; GUI callbacks.
    ;; =========================================================================

    ;; KEYBOARD events
    (define/override (on-char event)
      (define key-code (send event get-key-code))
      (when (not (equal? key-code 'release)) ; on keydown but not on keyup
        (cond
          [(member key-code '(left right))
           (rotate-controls! key-code)]
          [(member key-code '(add subtract #\+ #\-))
           (scale-controls! key-code)])))

    ;; MOUSE events
    (define/override (on-event event)
      (let*
          ( ;; click position
           [mouse-pos (make-point (send event get-x) (send event get-y))]
           ;; click event properties
           [down      (send event button-down?)]
           [up        (send event button-up?)]
           [dragging  (send event dragging?)]
           [should-move? (worth-animating? mouse-pos)])
        (cond
          ;; manually dispatch on event type
          [(and dragging active-control)
           (when should-move?
             (set! prev-mouse-pos mouse-pos)
             (move-active-control! mouse-pos)
             (send this refresh-now))]

          [(and dragging active-center)
           (when should-move?
             (move-curve! (- (point->complex mouse-pos)
                             (point->complex prev-mouse-pos)))
             (set! prev-mouse-pos mouse-pos)
             (send this refresh-now))]

          [down (cond
                  [(get-clicked-control mouse-pos) =>
                   (λ (control)
                     (set! active-control control)
                     (set! prev-mouse-pos mouse-pos))]
                  [(center-clicked? mouse-pos)
                   (set! active-center #true)
                   (set! prev-mouse-pos mouse-pos)])]

          [up (set! active-control #false)
              (set! active-center #false)])))


    (define/override (on-paint)
      (let ([dc (get-own-dc)])
        (draw-supporting-curve dc)
        (draw-controls dc)
        (draw-center dc)
        (draw-text dc)))))
