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
  (make-font #:size 28
             #:weight 'bold
             #:family 'roman))

(define curved-text-editor%
  (class canvas%
    ;; automatically initialized public fields
    (init-field [text           "Some curved text"]
                [font           default-font]
                [control-radius 15])

    ;; private fields
    (define controls       default-controls)
    (define active-control #false) ;; an index in controls vector or #false
    (define control-margin 5)


    ;; this is an attempt to reduce the number of redraws, but
    ;; I don't think it helped much
    ;; TODO: profile and probably throw this away or maybe refactor
    (define prev-mouse-pos     (make-point 0 0))
    (define min-animation-dist 3)
    (define (worth-animating? cur-pos)
      (> (points-distance prev-mouse-pos cur-pos)
         min-animation-dist))

    (define/private (get-control n)
      (vector-ref controls n))
    ;; move-active-control!: point? -> void?
    (define/private (move-active-control! where)
      (vector-set! controls active-control where))


    ;; Controls (control points) helpers.

    ;; get-clicked-control: point -> (or/c number? #f)
    (define/private (get-clicked-control click-pos)
      ;; return an index of clicked control or #false if none
      ;; of them was clicked... no idea how to do this better
      (cond
        [(nth-clicked? 0 click-pos)]
        [(nth-clicked? 1 click-pos)]
        [(nth-clicked? 2 click-pos)]
        [(nth-clicked? 3 click-pos)]
        [else #false]))

    (define/private (nth-clicked? n pos)
      (if (control-clicked? (get-control n) pos) n #f))
    (define/private (control-clicked? ctrl pos)
      (< (points-distance ctrl pos) (real-radius)))
    (define/private (real-radius)
      (+ control-radius control-margin))

    (define/public (get-endpoints)
      (list  (get-control 0)
             (get-control 1)
             (get-control 2)
             (get-control 3)))
    (define/public (get-center)
      (match-let
          ([(list a b c d) (map point->complex (get-endpoints))])
        (complex->point (/ (+ a b c d) 4))))


    ;; Drawing helpers.

    (define/private (get-own-dc)
      ;; and set smoothing by the way
      (let ([dc (send this get-dc)])
        (send dc set-smoothing 'smoothed)
        dc))

    (define/private (draw-supporting-curve dc)
      (draw-path dc (for/list ([c controls]) (point->list c))))
    (define/private (draw-center dc)
      (let* ([center (get-center)]
             [x (point-x center)]
             [y (point-y center)])
        (draw-circle dc x y 20)))
    (define/private (draw-controls dc)
      (for ([control-point controls])
        (match (point->list control-point)
          [(list x y) (draw-circle dc x y control-radius)])))

    (define/private (draw-text dc)
      (send* dc
        (set-text-foreground "blue")
        (set-font font))
      (draw-curved-text (make-curved-text controls text) dc))


    ;;
    ;; Public methods
    ;;

    ;; Rotate controls by an angle given as a number of degrees.
    (define/public (rotate-controls! direction [angle 5])
      (define center (get-center))
      (define degrees (if (equal? direction 'left) (- angle) angle))
      (define transform (cut rotate-point <> degrees center #:type 'degrees))

      (set! controls (vector-map transform controls))
      (send this refresh))

    (define/public (scale-controls! direction)
      (define scale (if (or (equal? direction 'add)
                            (equal? direction #\+))
                        1.2
                        0.8))
      (define transform (cut scale-point <> scale (get-center)))

      (set! controls (vector-map transform controls))
      (send this refresh))

    ;; GUI callbacks.

    ;; KEYBOARD events
    (define/override (on-char event)
      (define key-code (send event get-key-code))

      (when (not (equal? key-code 'release))
        ;; on keydown but not on keyup
        (cond
          [(member key-code '(left right))   (rotate-controls! key-code)]
          [(member key-code '(add subtract #\+ #\-)) (scale-controls! key-code)])))

    ;; MOUSE events
    (define/override (on-event event)
      (let*
          ( ;; click position
           [mouse-pos (make-point (send event get-x)
                                  (send event get-y))]
           ;; click event properties
           [down      (send event button-down?)]
           [up        (send event button-up?)]
           [dragging  (send event dragging?)])
        (cond
          ;; manually dispatch on event type
          [(and dragging active-control)
           (when (worth-animating? mouse-pos)
             (set! prev-mouse-pos mouse-pos)
             (move-active-control! mouse-pos)
             (send this refresh-now))]

          [down (let ([control (get-clicked-control mouse-pos)])
                  (when control
                    (set! active-control control)
                    (set! prev-mouse-pos mouse-pos)))]

          [up (set! active-control #false)])))

    (define/override (on-paint)
      (let
          ([dc (get-own-dc)])
        (draw-supporting-curve dc)
        (draw-controls dc)
        (draw-center dc)
        (draw-text dc)))

    (super-new)))
