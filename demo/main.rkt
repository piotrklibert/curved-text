#lang racket/gui

;;
;; A simple GUI app that uses curved-text structures; it displays some text
;; along the curve and lets the user reshape (click and drag) and rotate the
;; curve (left and right arrows).
;;
;; To run, provided you have this package installed with `raco pkg install`,
;; type:
;;
;;     racket -l curved-text
;;
;; from anywhere, or you can run it without installing with:
;;
;;     racket main.rkt
;;
;; from the package root directory.
;;
;; See ../priv/demo-app-screenshot.png for screenshot.
;;

(require
 curved-text/bezier/point
 curved-text/bezier/curve
 curved-text/bezier/utils
 curved-text/demo/canvas)


(define help-text
  (string-append
   "Click and drag the circles (control points) to manipulate them.\n"
   "Press left/right arrow keys to rotate the curve around its center.\n"
   "Press +/- to scale up and down."))


(define (start-app)
  (define frame (new frame%
                  [label "Curved text example"]
                  [height 600]
                  [width 1000]))
  (define top-pane (new pane%
                     [parent frame]
                     [alignment '(left center)]
                     [stretchable-height #f]))
  (define msg (new message%
                [parent top-pane]
                [label help-text]))

  (define canvas (new curved-text-editor% [parent frame]))

  (define bottom-pane (new vertical-pane%
                        [parent frame]
                        [alignment '(left center)]
                        [min-height 30]
                        [stretchable-height #f]))
  (define coords-msg (new message%
                       [parent bottom-pane]
                       [label "-----"]
                       [auto-resize #t]))

  ;; Start a thread which will update coord-msg with current control points
  ;; once in a while. This way there is no need for callbacks or coupling
  ;; canvas and coord-msg.
  (thread
   (thunk
    (let loop ()
      (let ([fmt-pt (λ (x) (map round (point->list x)))]
            [curve (make-curve (send canvas get-endpoints) 0.01)])
        (send coords-msg set-label
              (format (string-append
                       "Center: ~a; control points: ~a~n"
                       "Length: ~a(flattened)/~a(Gauss)")
                      (fmt-pt (send canvas get-center))
                      (map fmt-pt (send canvas get-endpoints))
                      (round (curve-len curve))
                      (round (compute-length curve)))))
      (sleep 1/6)
      (loop))))

  (send canvas focus)
  (send frame show #t))


(module+ main
  (start-app))
