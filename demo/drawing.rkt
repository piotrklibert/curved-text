#lang racket

;;
;; These are not tests per se, just something I quickly wrote when working on
;; the code to help me visualize the impact that my changes had.
;;
;; The test procedure creates a GUI window and displays a curved text in it
;; while test-perf creates a curved-text repeteadly and reports the time it
;; took.
;;


(require
 srfi/26
 threading
 racket/draw)

(require
 curved-text/bezier/point
 curved-text/bezier/curved-text
 curved-text/bezier/utils)

(provide
  (contract-out
    [test (->* () (string? coords?) void?)]
    [test-perf (->* () (number?) void?)]))

(define test-control-points '((40 40) (5 200) (400 5) (550 350)))

(define (test [word "Hello world!"]
              [control-points test-control-points])
  (let*
      ([bmp   (make-empty-bitmap)]
       [dc    (get-dc bmp)]
       [path  (new dc-path%)]
       [font  (make-font #:size 28 #:family 'roman #:weight 'bold)])
    ;; setup the drawing context
    (send* dc
      (set-smoothing 'smoothed)
      (set-text-foreground "blue")
      (set-font font))

    (draw-path dc control-points)       ; draw the supporting curve
    (~> control-points                  ; draw the text along the curve
      (make-curved-text word)
      (draw-curved-text dc))

    (show-plot bmp)))                   ; display the bitmap


(define (test-perf [n 500])
  (displayln
   (string-append "Creating " (number->string n) " curved text "
                  "instances took:"))
  (time
   (for ([x (in-range n)])
     (make-curved-text test-control-points "performance-test-word"))))


(module+ main
  (test-perf)
  (test "Sample text to curve!" '((102 316) (152 16) (610 27) (696 242))))
