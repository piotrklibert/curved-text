#lang racket
;;
;; Not really a tests here; just something I quickly wrote when working on the
;; code to help me visualize the impact that my changes had.
;;
;; The test procedure creates a GUI window and displays a curved text in it
;; and test-perf creates a curved-text repeteadly and reports the time it took.
;;


(require 
  srfi/26 
  racket/draw)

(require 
  "bezier-point.rkt"
  "bezier-text.rkt"
  "bezier-gui.rkt")

(provide 
  (contract-out 
    [test (->* () (string? coords?) void?)]
    [test-perf (-> number? void?)]))

(define test-control-points '((40 40) (5 200) (400 5) (550 350)))

(define (test [word "Hello world!"] 
              [control-points test-control-points])
  (let* 
      ([bmp   (make-empty-bitmap)]
       [dc    (get-dc bmp)]
       [path  (new dc-path%)]
       [font  (make-font #:size 24 #:family 'roman #:weight 'bold)]
       [curved-text (time (make-curved-text control-points word))]) 
    (send dc set-smoothing 'smoothed)

    ;; draw curve
    (send/apply path move-to  (car control-points))
    (send/apply path curve-to (flatten (cdr control-points)))
    (send dc draw-path path)

    ;; draw curved text
    (send dc set-text-foreground "blue")
    (send dc set-font font)
    (draw-curved-text curved-text dc)

    ;; display
    (show-plot bmp)))

(define (test-perf [n 200])
  (displayln (string-append "Creating " (number->string n) " curved text "
                            "instances took:"))
  (time (for ([x (in-range 200)])
          (make-curved-text test-control-points 
                            "performance-test-word"))))


(module+ main
  ;; for quick invoking from command line
  (test-perf)
  (test "10Clouds" '((10 90) (100 15) (300 470) (340 40))))
