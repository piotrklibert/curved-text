#lang racket
(require srfi/26)

(provide binarysearch)

(define (binarysearch vector value)
  (let 
      ([get-pos (cut vector-ref vector <>)])
    (let helper ([low 0]
                 [high (- (vector-length vector) 1)])
      (if (< high low)
          (list #f high)
          (let 
              ([middle (quotient (+ low high) 2)])
            (cond
              [(> (get-pos middle) value) (helper low (- middle 1))]
              [(< (get-pos middle) value) (helper (+ middle 1) high)]
              [else (list #t middle)]))))))

(define (test)
  (define test-vec (list->vector (range 12 45 3)))
  (displayln (binarysearch test-vec 34))
  (displayln (binarysearch test-vec 33)))
