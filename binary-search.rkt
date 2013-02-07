#lang racket
(require srfi/26)

(provide 
 (contract-out 
  [binarysearch (-> (vectorof number?) number? (list/c boolean? number?))]))

(define (binarysearch vector value)
  ;; performs binary search and returns a list with boolean indicating
  ;; if the value was found as it's first element. Second element is 
  ;; either an index of found value or an index at which the value should
  ;; be inserted.
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

(module+ test
  (require rackunit)
  (define test-vec (list->vector (range 12 45 3)))
  
  (match-let 
      ([(list found? pos) (binarysearch test-vec 34)])
    (test-equal? "was found?" found? #f)
    (test-equal? "insert at?" pos 7))
  
  (match-let 
      ([(list found? pos) (binarysearch test-vec 42)])
    (test-equal? "was found?" found? #t)
    (test-equal? "value index?" pos 10)))