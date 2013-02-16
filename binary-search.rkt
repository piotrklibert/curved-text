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
  (define get-pos (cut vector-ref vector <>))
  (define end (sub1 (vector-length vector)))
  
  (let helper ([low 0] 
               [high end])
    (if (< high low)
        (list #f high)
        (let 
            ([middle (quotient (+ low high) 2)])
          (cond
            [(> (get-pos middle) value) 
             (helper low (sub1 middle))]
            
            [(< (get-pos middle) value) 
             (helper (add1 middle) high)]
            
            [else 
             (list #t middle)])))))

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
    (test-equal? "value index?" pos 10))
  
  (test-case
   "If a value is smaller than the smallest value in vector search returns -1"
   (match-let
       ([(list found? pos) (binarysearch test-vec 7)])
     (test-equal? "was found?" found? #f)
     (test-equal? "value index?" pos -1)))
  
  (test-case
   "If a value is larger than the largest value in vector search returns 
    an index of a last element in vector"
   (match-let 
       ([(list found? pos) (binarysearch test-vec 100)])
     (test-equal? "was found?" found? #f)
     (test-equal? "value index?" pos (sub1 (vector-length test-vec))))))