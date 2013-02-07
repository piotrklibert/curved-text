#lang racket

(provide 
 ;; (repeat n body ...)
 ;;
 ;; Repeats the body given number of times.
 repeat
 ;; execute thunk a number of times and return the sum of running times
 timeit-apply
 ;; return a string representation of running times
 format-running-time
 ;; (timeit times body ...)
 ;; macro version of timeit-apply which prints directly to current 
 ;; output-port
 timeit)

(define-syntax repeat
  (syntax-rules ()
    [(repeat n body ...) (for 
                             ([_ (in-range n)])
                           body ...)]))

(define (timeit-apply times thunk)
  (define-values (real cpu gc)
    (for/fold 
      ([real 0] [cpu 0] [gc 0])
      ([z (in-range times)])
    (let-values 
        ([(_ a b c) (time-apply thunk '())])
      (values (+ real a) (+ cpu b) (+ gc c)))))
  (list real cpu gc))

(define (format-running-time times-list)
  (match-let 
      ([(list cpu real gc) times-list])
    (format "cpu: ~a real: ~a gc: ~a" cpu real gc)))

(define-syntax-rule (timeit times body ...)
    (displayln (format-running-time (timeit-apply times 
                                                  (λ () body ...)))))


(module+ test
  (require rackunit)
  
  (test-case
   "Check if repeat really reapeats it's body given number of times"
   (let ([acc 0])
     (repeat 5
       (set! acc (add1 acc)))
     (check-eq? acc 5))))
