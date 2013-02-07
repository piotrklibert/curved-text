#lang racket

(require 
 srfi/26
 "bezier-point.rkt"
 "bezier-curve.rkt"
 (only-in racket/draw dc<%>))


(define-struct/contract curved-letter ([pos  point-and-angle?] 
                                       [char string?]))

;; shortened accessors for curved-letter-pos properties
(define-syntax-rule (letter-x letter)     (point-x (curved-letter-pos letter)))
(define-syntax-rule (letter-y letter)     (point-y (curved-letter-pos letter)))
(define-syntax-rule (letter-angle letter) (point-and-angle-angle (curved-letter-pos letter)))


(struct curved-text
  (curve word letters))


(provide
 (contract-out 
  [struct curved-text ((curve   curve?)
                       (word    string?)
                       (letters (vectorof curved-letter?)))]
  [make-curved-text (->* (coords? string?) (number?) 
                         curved-text?)]
  
  [draw-curved-text (-> curved-text? (is-a?/c dc<%>) void?)]
  ))


(define (make-curved-text coords text [res 1000])
  (define curve (make-curve coords res))
  (curved-text curve text 
               (make-letters curve text)))


(define/contract (make-letters curve text)
                 (-> curve? string? (vectorof curved-letter?))
  (define positions (char-positions curve text))
  (for/vector ([char (in-string text)] 
               [pos  (in-vector positions)])
    (curved-letter pos (string char))))

(define/contract (char-positions curve word)
                 (-> curve? string? (vectorof point-and-angle?))
  (define word-length  (string-length word))
  (define curve-length (curve-len curve))
  (let* 
      ([spacing        (/ curve-length word-length)]
       [point-for-dist (cut get-nearest-point curve <>)])
    (for/vector ([char-num (in-range word-length)])
      (point-for-dist (* spacing char-num)))))


(define (draw-curved-text curved-text dc)
  (define letters (curved-text-letters curved-text))
  (for ([letter (in-vector letters)])
    (let 
        ;; experimenting with different code layouts here:
        ([x (letter-x letter)] [angle (letter-angle letter)]
         [y (letter-y letter)] [char  (curved-letter-char letter)])
        ;; is this readable? I need to look at it for some time to decide...
      (send dc draw-text char x y #f 0 angle))))