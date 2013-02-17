#lang racket

(require
 "point.rkt"
 "curve.rkt"
 (only-in racket/draw dc<%>))


;;================================================================================

(define-struct/contract curved-letter ([pos  point-and-angle?] 
                                       [char string?]))

;; shortened accessors for curved-letter-pos properties
(define-syntax-rule (letter-x letter)     (point-x (curved-letter-pos letter)))
(define-syntax-rule (letter-y letter)     (point-y (curved-letter-pos letter)))
(define-syntax-rule (letter-angle letter) (point-and-angle-angle (curved-letter-pos letter)))


(struct curved-text
  (curve word letters))


;;================================================================================

(provide
 (contract-out 
  [struct curved-text ((curve   curve?)
                       (word    string?)
                       (letters (vectorof curved-letter?)))]
  
  [make-curved-text (->* (coords? string?) (number?) curved-text?)]
  
  [draw-curved-text (-> curved-text? (is-a?/c dc<%>) void?)]))


;;================================================================================

;; Curved text constructor

(define (make-curved-text coords text [res 1000])
  (define curve (make-curve coords res))
  (curved-text curve 
               text 
               (make-letters curve text)))


;;================================================================================

;; Constructor helpers

(define/contract (make-letters curve text)
                 (-> curve? string? (vectorof curved-letter?))
  (define positions (char-positions curve text))
  
  (for/vector 
      ([char (in-string text)] 
       [pos  (in-vector positions)])
    (curved-letter pos (string char))))

(define/contract (char-positions curve word)
                 (-> curve? string? (vectorof point-and-angle?))
  (define word-length  (string-length word))
  (define curve-length (curve-len curve))
  (define spacing      (/ curve-length word-length))
  
  (for/vector 
      ([char-num (in-range word-length)])
    (get-nearest-point curve (* spacing char-num))))


;;================================================================================

;; Other curved text methods

(define (draw-curved-text curved-text dc)
  (define letters (curved-text-letters curved-text))
  
  (for ([letter (in-vector letters)])
    (let ([x (letter-x letter)] [angle (letter-angle letter)]
          [y (letter-y letter)] [char  (curved-letter-char letter)])
      
      (send dc draw-text char x y #f 0 angle))))