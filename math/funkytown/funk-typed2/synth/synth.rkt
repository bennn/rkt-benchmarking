#lang typed/racket

;; (require math/array)
(require (only-in "math/array.rkt"
                  Array
                  in-array))

(: signal->integer-sequence (-> (Array Any) Natural (Vectorof Natural)))
(define (signal->integer-sequence signal gain)
  (for/vector ([sample (in-array signal)]) 4))
