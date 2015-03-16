#lang typed/racket

(require math/array)

(define bits-per-sample 16)

(: myfun (-> (Array Natural) Natural (Vectorof Any)))
(define (myfun signal gain)
  (for/vector #:length (array-size signal)
              ([sample : Natural (in-array signal)])
    (let: ([n : Natural (max 0 (min (sub1 (expt 2 bits-per-sample))
                (exact-floor
                 (* gain
                    (* (+ sample 1.0) ; center at 1, instead of 0
                       (expt 2 (sub1 bits-per-sample) ))))))]) n)))
