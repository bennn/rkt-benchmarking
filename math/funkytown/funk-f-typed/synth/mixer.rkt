#lang typed/racket

(require (only-in "math/array.rkt"
                  Array
                  array-broadcasting
                  array-map))

(provide mix)

;; A Weighted-Signal is a (List Array Real)
(define-type Weighted-Signal (List Array Real))

;; Weighted sum of signals, receives a list of lists (signal weight).
;; Shorter signals are repeated to match the length of the longest.
;; Normalizes output to be within [-1,1].

;; mix : Weighted-Signal * -> (Array Float)
(: mix (->* () #:rest (Listof Weighted-Signal) Array))
(define (mix . ss)

  (define signals ((inst map Weighted-Signal) (lambda (x : Weighted-Signal)
                         ((inst car Array Real) x))
                       ss))
  (define weights ((inst map Weighted-Signal) (lambda (x : Weighted-Signal)
                         (real->double-flonum ((inst second (Array Float) Real Weighted-Signal) x)))
                       ss))
  (define downscale-ratio (/ 1.0 (apply + weights)))

  (: scale-signal (-> Number (-> Number Number)))
  (define ((scale-signal w) x) (* x w downscale-ratio))

  (parameterize ([array-broadcasting 'permissive]) ; repeat short signals
    (for/fold ([res : Array (array-map (scale-signal (first weights))
                               (first signals))])
        ([s (in-list (rest signals))]
         [w (in-list (rest weights))])
      (define scale (scale-signal w))
      (array-map (lambda ([acc : Float]
                          [new : Float])
                   (+ acc (scale new)))
                 res s))))
