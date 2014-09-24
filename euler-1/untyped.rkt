#lang racket

;; Find sum of all multiples of 3 or 5 below 1000
(define (find-multiples-under mods limit)
  (for/sum ([i (in-range limit)])
    (if (for/or ([m mods]) (zero? (modulo i m))) i 0)))

(time (find-multiples-under (list 3 5) 1000))