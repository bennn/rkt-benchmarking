#lang racket

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.
;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(define (fact n)
  (if (< n 2) 1 (* n (fact (sub1 n)))))

(define (fact-sum n)
  ;; sum the factorials of the digits of n
  (for/sum ([d (~a n)]) (fact (- (char->integer d) 48))))

(define (run limit)
  (for/sum ([i (in-range 10 limit)])
    (if (= i (fact-sum i)) i 0)))

;; (time (run 9000000))
(time (run 900))
