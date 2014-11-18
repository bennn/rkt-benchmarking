#lang racket

;; 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
;; Find the sum of all numbers which are equal to the sum of the factorial of their digits.
;; Note: as 1! = 1 and 2! = 2 are not sums they are not included.

(define (fact-memo n)
  (cond [(equal? n #\0) 1]
        [(equal? n #\1) 1]
        [(equal? n #\2) 2]
        [(equal? n #\3) 6]
        [(equal? n #\4) 24]
        [(equal? n #\5) 120]
        [(equal? n #\6) 720]
        [(equal? n #\7) 5040]
        [(equal? n #\8) 40320]
        [(equal? n #\9) 362880]
        [else (raise "fact-memo")]))

(define (fact-sum n)
  ;; sum the factorials of the digits of n
  (for/sum ([d (~a n)]) (fact-memo d)))

(define (run limit)
  (for/sum ([i (in-range 10 limit)])
    (if (= i (fact-sum i)) i 0)))

(time (run 9000000))
;; (time (run 900))
