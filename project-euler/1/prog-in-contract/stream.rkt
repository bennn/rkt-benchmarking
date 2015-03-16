#lang racket

(require contract-profile)

(define (positive-stream? st)
  (or (stream-empty? st)
      (positive? (stream-first st))))

;; it'd be nice to attach proof of function's correctness
(define/contract (stream-merge-unique-ints s1 s2)
  (-> positive-stream? positive-stream? positive-stream?)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [(< (stream-first s1) (stream-first s2))
           (stream-cons (stream-first s1) (stream-merge-unique-ints (stream-rest s1) s2))]
        [(> (stream-first s1) (stream-first s2))
           (stream-cons (stream-first s2) (stream-merge-unique-ints s1 (stream-rest s2)))]
        [(= (stream-first s1) (stream-first s2))
           (stream-cons (stream-first s1) (stream-merge-unique-ints (stream-rest s1) (stream-rest s2)))]))

(define/contract (get-answer limit)
  (-> positive? positive?)
  (define threes (in-range 3 limit 3))
  (define fives  (in-range 5 limit 5))
  (define merged (stream-merge-unique-ints threes fives))
  (stream-fold + 0 merged))

;; (define (main) (time (get-answer 1000)))
;; (contract-profile-thunk main)

(define ((sum-of-3-5-multiples? limit) n)
  (= n (get-answer 1000)))

(define/contract (answer)
  (-> (sum-of-3-5-multiples? 1000))
  233168)

(contract-profile-thunk answer)
