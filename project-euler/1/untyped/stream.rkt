#lang racket

(define (stream-merge-unique-ints s1 s2)
  (cond [(stream-empty? s1) s2]
        [(stream-empty? s2) s1]
        [(< (stream-first s1) (stream-first s2))
           (stream-cons (stream-first s1) (stream-merge-unique-ints (stream-rest s1) s2))]
        [(> (stream-first s1) (stream-first s2))
           (stream-cons (stream-first s2) (stream-merge-unique-ints s1 (stream-rest s2)))]
        [(= (stream-first s1) (stream-first s2))
           (stream-cons (stream-first s1) (stream-merge-unique-ints (stream-rest s1) (stream-rest s2)))]))

(define (get-answer limit)
  (define threes (in-range 3 limit 3))
  (define fives  (in-range 5 limit 5))
  (define merged (stream-merge-unique-ints threes fives))
  (stream-fold + 0 merged))

(define (main) (get-answer 1000))
(time (begin (main) (void)))
