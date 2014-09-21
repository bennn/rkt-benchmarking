#lang racket

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))))

(time (fib 26))
