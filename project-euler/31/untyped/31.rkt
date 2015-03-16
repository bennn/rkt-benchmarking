#lang racket

(define (coin-val x)
  (cond [(= x 0) 0]
        [(= x 1) 1]
        [(= x 2) 2]
        [(= x 3) 5]
        [(= x 4) 10]
        [(= x 5) 20]
        [(= x 6) 50]
        [(= x 7) 100]
        [else 200]))

(define (make-change n)
  (for
