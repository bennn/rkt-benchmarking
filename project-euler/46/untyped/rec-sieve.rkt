#lang racket

(define (sift n st)
  ;; remove all multiples of n from st
  (cond [(stream-empty? st) empty-stream]
        [(zero? (modulo (stream-first st) n)) (sift n (stream-rest st))]
        [else (stream-cons (stream-first st) (sift n (stream-rest st)))]))

(define (sieve st)
  (cond [(stream-empty? st) empty-stream]
        [else (stream-cons (stream-first st)
                           (sieve (sift (stream-first st)
                                 (stream-rest st))))]))

(define primes (sieve (in-range 2 10000)))

(define (stream-filter-until f st)
  (cond [(stream-empty? st) empty-stream]
        [(f (stream-first st)) empty-stream]
        [else (stream-cons (stream-first st)
                           (stream-filter-until f (stream-rest st)))]))

(define (stream-member n st)
  (stream-ormap (lambda (x) (= n x)) st))

(define (prime? n)
  (stream-member n primes))
  
(define (run n limit)
  (cond [(> n limit) #f] ; done recursing
        [(and (not (prime? n)) 
              (stream-andmap (lambda (p) (not (integer? (sqrt (/ (- n p) 2))))) (stream-filter-until (lambda (p) (> p n)) primes)))
           ;; n is not prime and couldn't find (n = p + 2(square))
           n]
        [else (run (+ 2 n) limit)]))

(time (run 9 10000))
