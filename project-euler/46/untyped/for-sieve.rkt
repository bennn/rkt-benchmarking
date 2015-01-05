#lang racket

(require contract-profile)

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

(define (run limit)
  (for/or ([n (in-range 9 limit 2)])
    (define prevs (stream-filter-until (lambda (p) (> p n)) primes))
    (if (and (not (stream-ormap (lambda (x) (= n x)) prevs)) ; not member
             (stream-andmap (lambda (p) (not (integer? (sqrt (/ (- n p) 2))))) prevs)) ; can't find prime+2(square)
        n
        #f)))

(define (main) (time (run 10000)))
(contract-profile-thunk main)
