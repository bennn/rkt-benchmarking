#lang racket

(define/contract
  (divides? a b)
  (-> natural-number/c natural-number/c boolean?)
  (cond [(= b 0) #t]
        [(< b a) #f]
        [else (divides? a (- b a))]))

(define/contract
  (sieve acc st)
  (-> (listof natural-number/c) stream? (listof natural-number/c))
  (cond [(stream-empty? st)                                acc]
        [(for/or ([p acc]) (divides? p (stream-first st))) (sieve acc (stream-rest st))]
        [else                                              (sieve (cons (stream-first st) acc) (stream-rest st))]))
         

(define primes (reverse (sieve empty (in-range 2 10000))))

(define/contract
  (run limit)
  (-> natural-number/c natural-number/c)
  (for/or ([n (in-range 9 limit 2)])
    (define prevs (filter (lambda (p) (<= p n)) primes))
    (if (and (not (memq n prevs))
             (for/and ([p prevs]) (not (integer? (sqrt (/ (- n p) 2))))))
        n
        #f)))

(define (main)
  (run 10000))
(time (begin (main) (void)))
