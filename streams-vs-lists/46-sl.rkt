#lang racket

(define (nat-stream? st)
  (and (stream? st)
       (or (stream-empty? st)
           (natural-number/c (stream-first st)))))

(define/contract (sift n st)
  (-> natural-number/c nat-stream? nat-stream?)
  ;; remove all multiples of n from st
  (cond [(stream-empty? st) empty-stream]
        [(zero? (modulo (stream-first st) n)) (sift n (stream-rest st))]
        [else (stream-cons (stream-first st) (sift n (stream-rest st)))]))

(define/contract (sieve st)
  (-> nat-stream? nat-stream?)
  (cond [(stream-empty? st) empty-stream]
        [else (stream-cons (stream-first st)
                           (sieve (sift (stream-first st)
                                 (stream-rest st))))]))

(define primes (sieve (in-range 2 10000)))

;; Create a list of all stream elements [x] for which [f x] is true
(define/contract (stream-take f st)
  (-> (-> natural-number/c boolean?) nat-stream? (listof natural-number/c))
  (for/list ([x st] #:when (f x))
    x))
    

(define (run limit)
  (-> natural-number/c natural-number/c)
  (for/or ([n (in-range 9 limit 2)])
    (define prevs (stream-take (lambda (p) (<= p n)) primes))
    (if (and (not (memq n prevs))
             (for/and ([p prevs]) (not (integer? (sqrt (/ (- n p) 2))))))
        n
        #f)))

(define (main) (run 10000))
(time (begin (main) (void)))
