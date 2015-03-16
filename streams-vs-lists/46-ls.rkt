#lang racket

(define (nat-stream? st)
  (and (stream? st)
       (or (stream-empty? st)
           (natural-number/c (stream-first st)))))

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

(define/contract (stream-filter-until f st)
  (-> (-> natural-number/c boolean?) (listof natural-number/c) nat-stream?)
  (cond [(empty? st) empty-stream]
        [(f (car st)) empty-stream]
        [else (stream-cons (car st)
                           (stream-filter-until f (cdr st)))]))

(define (run limit)
  (-> natural-number/c natural-number/c)
  (for/or ([n (in-range 9 limit 2)])
    (define prevs (stream-filter-until (lambda (p) (> p n)) primes))
    (if (and (not (stream-ormap (lambda (x) (= n x)) prevs)) ; not member
             (stream-andmap (lambda (p) (not (integer? (sqrt (/ (- n p) 2))))) prevs)) ; can't find prime+2(square)
        n
        #f)))

(define (main) (run 10000))
(time (begin (main) (void)))
