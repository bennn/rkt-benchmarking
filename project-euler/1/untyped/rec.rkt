#lang racket

(require contract-profile)

(define (find-mods mods i)
  (cond [(empty? mods) 0]
        [(zero? (modulo i (car mods))) i]
        [else (find-mods (cdr mods) i)]))

;; Find sum of all multiples of 3 or 5 below 1000
(define (find-multiples-under-aux mods limit i)
  (if (= i limit)
      0
      (+ (find-mods mods i)
         (find-multiples-under-aux mods limit (add1 i)))))

(define (find-multiples-under mods limit)
  (find-multiples-under-aux mods limit 3))

(define (main) (time (find-multiples-under (list 3 5) 1000)))
(contract-profile-thunk main)
