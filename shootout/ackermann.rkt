#lang racket/base

(require racket/contract)
(define nat? natural-number/c)

(define (ack m n)
  (-> nat? nat? nat?)
  (cond ((zero? m) (+ n 1))
        ((zero? n) (ack (- m 1) 1))
        (else      (ack (- m 1) (ack m (- n 1))))))

;; CPU time = 24ms without contracts
;;          = 4987ms  with contracts
;; (time (begin (ack 3 8) (void)))

;; CPU time = 5758ms without contracts
;;          = 
(time (begin (ack 3 12) (void)))
