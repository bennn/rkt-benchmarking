#lang racket/base

(require racket/fixnum racket/contract)

(provide (all-defined-out))

(define/contract
  unsafe-fx*
  (-> fixnum? fixnum? fixnum?)
  fx*)
(define/contract
  unsafe-fx+
  (-> fixnum? fixnum? fixnum?)
  fx+)
(define/contract
  unsafe-fxmodulo
  (-> fixnum? fixnum? fixnum?)
  fxmodulo)
(define/contract
  unsafe-vector-ref
  (-> (vectorof (or/c natural-number/c flonum? procedure?)) exact-nonnegative-integer? any/c)
  vector-ref)

(define/contract
  unsafe-vector-set!
  (-> (and/c (vectorof (or/c natural-number/c flonum? procedure?)) (not/c immutable?)) exact-nonnegative-integer? any/c void?)
  vector-set!)
