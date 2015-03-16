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
(define;/contract
  unsafe-vector-ref
  ;(parametric->/c (A) (-> (vectorof A) exact-nonnegative-integer? A))
  vector-ref)

(define;/contract
  unsafe-vector-set!
  ;(parametric->/c (A) (-> (and/c (vectorof A) (not/c immutable?)) exact-nonnegative-integer? A void?))
  vector-set!)
