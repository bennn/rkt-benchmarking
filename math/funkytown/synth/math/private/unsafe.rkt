#lang typed/racket/base

(require (prefix-in : racket/unsafe/ops)
         racket/flonum
         racket/fixnum)

(provide (all-defined-out))

(define unsafe-vector-ref vector-ref)
(define unsafe-vector-set! vector-set!)
(define unsafe-vector-length vector-length)

(define unsafe-flvector-ref flvector-ref)
(define unsafe-flvector-set! flvector-set!)

(define unsafe-fx* fx*)
(define unsafe-fx+ fx+)
(define unsafe-fx- fx-)
(define unsafe-fxmodulo fxmodulo)
(define unsafe-fxquotient fxquotient)
(define unsafe-fx>= fx>=)
(define unsafe-fx= fx=)
(define unsafe-fx< fx<)

(define unsafe-car car)
(define unsafe-cdr cdr)
