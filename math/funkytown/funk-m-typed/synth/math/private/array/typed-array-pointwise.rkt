#lang typed/racket/base

(require (only-in "array-struct.rkt"
                  Array
                  array-default-strict
                  Array-shape
                  Array-unsafe-proc
                  unsafe-build-array)
         (only-in "array-broadcast.rkt" array-broadcast array-shape-broadcast)
         (only-in "utils.rkt" Indexes)
         (only-in "untyped-array-pointwise.rkt" inline-array-map))

(provide array-map)

(: array-map (All (R A B T ...)
                  (case-> ((-> R) -> (Array R))
                          ((A -> R) (Array A) -> (Array R))
                          ((A B T ... T -> R) (Array A) (Array B) (Array T) ... T -> (Array R)))))
(define array-map
  (case-lambda:
    [([f : (-> R)])
     (inline-array-map f)]
    [([f : (A -> R)] [arr : (Array A)])
     (inline-array-map f arr)]
    [([f : (A B -> R)] [arr0 : (Array A)] [arr1 : (Array B)])
     (inline-array-map f arr0 arr1)]
    [([f : (A B T ... T -> R)] [arr0 : (Array A)] [arr1 : (Array B)] . [arrs : (Array T) ... T])
     (define ds (array-shape-broadcast (list* (Array-shape arr0)
                                              (Array-shape arr1)
                                              (map Array-shape arrs))))
     (let ([arr0  (array-broadcast arr0 ds)]
           [arr1  (array-broadcast arr1 ds)]
           [arrs  (map (plambda: (S) ([arr : (Array S)]) (array-broadcast arr ds)) arrs)])
       (define g0 (Array-unsafe-proc arr0))
       (define g1 (Array-unsafe-proc arr1))
       (define gs (map Array-unsafe-proc arrs))
       (array-default-strict
        (unsafe-build-array
         ds (λ: ([js : Indexes]) (apply f (g0 js) (g1 js)
                                        (map (plambda: (S) ([g : (Indexes -> S)]) (g js)) gs))))))]))