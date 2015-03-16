#lang racket/base

(require racket/performance-hint
         racket/fixnum
         "../unsafe.rkt"
         (only-in "array-struct.rkt"
                  array-default-strict
                  Array
                  array-shape
                  unsafe-array-proc
                  unsafe-build-array
                  array-strictness
                  )
         (only-in "typed-array-indexing.rkt" array-ref)
         "utils.rkt")

(provide array/c)

;; ===================================================================================================
;; Per-axis folds

; (: array-dims (All (A) ((Array A) -> Index)))                                   
(define (array-dims arr)                                                        
  (vector-length (array-shape arr)))

;(: check-array-axis (All (A) (Symbol (Array A) Integer -> Index)))
(define (check-array-axis name arr k)
  (define dims (array-dims arr))
  (cond
    [(fx= dims 0)  (raise-argument-error name "Array with at least one axis" 0 arr k)]
    [(or (0 . > . k) (k . >= . dims))
     (raise-argument-error name (format "Index < ~a" dims) 1 arr k)]
    [else  k]))

;(: unsafe-array-axis-reduce (All (A B) ((Array A) Index (Index (Index -> A) -> B) -> (Array B))))
(begin-encourage-inline
  (define (unsafe-array-axis-reduce arr k f)
    (define ds (array-shape arr))
    (define dk (unsafe-vector-ref ds k))
    (define new-ds (unsafe-vector-remove ds k))
    (define proc (unsafe-array-proc arr))
    (unsafe-build-array
     new-ds (λ (js) ;: ([js : Indexes])
              (define old-js (unsafe-vector-insert js k 0))
              (f dk (λ (jk);: ([jk : Index])
                      (unsafe-vector-set! old-js k jk)
                      (proc old-js)))))))

;(: array-axis-reduce (All (A B) ((Array A) Integer (Index (Integer -> A) -> B) -> (Array B))))
(define (array-axis-reduce arr k f)
  (let ([k  (check-array-axis 'array-axis-reduce arr k)])
    (array-default-strict
     (unsafe-array-axis-reduce
      arr k
      (λ (dk proc);: ([dk : Index] [proc : (Index -> A)])
        ;(: safe-proc (Integer -> A))
        (define (safe-proc jk)
          (cond [(or (jk . < . 0) (jk . >= . dk))
                 (raise-argument-error 'array-axis-reduce (format "Index < ~a" dk) jk)]
                [else  (proc jk)]))
        (f dk safe-proc))))))

;(: array-axis-fold/init (All (A B) ((Array A) Integer (A B -> B) B -> (Array B))))
(define (array-axis-fold/init arr k f init)
  (let ([k  (check-array-axis 'array-axis-fold arr k)])
    (unsafe-array-axis-reduce
     arr k (λ (dk proc);: ([dk : Index] [proc : (Index -> A)])
             (let;: loop : B ([jk : Nonnegative-Fixnum  0] [acc : B  init])
                 loop ([jk 0] [acc init])
               (cond [(jk . fx< . dk)  (loop (fx+ jk 1) (f (proc jk) acc))]
                     [else  acc]))))))

;(: array-axis-fold/no-init (All (A) ((Array A) Integer (A A -> A) -> (Array A))))
(define (array-axis-fold/no-init arr k f)
  (let ([k  (check-array-axis 'array-axis-fold arr k)])
    (when (fx= (unsafe-vector-ref (array-shape arr) k) 0)
      (raise-argument-error 'array-axis-fold "nonzero axis" 0 arr k))
    (unsafe-array-axis-reduce
     arr k (λ (dk proc);: ([dk : Index] [proc : (Index -> A)])
             (let;: loop : A ([jk : Nonnegative-Fixnum  1] [acc : A  (proc 0)])
                 loop ([jk 1] [acc (proc 0)])
               (cond [(jk . fx< . dk)  (loop (fx+ jk 1) (f (proc jk) acc))]
                     [else  acc]))))))

;(: array-axis-fold (All (A B) (case-> ((Array A) Integer (A A -> A) -> (Array A))
;                                      ((Array A) Integer (A B -> B) B -> (Array B)))))
(define array-axis-fold
  (case-lambda
    [(arr k f)  (array-default-strict (array-axis-fold/no-init arr k f))]
    [(arr k f init)  (array-default-strict (array-axis-fold/init arr k f init))]))

;; ===================================================================================================
;; Whole-array folds

(begin-encourage-inline
  
  ;(: array-fold (All (A) ((Array A) ((Array A) Index -> (Array A)) -> (Array A))))
  (define (array-fold arr f)
    (define dims (array-dims arr))
    (let loop ([k dims] [arr arr]);([#{k : Index} dims] [arr arr])
      (cond [(fx= k 0)  arr]
            [else  (let ([k  (fx- k 1)])
                     (loop k (f arr k)))])))
  
;  (: array-all-fold (All (A) (case-> ((Array A) (A A -> A) -> A)
;                                     ((Array A) (A A -> A) A -> A))))
  (define array-all-fold
    (case-lambda
      [(arr f)
       ;; Though `f' is folded over multiple axes, each element of `arr' is referred to only once, so
       ;; turning strictness off can't hurt performance
       (parameterize ([array-strictness #f])
         (array-ref (array-fold arr (λ (arr k);: ([arr : (Array A)] [k : Index])
                                      (array-axis-fold arr k f)))
                    #()))]
      [(arr f init)
       ;; See above for why non-strictness is okay
       (parameterize ([array-strictness #f])
         (array-ref (array-fold arr (λ (arr k);: ([arr : (Array A)] [k : Index])
                                      (array-axis-fold arr k f init)))
                    #()))]))
  
  )  ; begin-encourage-inline

;; ===================================================================================================
;; Contract

(define ((array/c elem?) arr)
  (array-all-fold arr (lambda (acc elem) (and acc (elem? elem))) #t))
