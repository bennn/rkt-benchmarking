;;; heapsort.scm

;; Prints 0.9990640717878372 instead of 0.9990640718 when n=1000.
;; Updated by Justin Smith
;;
;; Updated by Brent Fulgham to provide proper output formatting

#lang racket/base
(require racket/contract)

(require (only-in srfi/13 string-index string-pad-right)
         (only-in mzlib/string real->decimal-string))

(define IM   139968)
(define IA     3877)
(define IC    29573)

(define nat? natural-number/c)

(define LAST 42)
(define/contract (gen_random max)
  (-> number? number?)
  (set! LAST (modulo (+ (* LAST IA) IC) IM))
  (/ (* max LAST) IM))

(define/contract (heapsort n ra)
  (-> nat? (vectorof number?) boolean?)
  (let ((ir n)
        (l (+ (quotient n 2) 1))
        (i 0)
        (j 0)
        (rra 0.0))
    (let/ec return
    (begin
      (do ((bar #t))
          ((= 1 0))
        (cond ((> l 1)
               (set! l (- l 1))
               (set! rra (vector-ref ra l)))
              (else
               (set! rra (vector-ref ra ir))
               (vector-set! ra ir (vector-ref ra 1))
               (set! ir (- ir 1))
               (cond ((<= ir 1)
                      (vector-set! ra 1 rra)
                      (return #t)))))
        (set! i l)
        (set! j (* l 2))
        (do ((foo #t))
            ((> j ir))
          (cond ((and (< j ir) (< (vector-ref ra j) (vector-ref ra (+ j 1))))
                 (set! j (+ j 1))))
          (cond ((< rra (vector-ref ra j))
                 (vector-set! ra i (vector-ref ra j))
                 (set! i j)
                 (set! j (+ j i)))
                (else
                 (set! j (+ ir 1)))))
        (vector-set! ra i rra))))))

(define/contract (main args)
  (-> (vectorof string?) string?)
  (let* ((n (or (and (= (vector-length args) 1) (string->number (vector-ref args 0)))
                1))
         (last (+ n 1))
         (ary (make-vector last 0)))
    (define/contract (do1 i)
      (-> nat? void?)
      (vector-set! ary i (gen_random 1.0)))
    (do ((i 1 (+ i 1)))
        ((= i last))
        (do1 i))
    (heapsort n ary)
    (format "~a\n"
            (real->decimal-string (vector-ref ary n) 10))))

;(time (begin (main (vector "2500000")) (void)))
(time (begin (main (vector "25")) (void)))
