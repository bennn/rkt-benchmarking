#lang racket

(require mzlib/defmacro)
(require racket/contract)

(define-macro (nest n expr)
  (if (> n 0)
      `(let loop ([i 1]) (unless (> i n)
                           (nest ,(- n 1) ,expr)
                           (loop (add1 i))))
      expr))


(define/contract (main argv)
  (-> (vectorof string?) string?)
  (let* ([n (string->number (vector-ref argv 0))]
         [x 0])
    (nest 6 (set! x (+ x 1)))
    (format "~s\n" x)))

; (time (begin (main (vector "33")) (void)))
(time (begin (main (vector "3")) (void)))
