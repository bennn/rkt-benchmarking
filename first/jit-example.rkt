#lang racket

; Example from benchmark package docs
; http://pkg-build.racket-lang.org/doc/benchmark/index.html

(require benchmark
         plot
         racket
         racket/runtime-path)

(define-runtime-path fib-path
  "examples/fib.rkt")

(define-runtime-path collatz-path
  "examples/collatz.rkt")

(define-runtime-path compiled-dir
  "examples/compiled")

(define results
  (run-benchmarks
    ; file to run
    (list fib-path collatz-path)
    ; list of options
    (list (list 'jit 'no-jit))
    ; how to run each benchmark
    (lambda (file jit)
      (if (equal? jit 'jit)
        (system* (find-executable-path "racket") file)
        (system* (find-executable-path "racket") "-j" file)))
    #:build
    (lambda (file jit)
      (system* (find-executable-path "raco") "make" file))
    #:clean
    (lambda (file jit)
      (system* (find-executable-path "rm") "-r" "-f" compiled-dir))
    #:num-trials 30
    #:make-name (lambda (path)
                    (let-values ([(a file-name c) (split-path path)])
                    (path->string file-name)))))

(parameterize ([plot-x-ticks no-ticks])
  (plot;-pict
    #:title "jit vs no-jit"
    #:x-label #f
    #:y-label "normalized-time"
    #:out-file "output.png"
    (render-benchmark-alts
      ; default options
      (list 'jit)
      results)))

;; BOTH EXAMPLES fib + collatz explicitly call "time"
;; because time of a benchmark is, by default, the time reported to stdout
