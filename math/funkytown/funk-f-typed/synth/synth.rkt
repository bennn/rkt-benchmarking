#lang typed/racket

(require (only-in "math/array.rkt"
                  Array
                  array-size
                  array-strictness
                  in-array))

(require "wav-encode.rkt") ;; TODO does not accept arrays directly

;; TODO try to get deforestation for arrays. does that require
;;   non-strict arrays? lazy arrays?
(array-strictness #f)
;; TODO this slows down a bit, it seems, but improves memory use

(provide fs seconds->samples)

(: fs Natural)
(define fs 44100)
(: bits-per-sample Natural)
(define bits-per-sample 16)

(: freq->sample-period (-> Float Natural))
(define (freq->sample-period freq)
  (define i : Integer (round (inexact->exact (round (/ fs freq)))))
  (if (<= 0 i) i (error "unexpectedly negative")))

(: seconds->samples (-> Float Natural))
(define (seconds->samples s)
  (define i : Integer (round (inexact->exact (round (* s fs)))))
  (if (<= 0 i) i (error "unexpectedly negative")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Oscillators

(provide ;sine-wave 
         ;square-wave 
         sawtooth-wave inverse-sawtooth-wave
         triangle-wave)

;; array functions receive a vector of indices
(define-syntax-rule (array-lambda (i) body ...)
  (lambda: ([i* : (Vector Index)]) (let ([i (vector-ref i* 0)]) body ...)))

;; These all need to return floats.
;; TODO use TR? would also optimize for us
;(: sine-wave (-> Natural Natural))
;(define (sine-wave freq)
;  (define f (exact->inexact (/ (* freq 2.0 pi) fs)))
;  (array-lambda (x) (sin (* f (exact->inexact x)))))

;(: square-wave (-> Natural Natural))
;(define (square-wave freq)
;  (: sample-period Integer)
;  (define sample-period (freq->sample-period freq ))
;  (: sample-period/2 Integer)
;  (define sample-period/2 (quotient sample-period 2))
;  (array-lambda (x)
;    ;; 1 for the first half of the cycle, -1 for the other half
;    (: x* Integer)
;    (define x* (modulo x sample-period))
;    (if (> x* sample-period/2) -1.0 1.0)))

(: make-sawtooth-wave (-> Float (-> Float (Vector Natural))))
(define ((make-sawtooth-wave coeff) freq)
  (define sample-period (freq->sample-period freq))
  (define sample-period/2 (quotient sample-period 2))
  (array-lambda (x)
    ;; gradually goes from -1 to 1 over the whole cycle
    (define x* (exact->inexact (modulo x sample-period)))
    (* coeff (- (/ x* sample-period/2) 1.0))))
(: sawtooth-wave (-> Natural (Vector Float)))
(define sawtooth-wave         (make-sawtooth-wave 1.0))
(: inverse-sawtooth-wave (-> Float (Vector Float)))
(define inverse-sawtooth-wave (make-sawtooth-wave -1.0))

(: triangle-wave (-> Float Natural))
(define (triangle-wave freq)
  (: sample-period Natural)
  (define sample-period (freq->sample-period freq))
  (: sample-period/2 Integer)
  (define sample-period/2 (quotient sample-period 2))
  (: sample-period/4 Integer)
  (define sample-period/4 (quotient sample-period 4))
  (array-lambda (x)
    ;; go from 1 to -1 for the first half of the cycle, then back up
    (: x* Integer)
    (define x* (modulo x sample-period))
    (if (> x* sample-period/2)
        (- (/ x* sample-period/4) 3.0)
        (+ (/ x* sample-period/4 -1.0) 1.0))))

;; TODO make sure that all of these actually produce the right frequency
;;  (i.e. no off-by-an-octave errors)

;; TODO add weighted-harmonics, so we can approximate instruments
;;  and take example from old synth

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide emit) ;plot-signal)

;; assumes array of floats in [-1.0,1.0]
;; assumes gain in [0,1], which determines how loud the output is
; TODO encode [range]
(: signal->integer-sequence (->* (Array) (#:gain Float) (Vectorof Natural)))
(define (signal->integer-sequence signal #:gain [gain 1])
  (for/vector #:length (array-size signal)
              ([sample : Natural (in-array signal)])
    (max 0 (min (sub1 (expt 2 bits-per-sample)) ; clamp
                (exact-floor
                 (* gain
                    (* (+ sample 1.0) ; center at 1, instead of 0
                       (expt 2 (sub1 bits-per-sample)))))))))

;(require plot)
;(plot-new-window? #t)
;; shows 2 plots
;; - the original signal
;; - the "digitized" signal, with series for the bounds
;; press any key to dismiss
;(: plot-signal (-> None None))
;(define (plot-signal signal)
;  (define n (array-size signal))
;  (plot (points (for/list ([s (in-array signal)]
;                           [i (in-naturals)])
;                  (vector i s))))
;  (plot (list (points (for/list ([s (in-vector
;                                     (signal->integer-sequence signal))]
;                                 [i (in-naturals)])
;                        (vector i s)))
;              (points (for/list ([i (in-range n)])
;                        (vector i 0)))
;              (points (for/list ([i (in-range n)])
;                        (vector i (expt 2 bits-per-sample))))))
;  (read-char))

(: emit (-> Array String Void))
(define (emit signal file)
  (with-output-to-file file #:exists 'replace
    (lambda () (write-wav (signal->integer-sequence signal #:gain 0.3)))))
