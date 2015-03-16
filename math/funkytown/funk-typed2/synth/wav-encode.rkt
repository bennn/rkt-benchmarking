#lang typed/racket/base

;; Simple WAVE encoder

;; Very helpful reference:
;; http://ccrma.stanford.edu/courses/422/projects/WaveFormat/

(provide write-wav)

;; (require (only-in racket/sequence sequence-length))
(: sequence-length (-> (Sequenceof Any) Natural))
(define (sequence-length s)
  (unless (sequence? s) (raise-argument-error 'sequence-length "sequence?" s))
  (for/fold ([c : Natural 0]) ([i : Any (in-values*-sequence s)])
    (add1 c)))


;; A WAVE file has 3 parts:
;;  - the RIFF header: identifies the file as WAVE
;;  - fmt subchunk: describes format of the data subchunk
;;  - data subchunk

;; data : sequence of 32-bit unsigned integers
(: write-wav (->* ((Vectorof Natural)) (#:num-channels Natural
                                      #:sample-rate  Natural
                                      #:bits-per-sample Natural) Void))
(define (write-wav data
                   #:num-channels    [num-channels    1] ; 1 = mono, 2 = stereo
                   #:sample-rate     [sample-rate     44100]
                   #:bits-per-sample [bits-per-sample 16])

  (: bytes-per-sample Natural)
  (define bytes-per-sample (quotient bits-per-sample 8))
  (: write-integer-bytes (->* (Natural) (Natural) Natural))
  (define (write-integer-bytes i [size 4])
    (write-bytes (integer->integer-bytes i size #f)))
  (: data-subchunk-size Natural)
  (define data-subchunk-size
    (* (sequence-length data) num-channels (floor (/ bits-per-sample 8))))

  ;; RIFF header
  (write-bytes #"RIFF")
  ;; 4 bytes: 4 + (8 + size of fmt subchunk) + (8 + size of data subchunk)
  (write-integer-bytes (+ 36 data-subchunk-size))
  (write-bytes #"WAVE")

  ;; fmt subchunk
  (write-bytes #"fmt ")
  ;; size of the rest of the subchunk: 16 for PCM
  (write-integer-bytes 16)
  ;; audio format: 1 = PCM
  (write-integer-bytes 1 2)
  (write-integer-bytes num-channels 2)
  (write-integer-bytes sample-rate)
  ;; byte rate
  (write-integer-bytes (* sample-rate num-channels bytes-per-sample))
  ;; block align
  (write-integer-bytes (* num-channels bytes-per-sample) 2)
  (write-integer-bytes bits-per-sample 2)

  ;; data subchunk
  (write-bytes #"data")
  (write-integer-bytes data-subchunk-size)
  (for ([sample : Natural data])
    (write-integer-bytes sample bytes-per-sample)))
