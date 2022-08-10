#lang rosette/safe

(require (only-in racket/base
                  error-print-width
                  current-command-line-arguments)
         (only-in racket
                  hash)
         (only-in racket/vector
                  vector-member)
         rosette/solver/smt/z3
         rosette/solver/smt/boolector)

(current-solver (z3 #:logic 'QF_BV
                    #:options (hash
                               ':parallel.threads.max 4
                               ':parallel.enable 'true
                               ':timeout 120000)))

;;(output-smt "/Users/cchen/Desktop/smt-output")
(error-print-width (expt 2 16))

(define (flag-present? flag)
  (integer?
   (vector-member flag (current-command-line-arguments))))

;; debug mode
(define debug? (flag-present? "--debug"))

;; enable timing for compilation
(define time-compile? (flag-present? "--time-compile"))

;; enable timing for rule synthesis
(define time-synth? (flag-present? "--time-synth"))

;; enable timing for rule synthesis
(define time-noop? (flag-present? "--time-noop"))

;; Set bitwidth for Arduino/Verilog models
(define vect-len 32)

;; Constrains integer theory to bitvectors of size N
(current-bitwidth 64)

;; Maximum synthesis expression search depth
(define max-expression-depth 4)

(provide debug?
         time-compile?
         time-synth?
         time-noop?
         vect-len
         max-expression-depth)
