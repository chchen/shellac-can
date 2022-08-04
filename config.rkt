#lang rosette/safe

(require (only-in racket/base error-print-width)
         (only-in racket hash)
         rosette/solver/smt/z3
         rosette/solver/smt/boolector)

(current-solver (z3 #:logic 'QF_BV
                    #:options (hash
                               ':parallel.threads.max 4
                               ':parallel.enable 'true
                               ':timeout 120000)))

;;(output-smt "/Users/cchen/Desktop/smt-output")
(error-print-width (expt 2 16))

;; debug mode
(define debug? #f)

;; Set bitwidth for Arduino/Verilog models
(define vect-len 32)

;; Constrains integer theory to bitvectors of size N
(current-bitwidth 64)

;; Maximum synthesis expression search depth
(define max-expression-depth 4)

(provide debug?
         vect-len
         max-expression-depth)
