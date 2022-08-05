#lang rosette/safe

(require "unity/bbv-parallel.rkt"
         "unity/bbv-scalar.rkt"
         "unity/bbv-sequential.rkt"
         "arduino/bbv-arduino.rkt"
         "arduino/backend.rkt"
         "paxos.rkt")

(define (unity->arduino program)
  (let* ([bbv-par (unity->bbv-parallel program)]
         [bbv-scal (parallel->scalar bbv-par)]
         [bbv-seq (scalar->sequential bbv-scal)]
         [arduino-prog (sequential->arduino bbv-seq)])
    (print-arduino-program arduino-prog)))

;; Compile an acceptor
(unity->arduino acceptor)
;; Compile a proposer
(unity->arduino proposer)
