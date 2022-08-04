#lang rosette/safe

(require "unity/bbv-parallel.rkt"
         "unity/bbv-scalar.rkt"
         "unity/bbv-sequential.rkt"
         "verilog/bbv-verilog.rkt"
         "verilog/backend.rkt"
         "paxos.rkt")

(define (unity->verilog program name)
  (let* ([bbv-par (time (unity->bbv-parallel program))]
         [bbv-scal (time (parallel->scalar bbv-par))]
         [verilog-module (time (scalar->verilog bbv-scal name))])
    (print-verilog-module verilog-module)))

;; Compile an acceptor
(unity->verilog acceptor "acceptor")
;; Compile a proposer
(unity->verilog proposer "proposer")
