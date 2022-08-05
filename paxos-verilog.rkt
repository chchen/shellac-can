#lang rosette/safe

(require "unity/bbv-parallel.rkt"
         "unity/bbv-scalar.rkt"
         "unity/bbv-sequential.rkt"
         "verilog/bbv-verilog.rkt"
         "verilog/backend.rkt"
         "paxos.rkt")

(define (unity->verilog program name)
  (let* ([bbv-par (unity->bbv-parallel program)]
         [bbv-scal (parallel->scalar bbv-par)]
         [verilog-module (scalar->verilog bbv-scal name)])
    (print-verilog-module verilog-module)))

;; Compile an acceptor
(unity->verilog acceptor "acceptor")
;; Compile a proposer
(unity->verilog proposer "proposer")
