# The Shellac Compiler Synthesizer for Concurrent Programs

## Repository layout

* /arduino/: Arduino syntax model and backend
    * backend.rkt: Backend to native syntax
    * bbv-arduino.rkt: Conversion from bbv-sequential to Arduino
    * syntax.rkt
* /bool-bitvec/: Boolean-Bitvector embedding
    * inversion.rkt: Symbolic syntax tree generation
    * types.rkt
* /unity/: UNITY embedding
    * bbv-parallel.rkt: Conversion from UNITY to bbv-parallel
    * bbv-refinement.rkt: Refinement relation constraints
    * bbv-scalar.rkt: Conversion from bbv-parallel to scalar
    * bbv-sequential.rkt: Conversion from bbv-scalar to sequential
    * semantics.rkt: UNITY expression syntax semantics
    * syntax.rkt
* /verilog/: Verilog syntax model and backend
    * backend.rkt: Backend to native syntax
    * bbv-verilog.rkt: Conversion from bbv-scalar to Verilog
    * syntax.rkt
* /FPGA-notes.org: CAD tool notes for Arduino MKR Vidor 4000 board
* /LICENSE
* /README.md
* /config.rkt: Global config
* /paxos-arduino.rkt: Compilation harness for Arduino Paxos
* /paxos-verilog.rkt: Compilation harness for Verilog Paxos
* /paxos.rkt: UNITY specifications for Paxos
* /semantics.rkt
* /util.rkt

## Getting started

### Requirements
* <a href = "https://download.racket-lang.org/">Racket 8.0 or higher</a>
* Rosette 4.0 or higher -- if racket is properly installed, the following command should will install Rosette: 'raco pkg install rosette'. (Note: if an earlier version of racket is installed on your system, be sure to explicitly use the later version you installed.)
* Z3 (included with Rosette distribution)

### Running the experiments

#### Rule synthesis timing

Execute the `bbv-parallel` module with the `--time-synth` option: `racket unity/bbv-parallel.rkt --time-synth`.

#### Paxos compilation timing

Execute the `paxos-arduino` and `paxos-verilog` modules with the `--time-compile` option: `racket paxos-arduino.rkt --time-compile ; racket paxos-verilog.rkt --time-compile`.

### Writing your own specs

1. Take a look at the existing Paxos specification: `paxos.rkt`. In addition, take a look at the UNITY syntax: `unity/syntax.rkt`.
2. Use one of the existing specifications as scaffolding for writing your own.
3. Use one of the existing compilation harnesses as scaffolding for targetting Arduino or Verilog.
4. For FPGA-specific info, see `FPGA-notes.org`.
5. Run it!
