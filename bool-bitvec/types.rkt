#lang rosette/safe

(require "../config.rkt"
         "../semantics.rkt"
         "../util.rkt"
         rosette/lib/match)

(define fn:bool->bool
  (list !))

(define fn:vect->vect
  (list bvnot))

(define fn:bool->bool->bool
  (list &&
        ||
        <=>
        xor))

(define fn:vect->vect->bool
  (list bveq
        bvult))

(define fn:vect->vect->vect
  (list bvand
        bvor
        bvxor
        bvshl
        bvlshr
        bvadd))

(define (sig:bool->bool fn)
  (-> fn (list boolean?) boolean?))

(define (sig:vect->vect fn)
  (-> fn (list vect?) vect?))

(define (sig:bool->bool->bool fn)
  (-> fn (list boolean? boolean?) boolean?))

(define (sig:vect->vect->bool fn)
  (-> fn (list vect? vect?) boolean?))

(define (sig:vect->vect->vect fn)
  (-> fn (list vect? vect?) vect?))

(define (op->sig op)
  (cond
    [(in-list? op fn:bool->bool) (sig:bool->bool op)]
    [(in-list? op fn:vect->vect) (sig:vect->vect op)]
    [(in-list? op fn:bool->bool->bool) (sig:bool->bool->bool op)]
    [(in-list? op fn:vect->vect->bool) (sig:vect->vect->bool op)]
    [(in-list? op fn:vect->vect->vect) (sig:vect->vect->vect op)]))

(define vect?
  (bitvector vect-len))

(define (vect-literal n)
  (bv n vect-len))

(define (bool->vect b)
  (bool->bitvector b vect-len))

(define false-vect
  (bool->vect #f))

(define true-vect
  (bool->vect #t))

(provide fn:bool->bool
         fn:vect->vect
         fn:bool->bool->bool
         fn:vect->vect->bool
         fn:vect->vect->vect
         sig:bool->bool
         sig:vect->vect
         sig:bool->bool->bool
         sig:vect->vect->bool
         sig:vect->vect->vect
         op->sig
         vect?
         vect-literal
         bool->vect
         false-vect
         true-vect)
