#lang rosette/safe

(require "../bool-bitvec/types.rkt"
         "../util.rkt"
         rosette/lib/match
         (only-in racket/base
                  build-list
                  values))

(define-struct ordering
  (before
   after)
  #:transparent)

;; Takes a tuple and default tuple of the same size, and returns a map, for each
;; element x if the tuple, maps the index of x to x projected into the default
;; tuple.
;; e.g.: (projections '(a b) '(d0 d1)) = '((0 . (a d1)) (1 . (d0 b)))
(define (projections tuple default)
  (begin
    (assert (equal? (length tuple)
                    (length default)))
    (apply append
           (map (lambda (idx)
                  (let ([tuple-value (list-ref tuple idx)]
                        [default-value (list-ref default idx)])
                    (if (concrete-equal? tuple-value default-value)
                        '()
                        (list (cons idx
                                    (list-set default idx tuple-value))))))
                (build-list (length tuple) values)))))

;; Solves for an ordering that preserves refinement simulation
(define (refinement-ordering precondition pretuple posttuple refinement-mapping)
  (define (helper preidxs postidxs projs)
    (if (null? projs)
        (begin
          (assert (= 1 (length postidxs)))
          (map (lambda (preidx)
                 (ordering preidx
                           (car postidxs)))
               preidxs))
        (match projs
          [(cons (cons idx projection) tail)
           (if (unsat?
                (vc-wrapper
                 (verify (begin (assume precondition)
                                (assert (equal? (apply refinement-mapping pretuple)
                                                (apply refinement-mapping projection)))))))
               (helper (cons idx preidxs)
                       postidxs
                       tail)
               (helper preidxs
                       (cons idx postidxs)
                       tail))])))

  (helper '() '() (projections posttuple pretuple)))

(provide ordering
         ordering-before
         ordering-after
         refinement-ordering)
