#lang rosette/safe

(require "bbv-parallel.rkt"
         "bbv-scalar.rkt"
         "syntax.rkt"
         "../config.rkt"
         "../util.rkt"
         (only-in racket/set
                  set
                  set-add
                  set-map
                  set->list)
         (only-in racket/base
                  make-immutable-hash
                  hash-values)
         rosette/lib/match)

(struct bbv-sequential*
  (declare
   initially
   assign)
  #:transparent)

(struct bbv-guard-seq*
  (guard
   decls
   stmts)
  #:transparent)

(struct bbv-seq*
  (decls
   stmts)
  #:transparent)

(struct ordering-model
  (idx+>var
   shadowed-var-set)
  #:transparent)

(define (concretize-index-pair pair)
  ;; Is the index pair still symbolic? Just pick a value then
  (match pair
    [(cons idx var)
     (cons (if (symbolic? idx) 0 idx) var)]))

(define (stmtss->seq stmtss var+>idx)
  (let* ([idx+>var (map (lambda (p) (cons (cdr p) (car p)))
                        var+>idx)]
         [idx->var (lambda (i) (get-mapping-symbolic i idx+>var #f))]
         [inc-solve (solve+)])

    (define (find-ordering-model partial-model shadowed-vars constraints)
      (if (null? constraints)
          (begin
            ;; shutdown does what it says it does
            (inc-solve 'shutdown)
            (ordering-model (sort
                             (map concretize-index-pair
                                  (evaluate idx+>var partial-model))
                             (lambda (l r) (< (car l) (car r))))
                            shadowed-vars))
          (let* ([current-constraint (car constraints)]
                 [tail (cdr constraints)]
                 [model (inc-solve (car constraints))])
            (if (unsat? model)
                ;; conflict found! add rhs variable to shadow set
                (begin
                  (assert (expression? current-constraint))
                  (match current-constraint
                    [(expression op l-idx r-idx)
                     (begin
                       (assert (equal? op <))
                       ;; unwind solver stack one constraint
                       (inc-solve 1)
                       (find-ordering-model partial-model
                                            (set-add shadowed-vars (idx->var r-idx))
                                            tail))]))
                (find-ordering-model model
                                     shadowed-vars
                                     tail)))))

    (let ([all-stmts (apply append (map bbv-stmts*-stmts stmtss))]
          [all-constraints (apply append (map bbv-stmts*-constraints stmtss))])
      (match (find-ordering-model (sat) (set) all-constraints)
        [(ordering-model idx+>var shadowed-var-set)
         (let* ([shadow-hash (make-immutable-hash
                              (set-map
                               shadowed-var-set
                               (lambda (v)
                                 (cons v
                                       (unique-symbolic-constant 'temp (type-of v))))))]
                [shadow-decls (map (lambda (v)
                                     (cons v (type-of v)))
                                   (hash-values shadow-hash))]
                [shadow-model (sat shadow-hash)]
                [shadow-stmts (set-map
                               shadowed-var-set
                               (lambda (v)
                                 (:=* (evaluate v shadow-model) v)))]
                ;; builds a model from vars to their statements, making
                ;; substitutions for shadow variables
                [var+>stmt-model (sat
                                  (make-immutable-hash
                                   (map (lambda (s)
                                          (match s
                                            [(:=* l-var r-exp)
                                             (cons l-var
                                                   (:=* l-var
                                                        (evaluate r-exp
                                                                  shadow-model)))]))
                                        all-stmts)))])
           (bbv-seq*
            shadow-decls
            (append shadow-stmts
                    (flatten
                     (map (lambda (p)
                            (let ([evald-stmt (evaluate (cdr p) var+>stmt-model)])
                              ;; var indices that exist in idx+>var but
                              ;; whose statements were removed (due to stuttering)
                              ;; are removed here
                              (if (constant? evald-stmt)
                                  '()
                                  evald-stmt)))
                         idx+>var)))))]))))

(define (guard-stmts->guard-seq g-s)
  (match g-s
    [(bbv-guard-stmts* guard var+>idx stmtss)
     (let ([seq (stmtss->seq stmtss var+>idx)])
       (bbv-guard-seq* guard
                       (bbv-seq*-decls seq)
                       (bbv-seq*-stmts seq)))]))

;; (define-symbolic w x y z boolean?)
;; (define-symbolic a b c d integer?)
;; (define stmts (bbv-stmts*
;;                (list (<-* w (&& x z))
;;                      (<-* x (&& y z))
;;                      (<-* y x)
;;                      (<-* z x))
;;                (list (< a b)
;;                      (< a d)
;;                      (< b c)
;;                      (< b d)
;;                      (< c b)
;;                      (< d b))))
;; (define var+>idx (list (cons w a) (cons x b) (cons y c) (cons z d)))
;; (stmtss->seq (list stmts) var+>idx)

(define (scalar->sequential program)
  (define (helper)
    (match program
      [(bbv-scalar* declarations initially assignments)
       (let* ([initially-seq (map guard-stmts->guard-seq initially)]
              [assign-seqs (map (lambda (cases)
                                  (map guard-stmts->guard-seq cases))
                                assignments)])
         (bbv-sequential* declarations
                          initially-seq
                          assign-seqs))]))

  (if time-compile?
      (begin
        (err-print (format "bbv-scalar->bbv-sequential~n"))
        (time (helper)))
      (helper)))

(provide bbv-sequential*
         bbv-guard-seq*
         scalar->sequential)
