#lang rosette/safe

(require "bbv-parallel.rkt"
         "bbv-refinement.rkt"
         "syntax.rkt"
         "../util.rkt"
         rosette/lib/match)

;; Turn bbv-unity multi-assignments into correctly ordered imperative assignments
;; (:=*
;;  (list (list in-read96266) (list out96270 out96271 out96272))
;;  (list
;;   (bbv-guard-exprs*
;;    (list
;;     (&& (! in-read96266) (&& (<=> out96270 out96271) (|| (&& in96268 (! in96267)) (&& in96267 (! in96268))))))
;;    (list
;;     (bbv-exprs* '(#t) '())
;;     (bbv-exprs*
;;      (list (! out96271) out96271 in96269)
;;      (list (ordering 2 0)))))))

;; bbv-unity-(multi-)assignments to guarded-bbv-(single-)assignments
;; intermediate data structure: lhs var -> symbolic integer
;; convert bbv-expr ordering from (ordering rhs-idx rhs-idx) to (< rhs-idx-int rhs-idx-int)

(struct bbv-scalar*
  (declare
   initially
   assign)
  #:transparent)

(struct bbv-guard-stmts*
  (guard
   var+>idx
   stmts)
  #:transparent)

(struct bbv-stmts*
  (stmts
   constraints)
  #:transparent)

(define (vars+>order-indices vars)
  (map (lambda (v)
         (cons v (new-symbolic-constant integer?)))
       (flatten vars)))

(define (bbv-exprs->bbv-stmts var-tuple exprs var->idx)
  (define (constraint->indexed-constraint constraint)
    (match constraint
      [(ordering before after)
       (< (var->idx (list-ref var-tuple before))
          (var->idx (list-ref var-tuple after)))]))

  (define (stmt->dependencies stmt)
    (match stmt
      [(:=* lhs rhs)
       (let ([lhs-idx (var->idx lhs)])
         (map
          (lambda (rhs-var)
            (let ([rhs-idx (var->idx rhs-var)])
              (if (or (concrete-equal? rhs-idx #f)
                      (concrete-equal? lhs-idx rhs-idx))
                  '()
                  (list (< lhs-idx rhs-idx)))))
          (symbolics rhs)))]))

  (match exprs
    [(bbv-exprs* expr-tuple constraints)
     (begin
       (assert (= (length var-tuple)
                  (length expr-tuple)))
       (let* ([stmts (flatten
                      (map (lambda (var expr)
                             ;; Remove "stuttering" assignments
                             (if (concrete-equal? var expr)
                                 '()
                                 (:=* var expr)))
                           var-tuple
                           expr-tuple))]
              [dependencies (flatten
                             (map stmt->dependencies stmts))]
              [indexed-constraints (map constraint->indexed-constraint
                                        constraints)])
         (bbv-stmts* stmts
                     (append dependencies indexed-constraints))))]))

(define (guard-exprs->guard-statements var-tuples guard-exprs var+>idx var->idx)
  (match guard-exprs
    [(bbv-guard-exprs* guard-tuple exprss)
     (begin
       (assert (= (length var-tuples)
                  (length exprss)))
       (bbv-guard-stmts* guard-tuple
                         var+>idx
                         (map (lambda (v e)
                                (bbv-exprs->bbv-stmts v e var->idx))
                              var-tuples exprss)))]))

(define (parallel-assign->scalar-assign multi-assign)
  (match multi-assign
    [(:=* var-tuples guard-exprss)
     (let* ([var+>idx (vars+>order-indices var-tuples)]
            [var->idx (lambda (v)
                        (get-mapping-symbolic v var+>idx #f))])

       (map (lambda (g-e)
              (guard-exprs->guard-statements var-tuples g-e var+>idx var->idx))
            guard-exprss))]))

(define (parallel->scalar program)
  (match program
    [(bbv-parallel* declarations initially assignments)
     (bbv-scalar* declarations
                  (parallel-assign->scalar-assign initially)
                  (map parallel-assign->scalar-assign assignments))]))

(provide bbv-scalar*
         bbv-guard-stmts*
         bbv-stmts*
         bbv-stmts*-stmts
         bbv-stmts*-constraints
         parallel->scalar)
