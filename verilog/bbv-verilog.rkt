#lang rosette/safe

(require "../config.rkt"
         "../unity/bbv-parallel.rkt"
         "../unity/bbv-scalar.rkt"
         "../util.rkt"
         "syntax.rkt"
         (only-in racket/base
                  gensym)
         (only-in racket/list
                  range)
         (prefix-in bbv: "../bool-bitvec/types.rkt")
         (prefix-in bbv: "../unity/syntax.rkt")
         rosette/lib/match)


(define (standard-vars clock reset measure)
  (list (cons clock (bbv:in* boolean?))
        (cons reset (bbv:in* boolean?))
        (cons measure bbv:vect?)))

(define (standard-trigger clock reset)
  (or* (posedge* clock)
       (posedge* reset)))

(define (bbv-op->verilog-op op)
  (define table
    (list
     (cons ! not*)
     (cons bvnot bwnot*)
     (cons && and*)
     (cons || or*)
     (cons <=> eq*)
     (cons bveq bweq*)
     (cons bvult lt*)
     (cons bvand bwand*)
     (cons bvor bwor*)
     (cons bvxor bwxor*)
     (cons bvshl shl*)
     (cons bvlshr shr*)
     (cons bvadd add*)))

  (get-mapping op table))

(define (bbv-expr->verilog-expr bbv-expr)
  (match bbv-expr
    [(expression op args ...)
     (begin
       (debug-print (format "bbv-expr->verilog-expr ~a~n" bbv-expr))
       (assert (pair? args))
       (let ([first-arg (car args)]
             [verilog-subexprs (map bbv-expr->verilog-expr args)])
         ;; rosette ite ternary operator is restricted to internal use
         ;; so we can't match against it; but we can detect when it was
         ;; used so...
         (if (concrete-equal? bbv-expr (bbv:bool->vect first-arg))
             (bool->vect* (bbv-expr->verilog-expr first-arg))
             (apply (bbv-op->verilog-op op) verilog-subexprs))))]
    [t t]))

(define (decl->type-decl decl)
  (match decl
    [(cons ident (bbv:in* typ))
     (begin
       (assert (equal? typ boolean?))
       (wire* 1 ident))]
    [(cons ident (bbv:out* typ))
     (begin
       (assert (equal? typ boolean?))
       (reg* 1 ident))]
    [(cons ident (bitvector len))
     (reg* len ident)]))

(define (declare->port-list decls)
  (let ([inputs (filter (lambda (pair) (bbv:in*? (cdr pair))) decls)]
        [outputs (filter (lambda (pair) (bbv:out*? (cdr pair))) decls)])
    (map car (append inputs outputs))))

(define (declare->declarations decls)
  (map decl->type-decl decls))

(define (bbv-stmts->verilog-stmts s)
  (match s
    [(bbv-stmts* stmts _)
     (map (lambda (a)
            (match a
              [(bbv::=* var expr)
               (<=* var (bbv-expr->verilog-expr expr))]))
          stmts)]))

;; deterministic assignments to verilog if-then-else cascade
(define (assigns->verilog-stmts assigns)
  (begin
    (debug-print (format "assigns->verilog-stmts ~a~n" assigns))
    (match assigns
      [(cons assign tail)
       (match assign
         [(bbv-guard-stmts* guard _ stmts)
          (begin
            (assert (= (length guard) 1))
            (let ([verilog-stmts (flatten (map bbv-stmts->verilog-stmts stmts))])
              (if (concrete-equal? (car guard) #t) ;; unconditional statements
                  (append verilog-stmts (assigns->verilog-stmts tail))
                  (if* (bbv-expr->verilog-expr (car guard))
                       verilog-stmts
                       (flatten (list (assigns->verilog-stmts tail)))))))])]
      ['() '()])))

;; Nondeterministic choice over deterministic assignments (nested lists)
(define (choices->verilog-stmts choices measure)
  (let* ([verilog-choices (map assigns->verilog-stmts choices)]
         [choice-count (length verilog-choices)]
         [idxs (range choice-count)])
    (append
     (map (lambda (inner-if idx)
            (if* (bweq* measure idx)
                 (flatten (list inner-if))
                 '()))
          verilog-choices
          idxs)
     (list
      (if* (lt* measure (bbv:vect-literal choice-count))
           (list (<=* measure (add* measure (bbv:vect-literal 1))))
           (list (<=* measure (bbv:vect-literal 0))))))))

(define (initially->assign->always decls inits assigns clock reset measure)
  (list
   (always* (standard-trigger clock reset)
            (list (if* reset
                       (cons
                        (<=* measure (bbv:vect-literal 0))
                        (assigns->verilog-stmts inits))
                       (choices->verilog-stmts assigns measure))))))

(define (scalar->verilog program module-name)
  (define (helper)
    (let ([clock (gensym 'clock)]
          [reset (gensym 'reset)]
          [measure (gensym 'measure)])
      (match program
        [(bbv-scalar* declare initially assign)
         (let ([decls (append (standard-vars clock reset measure) declare)])
           (verilog-module*
            module-name
            (declare->port-list decls)
            (declare->declarations decls)
            (initially->assign->always decls initially assign clock reset measure)))])))

  (if time-compile?
      (begin
        (err-print (format "bbv-scalar->verilog~n"))
        (time (helper)))
      (helper)))


(provide scalar->verilog)
