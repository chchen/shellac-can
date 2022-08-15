#lang rosette/safe

(require "../config.rkt"
         "../unity/bbv-parallel.rkt"
         "../unity/bbv-scalar.rkt"
         "../unity/bbv-sequential.rkt"
         "../util.rkt"
         "syntax.rkt"
         (only-in racket/list
                  range)
         (prefix-in bbv: "../bool-bitvec/types.rkt")
         (prefix-in bbv: "../unity/syntax.rkt")
         rosette/lib/match)


(define pin-count 22)

;; Reserved pins for serial monitor
(define reserved-pins (list 13 14))

(define pins
  (remove* reserved-pins
           (range pin-count)))

(define (bbv-ghost-decl->arduino-stmt decl)
  (match decl
    [(cons ident typ)
     (let ([arduino-typ (cond [(equal? typ boolean?) bool*]
                              [(equal? typ bbv:vect?) unsigned-int*])])
       (arduino-typ ident))]))

(define (bbv-decl->arduino-stmt decl available-pins)
  (begin
    (assert (pair? available-pins))
    (match decl
      [(cons ident (bbv:in* _)) (cons (const-int* ident (car available-pins))
                                      (cdr available-pins))]
      [(cons ident (bbv:out* _)) (cons (const-int* ident (car available-pins))
                                       (cdr available-pins))]
      [(cons ident typ)
       (let ([arduino-typ (cond [(equal? typ boolean?) bool*]
                                [(equal? typ bbv:vect?) unsigned-int*])])
         (cons (arduino-typ ident)
               available-pins))])))

(define (bbv-inout-decl->arduino-stmt decl)
  (match decl
    [(cons ident (bbv:in* _)) (pin-mode* ident 'INPUT)]
    [(cons ident (bbv:out* _)) (pin-mode* ident 'OUTPUT)]))

(define (bbv-op->arduino-op op)
  (define table
    (list
     (cons ! not*)
     (cons bvnot bwnot*)
     (cons && and*)
     (cons || or*)
     (cons <=> bool-eq*)
     (cons bveq int-eq*)
     (cons bvult lt*)
     (cons bvand bwand*)
     (cons bvor bwor*)
     (cons bvxor bwxor*)
     (cons bvshl shl*)
     (cons bvlshr shr*)
     (cons bvadd add*)))

  (get-mapping op table))

(define (bbv-expr->arduino-expr bbv-expr decls)
  ;; read function required to access input and output pins
  (define (var-literal->arduino-var-expr var)
    (match (get-mapping-symbolic var decls)
      [(bbv:in* _) (read* var)]
      [(bbv:out* _) (read* var)]
      [_ var]))

  (define (helper expr)
    (match expr
      [(expression op args ...)
       (begin
         (debug-print (format "bbv-expr->arduino-expr ~a~n" expr))
         (assert (pair? args))
         (let ([first-arg (car args)]
               [arduino-subexprs (map helper args)])
           ;; rosette ite ternary operator is restricted to internal use
           ;; so we can't match against it; but we can detect when it was
           ;; used so...
           (if (concrete-equal? expr (bbv:bool->vect first-arg))
               (helper first-arg)
               (apply (bbv-op->arduino-op op) arduino-subexprs))))]
      [t (var-literal->arduino-var-expr t)]))

  (helper bbv-expr))

(define (bbv-stmt->arduino-stmt stmt decls)
  (begin
    (debug-print (format "bbv-stmt->arduino-stmt ~a~n" stmt))
    (match stmt
      [(bbv::=* var expr)
       ;; assigning values to a pin requires write
       (let ([op (match (get-mapping-symbolic var decls)
                   [(bbv:out* _) write*]
                   [_ :=*])]
             [arduino-expr (bbv-expr->arduino-expr expr decls)])
         (op var arduino-expr))])))

(define (bbv-decls->arduino-setup decls initially)
  (define (inout-decl? decl)
    (match decl
      [(cons _ (bbv:in* _)) #t]
      [(cons _ (bbv:out* _)) #t]
      [_ #f]))

  (define (decls->arduino-type-stmts ds [ts '()] [remaining-pins pins])
    (if (null? ds)
        ts
        (let* ([bbv-decl (car ds)]
               [stmt-pins (bbv-decl->arduino-stmt bbv-decl remaining-pins)]
               [arduino-stmt (car stmt-pins)]
               [next-pins (cdr stmt-pins)])
          (decls->arduino-type-stmts (cdr ds)
                                     (cons arduino-stmt ts)
                                     next-pins))))

  (define (decls->arduino-pin-stmts ds)
    (map bbv-inout-decl->arduino-stmt
         (filter inout-decl? decls)))

  (define (initially->arduino-stmts initially)
    (begin
      (assert (= (length initially) 1))
      (match (car initially)
        [(bbv-guard-seq* guard ghosts stmts)
         (begin
           (assert (concrete-equal? guard (list #t)))
           (assert (null? ghosts))
           (map (lambda (s)
                  (bbv-stmt->arduino-stmt s decls))
                stmts))])))

  (setup* (append (decls->arduino-type-stmts decls)
                  (decls->arduino-pin-stmts decls)
                  (initially->arduino-stmts initially))))

;; deterministic assignments to arduino if-then-else cascade
(define (assigns->arduino-stmts assigns decls)
  (begin
    (debug-print (format "assigns->arduino-stmts ~a~n" assigns))
    (match assigns
      [(cons assign tail)
       (match assign
         [(bbv-guard-seq* guard ghosts stmts)
          (begin
            (debug-print (format "stmts: ~a~n" stmts))
            (assert (= (length guard) 1))
            (let ([arduino-guard (bbv-expr->arduino-expr (car guard) decls)]
                  [ghost-stmts (map bbv-ghost-decl->arduino-stmt ghosts)]
                  [arduino-stmts (map (lambda (s)
                                        (bbv-stmt->arduino-stmt s decls))
                                       stmts)])
              (if (concrete-equal? (car guard) #t) ;; unconditional statements
                  (append ghost-stmts
                          arduino-stmts
                          (flatten (list (assigns->arduino-stmts tail decls))))
                  (if* arduino-guard
                       (append ghost-stmts
                               arduino-stmts)
                       (flatten (list (assigns->arduino-stmts tail decls)))))))])]
      ['() '()])))

(define (bbv-assigns->arduino-loop assigns decls)
  (loop* (map (lambda (a)
                (assigns->arduino-stmts a decls))
              assigns)))

(define (sequential->arduino program)
  (define (helper)
    (match program
      [(bbv-sequential* decls initially assigns)
       (arduino*
        (bbv-decls->arduino-setup decls initially)
        (bbv-assigns->arduino-loop assigns decls))]))

  (if time-compile?
      (begin
        (err-print (format "bbv-sequential->arduino~n"))
        (time (helper)))
      (helper)))


(provide sequential->arduino)
