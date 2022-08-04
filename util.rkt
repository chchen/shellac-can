#lang rosette/safe

(require "config.rkt"
         rosette/lib/match
         (only-in racket/base
                  gensym))

(define (debug-print fmt)
  (begin
    (if debug?
        (display fmt (current-error-port))
        (void))))

(define (concrete-eq? a b)
  (let ([val (eq? a b)])
    (and val
         (concrete? val))))

(define (concrete-equal? a b)
  (let ([val (equal? a b)])
    (and (concrete? val)
         val)))

(define (equal-length? a b)
  (and (list? a)
       (list? b)
       (eq? (length a)
            (length b))))

(define (in-list? val l)
  (ormap (lambda (x) (eq? val x)) l))

(define (in-symbolic-list? val l)
  (ormap (lambda (x) (concrete-eq? val x)) l))

(define (intersection l r)
  (define (in-r elem)
    (in-symbolic-list? elem r))

  (filter in-r l))

(define (zip l r)
  (map cons l r))

;; We use a unified state representation for our models, a associative list from
;; symbol to value.

(define (mapping? map)
  (if (null? map)
      #t
      (and (pair? map)
           (pair? (car map))
           (mapping? (cdr map)))))

(define (get-mapping key mapping)
  (match (assoc key mapping)
    [(cons _ val) val]
    [_ null]))

(define (get-mapping-symbolic key mapping [default null])
  (if (null? mapping)
      default
      (if (concrete-eq? key (caar mapping))
          (cdar mapping)
          (get-mapping-symbolic key (cdr mapping) default))))

(define (add-mapping key val mapping)
  (cons (cons key val)
        mapping))

;; Extract a subset of a mapping given a list of keys
(define (subset-mapping keys mapping)
  (filter (lambda (k-v)
            (member (car k-v) keys))
          mapping))

;; Extract a subset of a mapping given a list of keys to exclude
(define (inverse-subset-mapping keys mapping)
  (filter (lambda (k-v)
            (not (member (car k-v) keys)))
          mapping))

;; Extract keys from a mapping
(define (keys mapping)
  (map car mapping))

;; Extract vals from a mapping
(define (vals mapping)
  (map cdr mapping))

;; Test if maps are equal given up to a list of keys
(define (map-eq-modulo-keys? keys map-l map-r)
  (define (val-eq? k)
    (eq? (get-mapping k map-l)
         (get-mapping k map-r)))

  (andmap val-eq? keys))

;; type:Symbol -> cxt:List -> List[id:Symbol]
;; where (cons id type) in cxt
(define (type-in-context typ cxt)
  (map car
       (filter (lambda (pair)
                 (eq? typ
                      (cdr pair)))
               cxt)))

;; Indentation for pretty-printing
(define (pretty-indent items pre-fix)
  (if (null? items)
      '()
      (cons (if (pair? (car items))
                (pretty-indent (car items) (format "  ~a" pre-fix))
                (format "~a~a" pre-fix (car items)))
            (pretty-indent (cdr items) pre-fix))))

;; Ugh a list prefix function
(define (prefixes lst)
  (define (helper l acc)
    (if (null? l)
        (list acc)
        (cons acc
              (helper (cdr l)
                      (append acc (list (car l)))))))

  (helper lst '()))

;; Verification condition wrapper for expression, leaving current vc unchanged
(define-syntax vc-wrapper
  (syntax-rules ()
    [(vc-wrapper expr) (result-value (with-vc expr))]))

(define (union-pick-head val)
  (if (union? val)
      (cdar (union-contents val))
      val))

;; Given a trace sequence, return the trace sequence without the tail
(define (trim-trace trace tail)
  (if (eq? trace tail)
      '()
      (cons (car trace)
            (trim-trace (cdr trace) tail))))

;; Break a formula up into a list of conjuncts
(define (formula->conjuncts formula)
  (define (helper f)
    (match f
      [(expression op args ...)
       (if (equal? op &&)
           (map formula->conjuncts args)
           f)]
      [x (list x)]))

  (flatten (helper formula)))

;; Given a term containing symbolic constants, and a substitution mapping between
;; symbolic constants and terms, apply the substitution to the term
(define (apply-substs term substitution-mapping)
  (define (helper t)
    (match t
      [(constant ident typ)
       (get-mapping-symbolic t substitution-mapping t)]
      [(expression op args ...)
       (apply op (map helper args))]
      [_ t]))

  (helper term))

;; Given a list of formulae 'clauses' and a list of symbolic constants
;; (variables) 'syms', return the minimal set of formulae such that the elements
;; of the returned set represent a partition of 'syms' that occur in 'clauses'.
;; Each element of the returned set is either a formula from, or a conjunction
;; of formulae from 'clauses'
(define (symbolic-partition clauses syms)
  ;; process clauses, yielding a pair (i, n) where i is a conjunction of clauses
  ;; that have intersection symbolic constants under syms and n is a list of
  ;; clauses with an empty intersction of symbolic constants under syms with i
  (define (helper working nonintersecting unprocessed)
    (if (null? unprocessed)
        (cons working
              nonintersecting)
        (let* ([working-syms (intersection (symbolics working) syms)]
               [next (car unprocessed)]
               [next-syms (symbolics next)])
          (if (null? (intersection working-syms next-syms))
              (helper working
                      (cons next nonintersecting)
                      (cdr unprocessed))
              (helper (&& next working)
                      nonintersecting
                      (cdr unprocessed))))))

  (if (null? clauses)
      '()
      (let* ([processed-nonintersecting (helper (car clauses) '() (cdr clauses))]
             [processed (car processed-nonintersecting)]
             [nonintersecting (cdr processed-nonintersecting)])
        (cons processed
              (symbolic-partition nonintersecting syms)))))

(define (new-symbolic-constant typ)
  (begin
    (define-symbolic* x typ)
    x))

(define (unique-symbolic-constant ident typ)
  (constant (gensym ident) typ))

(provide debug-print
         concrete-eq?
         concrete-equal?
         equal-length?
         in-list?
         in-symbolic-list?
         intersection
         zip
         mapping?
         add-mapping
         get-mapping
         get-mapping-symbolic
         subset-mapping
         inverse-subset-mapping
         keys
         vals
         map-eq-modulo-keys?
         type-in-context
         pretty-indent
         prefixes
         vc-wrapper
         union-pick-head
         trim-trace
         formula->conjuncts
         apply-substs
         symbolic-partition
         new-symbolic-constant
         unique-symbolic-constant)
