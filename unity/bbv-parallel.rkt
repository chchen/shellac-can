#lang rosette/safe

(require "../bool-bitvec/inversion.rkt"
         "../bool-bitvec/types.rkt"
         "../config.rkt"
         "../semantics.rkt"
         "../util.rkt"
         "bbv-refinement.rkt"
         "semantics.rkt"
         "syntax.rkt"
         rosette/lib/angelic
         rosette/lib/match
         rosette/solver/solver
         ;; unsafe! be sure you know what you're doing
         ;; when you use the following
         (only-in racket/base
                  make-immutable-hash
                  symbol?
                  symbol->string
                  with-handlers
                  exn:fail?)
         (only-in racket/list
                  index-of))

;; How to synthesize a compilation rule

;; 1. From each element of the unity domain:
;; a. Find refinement domain
;; b. Generate a tuple of symbolic values of the refinement domain, lining up
;; with a multi-value from the subexpressions translated recursively
;; c. Apply refinement mapping to the tuple of symbolic values, yielding a
;; symbolic value in the unity domain

;; 2. Applying the semantic function to the symbolic values in the unity domain
;; yields a symbolic value in the unity codomain

;; 3. From the unity codomain:
;; a. Find refinement domain
;; c. Apply refinement mapping to the type of holes, yielding a symbolic value
;; in the unity codomain

;; The verification condition the solver now has to evaluate comes from
;; asserting equality between the symbolic value in the unity domain from (2)
;; and the symbolic value in the unity domain from (3c), *assuming* the precondition

;; But we can do better. We can decompose the verification condition into
;; solving for each subformula in a conjunction, which in the best case means we
;; solve for each hole independently

;; Now that we have a tuple of synthesized expressions, we emit a function that
;; takes subexpressions (see 1b) and substitutes them into the synthesized
;; expressions, yielding the translation.

;; Recall that the compilation rule only works if the precondition holds, but we
;; can check that straight away against the unity guard

;; High level transformer

;; Translation rule
;; pair: ( precondition . rule )
;; rule: ( bbv-domain . bbv-codomain )
;; bbv-domain: list of lists
;; s.t. for each list x, there exists a member of the unity domain y
;; refinement-mapping: x -> unity-domain
;; bbv-domain: list of symbolic values s.t. refinement-mapping: bbv-domain -> unity-domain
;; bbv-codomain:
;; precondition: unity-domain -> boolean

;; rule generator
;; syntactic struct: struct?
;; precondition?: domain -> boolean
;; domain: (list type?)
;; codomain: type?
;; semantics: domain -> codomain

;; bbv-parallel program structure
(struct bbv-parallel*
  (declare
   initially
   assign)
  #:transparent)

(struct bbv-guard-exprs*
  (guard
   bvv-exprs)
  #:transparent)

(struct bbv-exprs*
  (exprs
   constraints)
  #:transparent)

(struct bbv-context
  (bbv-val+>bbv-typ
   unity-ident->bbv-vals)
  #:transparent)

;; Synthesized rules
(struct translation-rule
  (precondition
   domain
   codomain
   ordering-constraints)
  #:transparent)

(struct translation-result
  (precondition
   expressions
   ordering-constraints)
  #:transparent)

;; Precondition

(define (valid-buffer? buf)
  (>= (buffer*-cursor buf) 0))

;; Refinement mapping functions

;; vect -> natural
(define (vect->natural v)
  (bitvector->natural v))

;; bool -> bool -> bool -> channel
(define (req->ack->val->channel req ack val)
  (if (eq? req ack)
      (channel* #f null)
      (channel* #t val)))

(define (req->ack->val->recv-channel req ack val)
  (if (eq? req ack)
      (recv-channel* #f null)
      (recv-channel* #t val)))

(define (req->ack->val->send-channel req ack val)
  (if (eq? req ack)
      (send-channel* #f null)
      (send-channel* #t val)))

;; vect -> vect -> buffer
(define (cursor->val->recv-buffer cursor val)
  (recv-buffer* (bitvector->natural cursor)
           (map bitvector->bool
                (bitvector->bits val))))

(define (cursor->val->send-buffer cursor val)
  (send-buffer* (bitvector->natural cursor)
           (map bitvector->bool
                (bitvector->bits val))))

;; bbv "field" accessors

;; (req ack val) -> req
(define (bbv-chan->req bbv-chan)
  (first bbv-chan))

;; (req ack val) -> ack
(define (bbv-chan->ack bbv-chan)
  (second bbv-chan))

;; (req ack val) -> val
(define (bbv-chan->val bbv-chan)
  (third bbv-chan))

;; (cursor val) -> cursor
(define (bbv-buf->cursor bbv-buf)
  (first bbv-buf))

;; (cursor val) -> val
(define (bbv-buf->val bbv-buf)
  (second bbv-buf))

;; Is the type externally visible?
(define (type-external? typ)
  (cond
    [(in*? typ) #t]
    [(out*? typ) #t]
    [(equal? recv-channel*? typ) #t]
    [(equal? send-channel*? typ) #t]
    [else #f]))

;; A mapping from unity types to their refinement mapping domain
(define (type->refinement-domain typ)
  (define table
    (list
     (cons boolean? (list boolean?))
     (cons (in* channel*?) (list (in* boolean?) (out* boolean?) (in* boolean?)))
     (cons (out* channel*?) (list (out* boolean?) (in* boolean?) (out* boolean?)))
     (cons recv-channel*? (list boolean? boolean? boolean?))
     (cons send-channel*? (list boolean? boolean? boolean?))
     (cons integer? (list vect?))
     (cons recv-buffer*? (list vect? vect?))
     (cons send-buffer*? (list vect? vect?))))

  (get-mapping typ table))

(define (unbox-type typ)
  (match typ
    [(in* t) t]
    [(out* t) t]
    [t t]))

(define (refinement-domain->symbolic-domain dom)
  (map (lambda (typ)
         (new-symbolic-constant (unbox-type typ)))
       dom))

(define (type->mapping-fn typ)
  (define table
    (list
     (cons boolean? identity)
     (cons (in* channel*?) req->ack->val->channel)
     (cons (out* channel*?) req->ack->val->channel)
     (cons recv-channel*? req->ack->val->recv-channel)
     (cons send-channel*? req->ack->val->send-channel)
     (cons integer? vect->natural)
     (cons recv-buffer*? cursor->val->recv-buffer)
     (cons send-buffer*? cursor->val->send-buffer)))

  (get-mapping typ table))



(define (try-synth-bbv-exprs precondition postcondition bbv-domain bbv-codomain)
  (let ([bbv-holes (intersection bbv-codomain (symbolics postcondition))]
        [bbv-vals (flatten bbv-domain)]
        [start-time (current-seconds)])
    (define (try-synth exp-depth)
      (with-terms
        (vc-wrapper
         (let* ([symbolic+>sketch (map (lambda (v)
                                         (cons v (exp?? exp-depth
                                                        bbv-vals
                                                        (type-of v))))
                                       bbv-holes)]
                [subst-model (sat (make-immutable-hash symbolic+>sketch))]
                [expanded-postcondition (evaluate postcondition subst-model)]
                [model (with-handlers
                         ([exn:fail? (lambda (exn) (unknown))])
                         (synthesize
                          #:forall bbv-vals
                          #:guarantee (begin (assume precondition)
                                             (assert expanded-postcondition))))]
                [symbolic+>impl (if (sat? model)
                                    (map (lambda (k-v)
                                           (cons (car k-v)
                                                 (evaluate (cdr k-v) model)))
                                         symbolic+>sketch)
                                    (unsat))])
           (if (sat? model)
               (begin
                 (debug-print (format "~a~n"
                                      (- (current-seconds) start-time)))
                 symbolic+>impl)
               (begin
                 (debug-print (format "~a."
                                      (- (current-seconds) start-time)))
                 (if (>= exp-depth max-expression-depth)
                     symbolic+>impl
                     (try-synth (add1 exp-depth)))))))))

    (try-synth 0)))

(define (synth-rule expr
                    [precondition? (lambda (domain) #t)]
                    [bbv-invariant? (lambda (bbv-domain bbv-codomain) #t)])
  (begin
    (debug-print (format "try-synth ~a~n" expr))
    (time
     (match (signature expr)
       [(-> form domain codomain)
        (vc-wrapper
         (let* ([bbv-domain (map type->refinement-domain domain)]
                [bbv-symbolic-domain (map refinement-domain->symbolic-domain bbv-domain)]
                [bbv-domain->domain (map type->mapping-fn domain)]
                [mapped-symbolic-domain (map (lambda (fn x)
                                               (apply fn x))
                                             bbv-domain->domain
                                             bbv-symbolic-domain)]
                [precondition (precondition? mapped-symbolic-domain)]
                [postcondition (evaluate-expr (apply form mapped-symbolic-domain))]
                [bbv-codomain (type->refinement-domain codomain)]
                [bbv-symbolic-codomain (refinement-domain->symbolic-domain bbv-codomain)]
                [bbv-codomain->codomain (type->mapping-fn codomain)]
                [mapped-codomain (apply bbv-codomain->codomain bbv-symbolic-codomain)]
                [synthesis-condition (and (bbv-invariant? bbv-symbolic-domain
                                                          bbv-symbolic-codomain)
                                          (equal? postcondition
                                                  mapped-codomain))]
                [partitioned-synthesis-conditions (symbolic-partition (formula->conjuncts synthesis-condition)
                                                                      bbv-symbolic-codomain)])
           (define (try-synth formula)
             (try-synth-bbv-exprs precondition
                                  formula
                                  bbv-symbolic-domain
                                  bbv-symbolic-codomain))

           (let* ([holevar+>impl (apply append (map try-synth partitioned-synthesis-conditions))]
                  [holevar-model (sat (make-immutable-hash holevar+>impl))]
                  [holevar->synthesized-expr (lambda (var) (evaluate var holevar-model))]
                  [synthesized-expr-tuple (map holevar->synthesized-expr bbv-symbolic-codomain)]
                  [ordering-constraints (if (type-external? codomain)
                                            (let* ([domain-idx (index-of domain codomain)]
                                                   [pretuple (list-ref bbv-symbolic-domain domain-idx)]
                                                   [mapping (list-ref bbv-domain->domain domain-idx)])
                                              (refinement-ordering precondition
                                                                   pretuple
                                                                   synthesized-expr-tuple
                                                                   mapping))
                                            '())])
             (translation-rule precondition
                               bbv-symbolic-domain
                               synthesized-expr-tuple
                               ordering-constraints))))]))))

;; Translation rules

(define translate-drain
  (let (;; Only defined for full channels
        [precondition
         (lambda (domain)
           (match domain
             [(list channel) (evaluate-expr (full?* channel))]))]
        ;; Request and value invariant
        [bbv-invariant
         (lambda (bbv-domain bbv-codomain-channel)
           (match bbv-domain
             [(list bbv-domain-channel)
              (&& (<=> (bbv-chan->req bbv-domain-channel) (bbv-chan->req bbv-codomain-channel ))
                  (<=> (bbv-chan->val bbv-domain-channel) (bbv-chan->val bbv-codomain-channel)))]))])
  (synth-rule drain* precondition bbv-invariant)))

;; TODO: Why does the refinement ordering find an ordering constraint for the setting
;; ack, even though ack is fixed by bbv-invariant?

;; TODO: looks like the precondition + bbv-inviarant allows the solver to find
;; an assignment that flips the req value. scary!

;; TODO: The precondition is that req and ack in the domain are equal (because
;; the channel is empty), so of course the synthesizer can satisfy the
;; bbv-invariant with the other value
(define translate-fill
  (let (;; Only defined for empty channels
        [precondition
         (lambda (domain)
           (match domain
             [(list channel _) (evaluate-expr (empty?* channel))]))]
        ;; Ack invariant
        [bbv-invariant
         (lambda (bbv-domain bbv-codomain-channel)
           (match bbv-domain
             [(list bbv-domain-channel _)
              (<=> (bbv-chan->ack bbv-domain-channel) (bbv-chan->ack bbv-codomain-channel))]))])
    (synth-rule fill* precondition bbv-invariant)))

(define translate-read
  (let (;; Only defined for full channels
        [precondition
         (lambda (domain)
           (match domain
             [(list channel) (evaluate-expr (full?* channel))]))])
    (synth-rule read* precondition)))

(define translate-recv-buf-put
  (let ([precondition
         (lambda (domain)
           (match domain
             [(list recv-buffer _)
              (&& (valid-buffer? recv-buffer)
                  (! (evaluate-expr (recv-buf-full?* recv-buffer))))]))])
    (synth-rule recv-buf-put* precondition)))

(define translate-recv-buf->nat
  (let* ([precondition (lambda (dom)
                         (let ([domain-buffer (car dom)])
                           (&& (valid-buffer? domain-buffer)
                               (evaluate-expr (recv-buf-full?* domain-buffer)))))])
    (synth-rule recv-buf->nat* precondition)))

(define translate-send-buf-get
  (let* ([precondition
          (lambda (domain)
            (match domain
              [(list send-buffer)
               (&& (valid-buffer? send-buffer)
                   (! (evaluate-expr (send-buf-empty?* send-buffer))))]))])
    (synth-rule send-buf-get* precondition)))

(define translate-send-buf-next
  (let* ([precondition
          (lambda (domain)
            (match domain
              [(list send-buffer)
               (&& (valid-buffer? send-buffer)
                   (! (evaluate-expr (send-buf-empty?* send-buffer))))]))])
    (synth-rule send-buf-next* precondition)))

(define translate-+
  (let* ([precondition (lambda (dom)
                         (let ([left (car dom)]
                               [right (cadr dom)])
                           (< (+ left right) (expt 2 vect-len))))])
    (synth-rule +* precondition)))

(define translate-empty-recv-buf* (synth-rule empty-recv-buf*))
(define translate-empty-send-buf* (synth-rule empty-send-buf*))
(define translate-not* (synth-rule not*))
(define translate-empty?* (synth-rule empty?*))
(define translate-full?* (synth-rule full?*))
(define translate-recv-buf-full?* (synth-rule recv-buf-full?*))
(define translate-send-buf-empty?* (synth-rule send-buf-empty?*))
(define translate-nat->send-buf* (synth-rule nat->send-buf*))
(define translate-and* (synth-rule and*))
(define translate-or* (synth-rule or*))
(define translate-<=>* (synth-rule <=>*))
(define translate-=?* (synth-rule =?*))
(define translate-<?* (synth-rule <?*))

;; unity-program->bbv-parallel-context
(define (unity-declare->bbv-context unity-declare)
  (define (helper decls
                  bbv-val+>bbv-typ
                  unity-ident+>bbv-vals)
    (if (null? decls)
        (bbv-context bbv-val+>bbv-typ
                     (lambda (i)
                       (get-mapping i unity-ident+>bbv-vals)))
        (match decls
          [(cons (cons ident typ) tail)
           (let* ([bbv-typs (type->refinement-domain typ)]
                  [bbv-vals (map (lambda (t) (unique-symbolic-constant ident
                                                                       (unbox-type t)))
                                 bbv-typs)])
             (helper tail
                     (append (zip bbv-vals bbv-typs)
                             bbv-val+>bbv-typ)
                     (cons (cons ident bbv-vals)
                           unity-ident+>bbv-vals)))])))

  (helper unity-declare '() '()))

;; Given a translation rule and a compatible implementation of its domain,
;; apply a substitution yielding a codomain in terms of the implementation.
;; translation-rule -> list[translation-result] -> translation-result
(define (apply-translation tr-rule tr-results)
  (let* ([precond-l (map translation-result-precondition tr-results)]
         [impl-l (map translation-result-expressions tr-results)])
    (begin
      ;; we don't (yet) support nested expressions that imply refinement orderings
      (assert (andmap null?
                      (map translation-result-ordering-constraints
                           tr-results)))
      (match tr-rule
        [(translation-rule precond domain-l codomain ordering-constraints)
         (let* ([symbolic+>impl (apply append (map zip domain-l impl-l))]
                [impl-model (sat (make-immutable-hash symbolic+>impl))])
           (translation-result
            (foldl &&
                   (evaluate precond impl-model)
                   precond-l)
            (map (lambda (expr)
                   (evaluate expr impl-model))
                 codomain)
            ordering-constraints))]))))

;; unity-expr -> unity-ident->bbv-vals -> translation-result
(define (expr->bbv-expr expr unity-ident->bbv-vals)
  ;; expr x expr x translation-rule -> translation-result
  (define (binary-translate left right tr-rule)
    (let* ([tr-results (list (expr->bbv-expr left unity-ident->bbv-vals)
                             (expr->bbv-expr right unity-ident->bbv-vals))])
      (apply-translation tr-rule tr-results)))

  ;; expr x translation-rule -> translation-result
  (define (unary-translate expr tr-rule)
    (let* ([tr-results (list (expr->bbv-expr expr unity-ident->bbv-vals))])
      (apply-translation tr-rule tr-results)))

  ;; translation-rule -> translation-result
  (define (const-translate tr-rule)
    (apply-translation tr-rule '()))

  (match expr
    ;; -> Buffer
    [(empty-recv-buf*) (const-translate translate-empty-recv-buf*)]
    [(empty-send-buf*) (const-translate translate-empty-send-buf*)]
    ;; Bool -> Boolean
    [(not* bool) (unary-translate bool translate-not*)]
    ;; Channel -> Boolean
    [(empty?* chan) (unary-translate chan translate-empty?*)]
    [(full?* chan) (unary-translate chan translate-full?*)]
    [(read* chan) (unary-translate chan translate-read)]
    ;; Buffer -> Boolean
    [(recv-buf-full?* buf) (unary-translate buf translate-recv-buf-full?*)]
    [(send-buf-empty?* buf) (unary-translate buf translate-send-buf-empty?*)]
    ;; Channel -> Channel
    [(drain* chan) (unary-translate chan translate-drain)]
    ;; Buffer -> Natural
    [(recv-buf->nat* buf) (unary-translate buf translate-recv-buf->nat)]
    ;; Buffer -> Boolean
    [(send-buf-get* buf) (unary-translate buf translate-send-buf-get)]
    ;; Buffer -> Buffer
    [(send-buf-next* buf) (unary-translate buf translate-send-buf-next)]
    ;; Natural -> Buffer
    [(nat->send-buf* nat) (unary-translate nat translate-nat->send-buf*)]
    ;; Bool x Bool -> Bool
    [(and* bool-left bool-right) (binary-translate bool-left bool-right translate-and*)]
    [(or* bool-left bool-right) (binary-translate bool-left bool-right translate-or*)]
    [(<=>* bool-left bool-right) (binary-translate bool-left bool-right translate-<=>*)]
    ;; Nat x Nat -> Nat
    [(+* nat-l nat-r) (binary-translate nat-l nat-r translate-+)]
    ;; Nat x Nat -> Bool
    [(=?* nat-l nat-r) (binary-translate nat-l nat-r translate-=?*)]
    [(<?* nat-l nat-r) (binary-translate nat-l nat-r translate-<?*)]
    ;; Channel x Boolean -> Channel
    [(fill* chan bool) (binary-translate chan bool translate-fill)]
    ;; Buffer x Boolean -> Buffer
    [(recv-buf-put* buf bool) (binary-translate buf bool translate-recv-buf-put)]
    ;; Terminal
    [terminal (if (symbol? terminal)
                  (translation-result #t (unity-ident->bbv-vals terminal) '())
                  (const-translate (synth-rule terminal)))]))

(define (guard-exprs->bbv-guard-exprs guard-exprs unity-ident->bbv-values)
  (define (translate expr)
    (expr->bbv-expr expr unity-ident->bbv-values))

  (define (guard=>precond? guard precond)
    (if (concrete-eq? #t precond)
        #t
        (vc-wrapper
         (unsat? (verify (begin
                           (assume guard)
                           (assert precond)))))))

  (define (translation-result->bbv-exprs guard tr)
    (match tr
      [(translation-result precond exprs constraints)
       (begin
         (let ([verified? (guard=>precond? guard precond)])
           (if verified?
               (debug-print (format "sound: ~a~n"
                                    guard-exprs))
               (debug-print (format "unsound! ~a~n~a !=> ~a~n"
                                    guard-exprs guard precond)))
           (assert verified?))
         (bbv-exprs* exprs constraints))]))

  (match guard-exprs
    [(guard-exprs* guard exprs)
     (let* ([guard-tr (translate guard)]
            [exprs-trs (map translate exprs)])
       (match guard-tr
         [(translation-result precond guard-tuple ordering-constraint)
          (begin
            ;; guards must be boolean*, total**, and pure***
            ;; * a singleton tuple
            ;; ** no preconditions
            ;; *** no refinement constraints
            (assert (and (= 1 (length guard-tuple))
                         (concrete-eq? precond #t)
                         (null? ordering-constraint)))
            (bbv-guard-exprs* guard-tuple
                              (map (lambda (tr)
                                     (translation-result->bbv-exprs (car guard-tuple) tr))
                                   exprs-trs)))]))]))

(define (assignment->bbv-assignment assignment unity-ident->bbv-vals)
  (define (translate g-e)
    (guard-exprs->bbv-guard-exprs g-e unity-ident->bbv-vals))

  (match assignment
    [(:=* vars guard-exprs)
     (:=* (map unity-ident->bbv-vals vars)
          (map translate guard-exprs))]))

(define (unity->bbv-parallel program)
  (match program
    [(unity* declarations initially assignments)
     (let* ([context (unity-declare->bbv-context declarations)]
            [bbv-decls (bbv-context-bbv-val+>bbv-typ context)]
            [unity-ident->bbv-vals (bbv-context-unity-ident->bbv-vals context)]
            [bbv-initially (assignment->bbv-assignment initially unity-ident->bbv-vals)]
            [bbv-assignments (map (lambda (a) (assignment->bbv-assignment a unity-ident->bbv-vals))
                                  assignments)])
       (bbv-parallel* bbv-decls bbv-initially bbv-assignments))]))

(define unity-test
  (unity*
   (list (cons 'ballot natural?)
         (cons 'value natural?)
         (cons 'phase natural?)
         (cons 'prom_bal natural?)
         (cons 'prop_mbal natural?)
         (cons 'prop_mval natural?)
         (cons 'out_prop (out* channel*?))
         (cons 'in_prop (in* channel*?))
         (cons 'out_prop_bal send-buffer*?)
         (cons 'out_prop_val send-buffer*?)
         (cons 'in_prop_bal recv-buffer*?)
         (cons 'in_prop_val recv-buffer*?))
   (:=* (vars* 'ballot
               'value
               'phase
               'prom_bal
               'in_prop_bal)
        (exprs* 0
                0
                1
                0
                (empty-recv-buf*)))
   (choice*
    ;; Phase 0: Accepting state
    ;; Phase 255: Failure state
    ;; Phase 1->2: prepare receive buffers
    (:=* (vars* 'in_prop_bal
                'phase)
         (list
          (case-exprs* (=?* 'phase 1)
                       (empty-recv-buf*)
                       2)))
    ;; Phase 2: receive proposal ballot from proposer
    (:=* (vars* 'in_prop
                'in_prop_bal)
         (list
          (case-exprs* (and* (full?* 'in_prop)
                             (and* (not* (recv-buf-full?* 'in_prop_bal))
                                   (=?* 'phase 2)))
                       (drain* 'in_prop)
                       (recv-buf-put* 'in_prop_bal (read* 'in_prop)))))
    ;; Phase 2->3: read proposal
    (:=* (vars* 'prop_mbal
                'phase)
         (list
          (case-exprs* (and* (recv-buf-full?* 'in_prop_bal)
                             (=?* 'phase 2))
                       (recv-buf->nat* 'in_prop_bal)
                       3)))
    ;; Phase 3->4: prepare promise response
    ;; Proposed ballot # > 'ballot and > 'prom_bal
    (:=* (vars* 'out_prop_bal
                'out_prop_val
                'prom_bal
                'phase)

         (list
          (case-exprs* (and* (<?* 'ballot
                                  'prop_mbal)
                             (and* (<?* 'prom_bal
                                        'prop_mbal)
                                   (=?* 'phase 3)))
                       (nat->send-buf* 'ballot)
                       (nat->send-buf* 'value)
                       'prop_mbal
                       4)))
    ;; Phase 4: send promise message
    (:=* (vars* 'out_prop
                'out_prop_bal
                'out_prop_val)
         (list
          (case-exprs* (and* (empty?* 'out_prop)
                             (and* (not* (send-buf-empty?* 'out_prop_bal))
                                   (=?* 'phase 4)))
                       (fill* 'out_prop (send-buf-get* 'out_prop_bal))
                       (send-buf-next* 'out_prop_bal)
                       'out_prop_val)
          (case-exprs* (and* (empty?* 'out_prop)
                             (and* (send-buf-empty?* 'out_prop_bal)
                                   (and* (not* (send-buf-empty?* 'out_prop_val))
                                         (=?* 'phase 4))))
                       (fill* 'out_prop (send-buf-get* 'out_prop_val))
                       'out_prop_bal
                       (send-buf-next* 'out_prop_val)))))))

(provide unity->bbv-parallel
         unity-test
         bbv-parallel*
         bbv-exprs*
         bbv-guard-exprs*)
