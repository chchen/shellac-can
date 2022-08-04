#lang rosette/safe

(require "../bool-bitvec/types.rkt"
         "../config.rkt"
         "../semantics.rkt"
         "../util.rkt"
         "syntax.rkt"
         rosette/lib/match
         ;; unsafe! be sure you know what you're doing
         ;; when you use the following
         (only-in racket/base
                  random
                  symbol?
                  build-list))

;; ;; Encode the signature of an expression
;; (struct ->
;;   (form
;;    domain
;;    codomain)
;;   #:transparent)

(struct guard-trace
  (guard
   trace)
  #:transparent)

;; TODO: Clean up with match and (== struct_name); == from racket/match
;; Type signatures
(define (signature val)
  (cond
    ;; -> Buffer
    [(equal? val empty-recv-buf*) (-> val '() recv-buffer*?)]
    [(equal? val empty-send-buf*) (-> val '() send-buffer*?)]
    ;; Boolean -> Boolean
    [(equal? val not*) (-> val (list boolean?) boolean?)]
    ;; Channel -> Boolean
    [(equal? val empty?*) (-> val (list (out* channel*?)) boolean?)]
    [(equal? val full?*) (-> val (list (in* channel*?)) boolean?)]
    [(equal? val read*) (-> val (list (in* channel*?)) boolean?)]
    ;; Buffer -> Boolean
    [(equal? val recv-buf-full?*) (-> val (list recv-buffer*?) boolean?)]
    [(equal? val send-buf-empty?*) (-> val (list send-buffer*?) boolean?)]
    ;; Channel -> Channel
    [(equal? val drain*) (-> val (list (in* channel*?)) (in* channel*?))]
    ;; Buffer -> Natural
    [(equal? val recv-buf->nat*) (-> val (list recv-buffer*?) integer?)]
    ;; Buffer -> Boolean
    [(equal? val send-buf-get*) (-> val (list send-buffer*?) boolean?)]
    ;; Buffer -> Buffer
    [(equal? val send-buf-next*) (-> val (list send-buffer*?) send-buffer*?)]
    ;; Natural -> Buffer
    [(equal? val nat->send-buf*) (-> val (list integer?) send-buffer*?)]
    ;; Boolean x Boolean -> Boolean
    [(equal? val and*) (-> val (list boolean? boolean?) boolean?)]
    [(equal? val or*) (-> val (list boolean? boolean?) boolean?)]
    [(equal? val <=>*) (-> val (list boolean? boolean?) boolean?)]
    ;; Nat x Nat -> Nat Expressions
    [(equal? val +*) (-> val (list integer? integer?) integer?)]
    ;; Nat x Nat -> Boolean Expressions
    [(equal? val <?*) (-> val (list integer? integer?) boolean?)]
    [(equal? val =?*) (-> val (list integer? integer?) boolean?)]
    ;; Channel x Boolean -> Channel
    [(equal? val fill*) (-> val (list (out* channel*?) boolean?) (out* channel*?))]
    ;; Buffer x Boolean -> Buffer
    [(equal? val recv-buf-put*) (-> val (list recv-buffer*? boolean?) recv-buffer*?)]
    ;; Literal value: constant function
    [else (let ([typ (cond
                       [(boolean? val) boolean?]
                       [(integer? val) integer?])])
            (-> (lambda () val) '() typ))]))

;; Semantics
;; Constant functions

;; empty-recv-buf: natural -> recv-buffer
(define (eval-empty-recv-buf)
  (recv-buffer* 0
                (build-list vect-len (lambda (_) #f))))

;; empty-send-buf: natural -> send-buffer
(define (eval-empty-send-buf)
  (send-buffer* 0
                (build-list vect-len (lambda (_) #f))))

;; Unary functions

;; not: boolean -> boolean
(define (eval-not expr)
  (! expr))

;; empty?: channel -> boolean
(define (eval-empty? chan)
  (! (channel*-valid chan)))

;; full?: channel -> boolean
(define (eval-full? chan)
  (channel*-valid chan))

;; read: channel -> boolean
(define (eval-read chan)
  (channel*-value chan))

;; recv-buf-full?: recv-buffer -> boolean
(define (eval-recv-buf-full? buf)
  (>= (buffer*-cursor buf) vect-len))

;; send-buf-empty?: send-buffer -> boolean
(define (eval-send-buf-empty? buf)
  (>= (buffer*-cursor buf) vect-len))

;; channel-drain: channel -> channel
(define (eval-drain chan)
  (channel* #f '()))

;; recv-buf->nat: recv-buffer -> natural
(define (eval-recv-buf->nat buf)
  (define (helper vals modulus)
    (if (null? vals)
        0
        (+ (if (car vals)
               modulus
               0)
           (helper (cdr vals)
                   (* 2 modulus)))))

  (helper (buffer*-vals buf) 1))

;; send-buf-get: send-buffer -> boolean
(define (eval-send-buf-get buf)
  (list-ref (buffer*-vals buf)
            (buffer*-cursor buf)))

;; send-buf-next: send-buffer -> send-buffer
(define (eval-send-buf-next buf)
  (send-buffer* (add1 (buffer*-cursor buf))
                (buffer*-vals buf)))

;; Binary functions

;; and/or/iff: boolean -> boolean -> boolean
(define (eval-and left right)
  (&& left right))

(define (eval-or left right)
  (|| left right))

(define (eval-<=> left right)
  (<=> left right))

;; +: natural -> natural -> natural
(define (eval-+ left right)
  (+ left right))

;; </=: natural -> natural -> boolean
(define (eval-< left right)
  (< left right))

(define (eval-= left right)
  (= left right))

;; channel-fill: channel -> boolean -> channel
(define (eval-fill chan val)
  (channel* #t val))

;; recv-buf-put: recv-buffer -> boolean -> recv-buffer
(define (eval-recv-buf-put buf val)
  (let ([cursor (buffer*-cursor buf)]
        [vals (buffer*-vals buf)])
    (recv-buffer* (add1 cursor) (list-set vals cursor val))))

;; nat->send-buf: natural -> natural -> send-buffer
(define (eval-nat->send-buf val)
  (let ([bools (map bitvector->bool
                    (bitvector->bits
                     (integer->bitvector val
                                         vect?)))])
    (send-buffer* 0 bools)))

;; Evaluate an expression. Takes an expression, context, and a state
(define (evaluate-expr expression
                       [context '()]
                       [state '()])
  (define (unary next-func expr)
    (next-func (eval-helper expr)))

  (define (binary next-func expr-l expr-r)
    (next-func (eval-helper expr-l)
               (eval-helper expr-r)))

  (define (eval-helper expr)
    (match expr
      ;; -> Buffer
      [(empty-recv-buf*) (eval-empty-recv-buf)]
      [(empty-send-buf*) (eval-empty-send-buf)]
      ;; Boolean -> Boolean
      [(not* e) (unary eval-not e)]
      ;; Channel -> Boolean
      [(empty?* e) (unary eval-empty? e)]
      [(full?* e) (unary eval-full? e)]
      [(read* e) (unary eval-read e)]
      ;; Buffer -> Boolean
      [(recv-buf-full?* e) (unary eval-recv-buf-full? e)]
      [(send-buf-empty?* e) (unary eval-send-buf-empty? e)]
      ;; Channel -> Channel
      [(drain* e) (unary eval-drain e)]
      ;; Buffer -> Natural
      [(recv-buf->nat* e) (unary eval-recv-buf->nat e)]
      ;; Buffer -> Boolean
      [(send-buf-get* e) (unary eval-send-buf-get e)]
      ;; Buffer -> Buffer
      [(send-buf-next* e) (unary eval-send-buf-next e)]
      ;; Natural -> Buffer
      [(nat->send-buf* e) (unary eval-nat->send-buf e)]
      ;; Boolean x Boolean -> Boolean
      [(and* l r) (binary eval-and l r)]
      [(or* l r) (binary eval-or l r)]
      [(<=>* l r) (binary eval-<=> l r)]
      ;; Nat x Nat -> Nat Expressions
      [(+* l r) (binary eval-+ l r)]
      ;; Nat x Nat -> Boolean Expressions
      [(<?* l r) (binary eval-< l r)]
      [(=?* l r) (binary eval-= l r)]
      ;; Channel x Boolean -> Channel
      [(fill* l r) (binary eval-fill l r)]
      ;; Buffer x Boolean -> Buffer
      [(recv-buf-put* l r) (binary eval-recv-buf-put l r)]
      ;; Terminals
      [term
       (cond
         [(symbol? expr) (get-mapping expr state)]
         [else term])]))

  (eval-helper expression))


(provide signature
         evaluate-expr)
