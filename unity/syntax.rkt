#lang rosette/safe

(require "../config.rkt"
         "../util.rkt"
         rosette/lib/match)

;; Types
;; Boolean (true/false)
;; Channel (full/empty containers for booleans)
;; Nat (0, 1, 2...)
;; Send-buf (N-ary lists of booleans in "big-endian")
;; Recv-buf (N-ary lists of booleans in "little-endian")
;;
;; Terminals: #t, #f, 'empty, natural numbers

;; Type modifiers
(struct in*
  (typ)
  #:transparent)

(struct out*
  (typ)
  #:transparent)

;; Channel types. Two fields:
;; 1) validity
;; 2) value
(struct channel*
  (valid
   value)
  #:transparent)

(struct recv-channel*
  channel* ()
  #:transparent)

(struct send-channel*
  channel* ()
  #:transparent)

;; Boolean buffer with a cursor
(struct buffer*
  (cursor
   vals)
  #:transparent)

(struct recv-buffer*
  buffer* ()
  #:transparent)

(struct send-buffer*
  buffer* ()
  #:transparent)

;; Top-level syntax.
;; A UNITY program consists of a triple:
;; 1) a declaration of variable types (map from identifers to types)
;; 2) an initial (multi-)assignment
;; 3) a nondeterministic choice over (multi-)assignments
(struct unity*
  (declare
   initially
   assign)
  #:transparent)

;; Assignment
(struct :=*
  (lhs
   rhs)
  #:transparent)

;; Guarded expressions
(struct guard-exprs*
  (guard
   exprs)
  #:transparent)

;; A tuple of variables
(define-syntax vars*
  (syntax-rules ()
    [(vars* v) (list v)]
    [(vars* v vs ...) (cons v (vars* vs ...))]))

(define-syntax i-exprs
  (syntax-rules ()
    [(i-exprs e) (list e)]
    [(i-exprs e es ...) (cons e (i-exprs es ...))]))

;; Unconditional guarded expressions
(define-syntax exprs*
  (syntax-rules ()
    [(exprs* e ...) (list (guard-exprs* #t (i-exprs e ...)))]))

;; Conditional guarded expressions
(define-syntax case-exprs*
  (syntax-rules ()
    [(case-exprs* g e ...) (guard-exprs* g (i-exprs e ...))]))

;; Nondeterministic choice over parallel assignments
(define-syntax choice*
  (syntax-rules ()
    [(choice* e) (list e)]
    [(choice* e es ...) (cons e (choice* es ...))]))

(define natural? integer?)

;; Expressions
;; Negation (Bool -> Bool)
(struct not*
  (expr)
  #:transparent)

;; Logical AND (Bool x Bool -> Bool)
(struct and*
  (left
   right)
  #:transparent)

;; Logical OR (Bool x Bool -> Bool)
(struct or*
  (left
   right)
  #:transparent)

;; Iff (Bool x Bool -> Bool)
(struct <=>*
  (left
   right)
  #:transparent)

;; Nat Addition
(struct +*
  (left
   right)
  #:transparent)

;; Nat Less-Than
(struct <?*
  (left
   right))

;; Nat Equality
(struct =?*
  (left
   right))

;; Channel not valid (Channel -> Bool)
(struct empty?*
  (chan)
  #:transparent)

;; Channel valid (Channel -> Bool)
(struct full?*
  (chan)
  #:transparent)

;; Send-buf* exhausted
(struct send-buf-empty?*
  (buf)
  #:transparent)

;; Recv-buf* ready
(struct recv-buf-full?*
  (buf)
  #:transparent)

;; Read a value from a channel Channel -> Bool
;; Partial function! Only defined for full channels
(struct read*
  (channel)
  #:transparent)

;; New channel modification:
;; Empty a channel Channel -> Channel
;; Partial function! Only defined for full channels
(struct drain*
  (chan)
  #:transparent)

;; Fill a channel Channel -> Bool -> Channel
;; Partial function! Only defined for empty channels
(struct fill*
  (chan value)
  #:transparent)

;; '() -> empty Recv-buf*
(struct empty-recv-buf*
  ()
  #:transparent)

;; '() -> empty Send-buf*
(struct empty-send-buf*
  ()
  #:transparent)

;; New recv-buf* with an item added to an existing recv-buf*
;; Partial function! Only defined for non-full recv-buf*
;; Recv-buf* -> Bool -> Recv-buf*
(struct recv-buf-put*
  (buf
   item)
  #:transparent)

;; The natural number equivalent of a recv-buf*
;; Partial function! Only defined for full recv-buf*
;; Recv-buf* -> Nat
(struct recv-buf->nat*
  (buf)
  #:transparent)

(struct nat->send-buf*
  (value)
  #:transparent)

;; The next item in the send-buf*
;; Partial function! Only defined for non-empty send-buf*
;; Send-buf* -> Bool
(struct send-buf-get*
  (buf)
  #:transparent)

;; New send-buf* where send-buf-get* yields the next item
;; Partial function! Only defined for non-empty send-buf*
;; Send-buf* -> Send-buf*
(struct send-buf-next*
  (buf)
  #:transparent)

;; Export the following from the module:
(provide in* ;; type modifiers
         in*?
         out*
         out*?
         channel* ;; types
         channel*?
         recv-channel*
         recv-channel*?
         send-channel*
         send-channel*?
         channel*-valid
         channel*-value
         buffer*
         buffer*?
         buffer*-cursor
         buffer*-vals
         recv-buffer*
         recv-buffer*?
         send-buffer*
         send-buffer*?
         natural?
         unity* ;; syntax
         unity*-declare
         unity*-initially
         unity*-assign
         :=*
         guard-exprs*
         guard-exprs*-guard
         guard-exprs*-exprs
         vars*
         exprs*
         case-exprs*
         choice*
         not* ;; exprs
         and*
         or*
         +*
         <?*
         =?*
         <=>*
         empty?*
         full?*
         recv-buf-full?*
         send-buf-empty?*
         read*
         drain*
         fill*
         empty-recv-buf*
         empty-send-buf*
         recv-buf-put*
         recv-buf->nat*
         nat->send-buf*
         send-buf-get*
         send-buf-next*
         unity-example
         )

;; Example syntax

(define unity-example
  (unity*
   (list (cons 'in-read boolean?)
         (cons 'in (in* channel*?))
         (cons 'out (out* channel*?)))
   (:=* (vars* 'in-read)
        (exprs* #f))
   (choice*
    (:=* (vars* 'in-read
                'out)
         (list (case-exprs* (and* (not* 'in-read)
                                  (and* (empty?* 'out)
                                        (full?* 'in)))
                            #t
                            (fill* 'out (read* 'in)))))
    (:=* (vars* 'in-read
                'in)
         (list (case-exprs* (and* 'in-read
                                  (full?* 'in))
                            #f
                            (drain* 'in)))))))

(assert
 (unity*? unity-example))
