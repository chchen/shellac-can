#lang rosette/safe

(require "../bool-bitvec/types.rkt"
         "../util.rkt"
         "syntax.rkt"
         (only-in racket/base symbol->string substring)
         (only-in racket/string string-join)
         rosette/lib/match)

(define (print-arduino-program program)
  (display
   (string-join
    (flatten
     (pretty-indent (emit-arduino-program program) ""))
    "\n"
    #:after-last "\n")))

(define (emit-arduino-program program)
  (match program
    [(arduino* setup loop)
     (let* ([setup-stmts (setup*-statements setup)]
            [loop-stmts (loop*-statements loop)]
            [var-decls (setup->var-decls setup-stmts)]
            [pins-inits (setup->pins-inits setup-stmts)])
       (append (emit-stmts var-decls)
               (list
                "void setup()"
                (emit-block pins-inits)
                "void loop()"
                (emit-block loop-stmts))))]))

(define (setup->var-decls setup)
  (define (var-stmt? stmt)
    (match stmt
      [(bool* _) #t]
      [(byte* _) #t]
      [(const-int* _ _) #t]
      [(unsigned-int* _) #t]
      [_ #f]))

  (filter var-stmt? setup))

(define (setup->pins-inits setup)
  (define (pins-inits-stmt? stmt)
    (match stmt
      [(pin-mode* _ _) #t]
      [_ #f]))

  (filter pins-inits-stmt? setup))

(define (emit-block stmts)
  (if (null? stmts)
      (list "{"
            "// noop"
            "}")
      (list "{"
            (emit-stmts stmts)
            "}")))

(define (emit-stmts stmts)
  (define (emit-stmt stmt)
    (match stmt
      [(bool* ident) (format "bool ~a;" ident)]
      [(byte* ident) (format "byte ~a;" ident)]
      [(unsigned-int* ident) (format "unsigned int ~a;" ident)]
      [(const-int* ident val) (format "const int ~a = ~a;" ident val)]
      [(pin-mode* pin mode) (format "pinMode(~a, ~a);" pin mode)]
      [(write* pin expr) (format "digitalWrite(~a, ~a);"
                                 pin
                                 (emit-expr expr))]
      [(:=* var expr) (format "~a = ~a;"
                              var
                              (emit-expr expr))]
      [(if* test left right) (if (null? right)
                                 (list
                                  (format "if (~a)" (emit-expr test))
                                  (emit-block left))
                                 (list
                                  (format "if (~a)" (emit-expr test))
                                  (emit-block left)
                                  (cons "else"
                                        (emit-block right))))]))

  (map emit-stmt stmts))

(define (emit-expr expr)
  (match expr
    [(not* e) (format "!~a" (emit-expr e))]
    [(and* l r) (format "(~a && ~a)" (emit-expr l) (emit-expr r))]
    [(or* l r) (format "(~a || ~a)" (emit-expr l) (emit-expr r))]
    [(lt* l r) (format "(~a < ~a)" (emit-expr l) (emit-expr r))]
    [(bool-eq* l r) (format "(~a == ~a)" (emit-expr l) (emit-expr r))]
    [(int-eq* l r) (format "(~a == ~a)" (emit-expr l) (emit-expr r))]
    [(bwnot* e) (format "~~~a" (emit-expr e))]
    [(add* l r) (format "(~a + ~a)" (emit-expr l) (emit-expr r))]
    [(bwand* l r) (format "(~a & ~a)" (emit-expr l) (emit-expr r))]
    [(bwor* l r) (format "(~a | ~a)" (emit-expr l) (emit-expr r))]
    [(bwxor* l r) (format "(~a ^ ~a)" (emit-expr l) (emit-expr r))]
    [(shl* l r) (format "(~a << ~a)" (emit-expr l) (emit-expr r))]
    [(shr* l r) (format "(~a >> ~a)" (emit-expr l) (emit-expr r))]
    [(read* e) (format "digitalRead(~a)" e)]
    ;; Symbolic constant; treat like identifier
    [(constant id _) (format "~a" id)]
    ;; Literals
    [t (format "~a"
               (cond
                 [(vect? t) (bitvector->natural t)]
                 [else t]))]))

(provide print-arduino-program)

;; (print-arduino-program
;;  (arduino* (setup*
;;             (list (byte* 'x)
;;                   (pin-mode* 'd0 'input)
;;                   (pin-mode* 'd1 'output)
;;                   (:=* 'x 'true)
;;                   (write* 'd1 'LOW)))
;;            (loop*
;;             (list (if* (and* (bool-eq* (read* 'd0) 'HIGH)
;;                              'x)
;;                        (list (:=* 'x 'false)
;;                              (write* 'd1 'HIGH))
;;                        (list (:=* 'x 'false)
;;                              (write* 'd1 'HIGH))
;;                        )))))
