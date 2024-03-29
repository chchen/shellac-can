#lang rosette/safe

(require "../util.rkt"
         "types.rkt"
         rosette/lib/angelic
         rosette/lib/match
         rosette/lib/synthax)

(define (vals->bool-terminals vals)
  (append (list #t #f)
          (filter boolean? vals)))

(define (vals->vect-terminals vals)
  (append (list (?? vect?))
          (filter vect? vals)))

(define (exp?? depth vals typ)
  (let* ([bool-terminals (vals->bool-terminals vals)]
         [vect-terminals (vals->vect-terminals vals)])

    (define (boolexp?? depth)
      (let ([terminal-choice (apply choose* bool-terminals)])
        (if (zero? depth)
            terminal-choice
            (let* ([bool-l (boolexp?? (sub1 depth))]
                   [bool-r (boolexp?? (sub1 depth))]
                   [vect-l (vectexp?? (sub1 depth))]
                   [vect-r (vectexp?? (sub1 depth))]
                   [b->b ((apply choose* fn:bool->bool) bool-l)]
                   [b->b->b ((apply choose* fn:bool->bool->bool) bool-l bool-r)]
                   [v->v->b ((apply choose* fn:vect->vect->bool) vect-l vect-r)])
              (choose* terminal-choice
                       b->b
                       b->b->b
                       v->v->b)))))

    (define (vectexp?? depth)
      (let* ([bool-choice (bool->vect (apply choose* bool-terminals))]
             [vect-choice (apply choose* vect-terminals)]
             [terminal-choice (choose* bool-choice
                                       vect-choice)])
        (if (zero? depth)
            terminal-choice
            (let* ([vect-l (vectexp?? (sub1 depth))]
                   [vect-r (vectexp?? (sub1 depth))]
                   [v->v ((apply choose* fn:vect->vect) vect-l)]
                   [v->v->v ((apply choose* fn:vect->vect->vect) vect-l vect-r)])
              (choose* terminal-choice
                       v->v
                       v->v->v)))))

    (cond
      [(eq? typ boolean?) (boolexp?? depth)]
      [(eq? typ vect?) (vectexp?? depth)])))

(define (trace?? target-ids target-vals depth target-st)
  (define (target-id->exp?? id)
    (let* ([original-val (get-mapping id target-st)]
           [exp-typ (cond
                      [(boolean? original-val) boolean?]
                      [(vect? original-val) vect?])])
      (cons id (exp?? depth target-vals exp-typ))))

  (append (map target-id->exp?? target-ids)
          target-st))

(define (trace-permutation?? trace)
  (define (helper len)
    (if (zero? len)
        '()
        (cons (apply choose* trace)
              (helper (sub1 len)))))

  (helper (length trace)))

(provide exp??
         trace??
         trace-permutation??)
