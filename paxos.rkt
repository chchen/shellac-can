#lang rosette/safe

(require "unity/syntax.rkt")

;; Model a Paxos proposer
;; A proposer sends phase 1a and 2a and receives phase 1b and 2b messages
;; Internal vars:
;; Ballot
;; Value
;; Phase
;; Sent#
;; Rcvd#
;; Send/Recv Buffers for each Acceptor
;; Send/Recv Channels for each Acceptor

(define proposer
  (unity*
   (list (cons 'ballot natural?)
         (cons 'value natural?)
         (cons 'phase natural?)
         (cons 'a_mbal natural?)
         (cons 'b_mbal natural?)
         (cons 'c_mbal natural?)
         (cons 'a_mval natural?)
         (cons 'b_mval natural?)
         (cons 'c_mval natural?)
         (cons 'out_a (out* channel*?))
         (cons 'out_b (out* channel*?))
         (cons 'out_c (out* channel*?))
         (cons 'in_a (in* channel*?))
         (cons 'in_b (in* channel*?))
         (cons 'in_c (in* channel*?))
         (cons 'out_a_bal send-buffer*?)
         (cons 'out_b_bal send-buffer*?)
         (cons 'out_c_bal send-buffer*?)
         (cons 'out_a_val send-buffer*?)
         (cons 'out_b_val send-buffer*?)
         (cons 'out_c_val send-buffer*?)
         (cons 'in_a_bal recv-buffer*?)
         (cons 'in_b_bal recv-buffer*?)
         (cons 'in_c_bal recv-buffer*?)
         (cons 'in_a_val recv-buffer*?)
         (cons 'in_b_val recv-buffer*?)
         (cons 'in_c_val recv-buffer*?))
   (:=* (vars* 'ballot
               'value
               'phase)
        (exprs* 1
                32
                1))
   (choice*
    ;; Phase 0: Accepting state
    ;; Phase 255: Failure state
    ;; 01 Failure mode: max ballot value
    (:=* (vars* 'phase)
         (list
          (case-exprs* (=?* 'ballot 255)
                       255)))
    ;; 02 Phase 1->2: load buffers for sending prepare messages
    (:=* (vars* 'out_a_bal
                'out_b_bal
                'out_c_bal
                'phase)
         (list
          (case-exprs* (=?* 'phase 1)
                       (nat->send-buf* 'ballot)
                       (nat->send-buf* 'ballot)
                       (nat->send-buf* 'ballot)
                       2)))
    ;; 03 Phase 2: send promise message to acceptors
    (:=* (vars* 'out_a
                'out_a_bal)
         (list
          (case-exprs* (and* (empty?* 'out_a)
                             (and* (not* (send-buf-empty?* 'out_a_bal))
                                   (=?* 'phase 2)))
                       (fill* 'out_a (send-buf-get* 'out_a_bal))
                       (send-buf-next* 'out_a_bal))))
    ;; 04
    (:=* (vars* 'out_b
                'out_b_bal)
         (list
          (case-exprs* (and* (empty?* 'out_b)
                             (and* (not* (send-buf-empty?* 'out_b_bal))
                                   (=?* 'phase 2)))
                       (fill* 'out_b (send-buf-get* 'out_b_bal))
                       (send-buf-next* 'out_b_bal))))
    ;; 05
    (:=* (vars* 'out_c
                'out_c_bal)
         (list
          (case-exprs* (and* (empty?* 'out_c)
                             (and* (not* (send-buf-empty?* 'out_c_bal))
                                   (=?* 'phase 2)))
                       (fill* 'out_c (send-buf-get* 'out_c_bal))
                       (send-buf-next* 'out_c_bal))))
    ;; 06 Phase 2->3: prepare receive buffers
    (:=* (vars* 'in_a_bal
                'in_b_bal
                'in_c_bal
                'in_a_val
                'in_b_val
                'in_c_val
                'phase)
         (list
          (case-exprs* (and* (send-buf-empty?* 'out_a_bal)
                             (and* (send-buf-empty?* 'out_b_bal)
                                   (and* (send-buf-empty?* 'out_c_bal)
                                         (=?* 'phase 2))))
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       3)))
    ;; Phase 3: receive promise replies from acceptors
    (:=* (vars* 'in_a
                'in_a_bal
                'in_a_val)
         (list
          ;; 07
          (case-exprs* (and* (full?* 'in_a)
                             (and* (not* (recv-buf-full?* 'in_a_bal))
                                   (=?* 'phase 3)))
                       (drain* 'in_a)
                       (recv-buf-put* 'in_a_bal (read* 'in_a))
                       'in_a_val)
          ;; 08
          (case-exprs* (and* (full?* 'in_a)
                             (and* (recv-buf-full?* 'in_a_bal)
                                   (and* (not* (recv-buf-full?* 'in_a_val))
                                         (=?* 'phase 3))))
                       (drain* 'in_a)
                       'in_a_bal
                       (recv-buf-put* 'in_a_val (read* 'in_a)))))
    (:=* (vars* 'in_b
                'in_b_bal
                'in_b_val)
         (list
          ;; 09
          (case-exprs* (and* (full?* 'in_b)
                             (and* (not* (recv-buf-full?* 'in_b_bal))
                                   (=?* 'phase 3)))
                       (drain* 'in_b)
                       (recv-buf-put* 'in_b_bal (read* 'in_b))
                       'in_b_val)
          ;; 10
          (case-exprs* (and* (full?* 'in_b)
                             (and* (recv-buf-full?* 'in_b_bal)
                                   (and* (not* (recv-buf-full?* 'in_b_val))
                                         (=?* 'phase 3))))
                       (drain* 'in_b)
                       'in_b_bal
                       (recv-buf-put* 'in_b_val (read* 'in_b)))))
    (:=* (vars* 'in_c
                'in_c_bal
                'in_c_val)
         (list
          ;; 11
          (case-exprs* (and* (full?* 'in_c)
                             (and* (not* (recv-buf-full?* 'in_c_bal))
                                   (=?* 'phase 3)))
                       (drain* 'in_c)
                       (recv-buf-put* 'in_c_bal (read* 'in_c))
                       'in_c_val)
          ;; 12
          (case-exprs* (and* (full?* 'in_c)
                             (and* (recv-buf-full?* 'in_c_bal)
                                   (and* (not* (recv-buf-full?* 'in_c_val))
                                         (=?* 'phase 3))))
                       (drain* 'in_c)
                       'in_c_bal
                       (recv-buf-put* 'in_c_val (read* 'in_c)))))
    ;; 13 Phase 3->4
    (:=* (vars* 'a_mbal
                'b_mbal
                'c_mbal
                'a_mval
                'b_mval
                'c_mval
                'phase)
         (list
          (case-exprs* (and* (recv-buf-full?* 'in_a_bal)
                             (and* (recv-buf-full?* 'in_b_bal)
                                   (and* (recv-buf-full?* 'in_c_bal)
                                         (and* (recv-buf-full?* 'in_a_val)
                                               (and* (recv-buf-full?* 'in_b_val)
                                                     (and* (recv-buf-full?* 'in_c_val)
                                                           (=?* 'phase 3)))))))
                       (recv-buf->nat* 'in_a_bal)
                       (recv-buf->nat* 'in_b_bal)
                       (recv-buf->nat* 'in_c_bal)
                       (recv-buf->nat* 'in_a_val)
                       (recv-buf->nat* 'in_b_val)
                       (recv-buf->nat* 'in_c_val)
                       4)))
    ;; Phase 4->5: select value
    (:=* (vars* 'value
                'phase)
         (list
          ;; 14
          (case-exprs* (and* (=?* 0 'a_mbal)
                             (and* (=?* 0 'b_mbal)
                                   (and* (=?* 0 'c_mbal)
                                         (=?* 'phase 4))))
                       'value
                       5)
          ;; 15
          (case-exprs* (and* (or* (=?* 'b_mbal 'a_mbal)
                                  (<?* 'b_mbal 'a_mbal))
                             (and* (or* (=?* 'c_mbal 'a_mbal)
                                        (<?* 'c_mbal 'a_mbal))
                                   (=?* 'phase 4)))
                       'a_mval
                       5)
          ;; 16
          (case-exprs* (and* (or* (=?* 'a_mbal 'b_mbal)
                                  (<?* 'a_mbal 'b_mbal))
                             (and* (or* (=?* 'c_mbal 'b_mbal)
                                        (<?* 'c_mbal 'b_mbal))
                                   (=?* 'phase 4)))
                       'b_mval
                       5)
          ;; 17
          (case-exprs* (and* (or* (=?* 'a_mbal 'c_mbal)
                                  (<?* 'a_mbal 'c_mbal))
                             (and* (or* (=?* 'b_mbal 'c_mbal)
                                        (<?* 'b_mbal 'c_mbal))
                                   (=?* 'phase 4)))
                       'c_mval
                       5)))
    ;; 18 Phase 5->6: load buffers for sending vote messages
    (:=* (vars* 'out_a_bal
                'out_b_bal
                'out_c_bal
                'out_a_val
                'out_b_val
                'out_c_val
                'phase)
         (list
          (case-exprs* (=?* 'phase 5)
                       (nat->send-buf* 'ballot)
                       (nat->send-buf* 'ballot)
                       (nat->send-buf* 'ballot)
                       (nat->send-buf* 'value)
                       (nat->send-buf* 'value)
                       (nat->send-buf* 'value)
                       6)))
    ;; Phase 6: send vote message to acceptors
    (:=* (vars* 'out_a
                'out_a_bal
                'out_a_val)
         (list
          ;; 19
          (case-exprs* (and* (empty?* 'out_a)
                             (and* (not* (send-buf-empty?* 'out_a_bal))
                                   (=?* 'phase 6)))
                       (fill* 'out_a (send-buf-get* 'out_a_bal))
                       (send-buf-next* 'out_a_bal)
                       'out_a_val)
          ;; 20
          (case-exprs* (and* (empty?* 'out_a)
                             (and* (send-buf-empty?* 'out_a_bal)
                                   (and* (not* (send-buf-empty?* 'out_a_val))
                                         (=?* 'phase 6))))
                       (fill* 'out_a (send-buf-get* 'out_a_val))
                       'out_a_bal
                       (send-buf-next* 'out_a_val))))
    (:=* (vars* 'out_b
                'out_b_bal
                'out_b_val)
         (list
          ;; 21
          (case-exprs* (and* (empty?* 'out_b)
                             (and* (not* (send-buf-empty?* 'out_b_bal))
                                   (=?* 'phase 6)))
                       (fill* 'out_b (send-buf-get* 'out_b_bal))
                       (send-buf-next* 'out_b_bal)
                       'out_b_val)
          ;; 22
          (case-exprs* (and* (empty?* 'out_b)
                             (and* (send-buf-empty?* 'out_b_bal)
                                   (and* (not* (send-buf-empty?* 'out_b_val))
                                         (=?* 'phase 6))))
                       (fill* 'out_b (send-buf-get* 'out_b_val))
                       'out_b_bal
                       (send-buf-next* 'out_b_val))))
    (:=* (vars* 'out_c
                'out_c_bal
                'out_c_val)
         (list
          ;; 23
          (case-exprs* (and* (empty?* 'out_c)
                             (and* (not* (send-buf-empty?* 'out_c_bal))
                                   (=?* 'phase 6)))
                       (fill* 'out_c (send-buf-get* 'out_c_bal))
                       (send-buf-next* 'out_c_bal)
                       'out_c_val)
          ;; 24
          (case-exprs* (and* (empty?* 'out_c)
                             (and* (send-buf-empty?* 'out_c_bal)
                                   (and* (not* (send-buf-empty?* 'out_c_val))
                                         (=?* 'phase 6))))
                       (fill* 'out_c (send-buf-get* 'out_c_val))
                       'out_c_bal
                       (send-buf-next* 'out_c_val))))
    ;; 25 Phase 6->7
    (:=* (vars* 'in_a_bal
                'in_b_bal
                'in_c_bal
                'in_a_val
                'in_b_val
                'in_c_val
                'phase)
         (list
          (case-exprs* (and* (send-buf-empty?* 'out_a_val)
                             (and* (send-buf-empty?* 'out_b_val)
                                   (and* (send-buf-empty?* 'out_c_val)
                                         (=?* 'phase 6))))
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       7)))
    ;; Phase 7: receive vote replies from acceptors
    (:=* (vars* 'in_a
                'in_a_bal
                'in_a_val)
         (list
          ;; 26
          (case-exprs* (and* (full?* 'in_a)
                             (and* (not* (recv-buf-full?* 'in_a_bal))
                                   (=?* 'phase 7)))
                       (drain* 'in_a)
                       (recv-buf-put* 'in_a_bal (read* 'in_a))
                       'in_a_val)
          ;; 27
          (case-exprs* (and* (full?* 'in_a)
                             (and* (recv-buf-full?* 'in_a_bal)
                                   (and* (not* (recv-buf-full?* 'in_a_val))
                                         (=?* 'phase 7))))
                       (drain* 'in_a)
                       'in_a_bal
                       (recv-buf-put* 'in_a_val (read* 'in_a)))))
    (:=* (vars* 'in_b
                'in_b_bal
                'in_b_val)
         (list
          ;; 28
          (case-exprs* (and* (full?* 'in_b)
                             (and* (not* (recv-buf-full?* 'in_b_bal))
                                   (=?* 'phase 7)))
                       (drain* 'in_b)
                       (recv-buf-put* 'in_b_bal (read* 'in_b))
                       'in_b_val)
          ;; 29
          (case-exprs* (and* (full?* 'in_b)
                             (and* (recv-buf-full?* 'in_b_bal)
                                   (and* (not* (recv-buf-full?* 'in_b_val))
                                         (=?* 'phase 7))))
                       (drain* 'in_b)
                       'in_b_bal
                       (recv-buf-put* 'in_b_val (read* 'in_b)))))
    (:=* (vars* 'in_c
                'in_c_bal
                'in_c_val)
         (list
          ;; 30
          (case-exprs* (and* (full?* 'in_c)
                             (and* (not* (recv-buf-full?* 'in_c_bal))
                                   (=?* 'phase 7)))
                       (drain* 'in_c)
                       (recv-buf-put* 'in_c_bal (read* 'in_c))
                       'in_c_val)
          ;; 31
          (case-exprs* (and* (full?* 'in_c)
                             (and* (recv-buf-full?* 'in_c_bal)
                                   (and* (not* (recv-buf-full?* 'in_c_val))
                                         (=?* 'phase 7))))
                       (drain* 'in_c)
                       'in_c_bal
                       (recv-buf-put* 'in_c_val (read* 'in_c)))))
    ;; 32 Phase 7->8
    (:=* (vars* 'a_mbal
                'b_mbal
                'c_mbal
                'a_mval
                'b_mval
                'c_mval
                'phase)
         (list
          (case-exprs* (and* (recv-buf-full?* 'in_a_bal)
                             (and* (recv-buf-full?* 'in_b_bal)
                                   (and* (recv-buf-full?* 'in_c_bal)
                                         (and* (recv-buf-full?* 'in_a_val)
                                               (and* (recv-buf-full?* 'in_b_val)
                                                     (and* (recv-buf-full?* 'in_c_val)
                                                           (=?* 'phase 7)))))))
                       (recv-buf->nat* 'in_a_bal)
                       (recv-buf->nat* 'in_b_bal)
                       (recv-buf->nat* 'in_c_bal)
                       (recv-buf->nat* 'in_a_val)
                       (recv-buf->nat* 'in_b_val)
                       (recv-buf->nat* 'in_c_val)
                       8)))
    ;; Phase 8: check value
    (:=* (vars* 'phase)
          (list
           ;; 33
           (case-exprs* (and* (=?* 'value 'a_mval)
                              (and* (=?* 'value 'b_mval)
                                    (and* (=?* 'value 'c_mval)
                                          (=?* 'phase 8))))
                        0)
           ;; 34
           (case-exprs* (and* (not* (and* (=?* 'value 'a_mval)
                                          (and* (=?* 'value 'b_mval)
                                                (=?* 'value 'c_mval))))
                              (=?* 'phase 8))
                        255))))))

;; Model a Paxos acceptor

(define acceptor
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
                       (send-buf-next* 'out_prop_val))))
    ;; Phase 4->5: prepare to receive accept message
    (:=* (vars* 'in_prop_bal
                'in_prop_val
                'phase)
         (list
          (case-exprs* (and* (send-buf-empty?* 'out_prop_val)
                             (=?* 'phase 4))
                       (empty-recv-buf*)
                       (empty-recv-buf*)
                       5)))
    ;; Phase 5: receive accept ballot/value from proposer
    (:=* (vars* 'in_prop
                'in_prop_bal
                'in_prop_val)
         (list
          (case-exprs* (and* (full?* 'in_prop)
                             (and* (not* (recv-buf-full?* 'in_prop_bal))
                                   (=?* 'phase 5)))
                       (drain* 'in_prop)
                       (recv-buf-put* 'in_prop_bal (read* 'in_prop))
                       'in_prop_val)
          (case-exprs* (and* (full?* 'in_prop)
                             (and* (recv-buf-full?* 'in_prop_bal)
                                   (and* (not* (recv-buf-full?* 'in_prop_val))
                                         (=?* 'phase 5))))
                       (drain* 'in_prop)
                       'in_prop_bal
                       (recv-buf-put* 'in_prop_val (read* 'in_prop)))))
    ;; Phase 5->6: read accept
    (:=* (vars* 'prop_mbal
                'prop_mval
                'phase)
         (list
          (case-exprs* (and* (recv-buf-full?* 'in_prop_bal)
                             (and* (recv-buf-full?* 'in_prop_val)
                                   (=?* 'phase 5)))
                       (recv-buf->nat* 'in_prop_bal)
                       (recv-buf->nat* 'in_prop_val)
                       6)))
    ;; Phase 6->7: check validity of accept
    (:=* (vars* 'ballot
                'value
                'out_prop_bal
                'out_prop_val
                'phase)
         (list
          (case-exprs* (and* (=?* 'prop_mbal 'prom_bal)
                             (=?* 'phase 6))
                       'prop_mbal
                       'prop_mval
                       (nat->send-buf* 'prop_mbal)
                       (nat->send-buf* 'prop_mval)
                       7)))
    ;; Phase 7: send accept acknowledged message
    (:=* (vars* 'out_prop
                'out_prop_bal
                'out_prop_val)
         (list
          (case-exprs* (and* (empty?* 'out_prop)
                             (and* (not* (send-buf-empty?* 'out_prop_bal))
                                   (=?* 'phase 7)))
                       (fill* 'out_prop (send-buf-get* 'out_prop_bal))
                       (send-buf-next* 'out_prop_bal)
                       'out_prop_val)
          (case-exprs* (and* (empty?* 'out_prop)
                             (and* (send-buf-empty?* 'out_prop_bal)
                                   (and* (not* (send-buf-empty?* 'out_prop_val))
                                         (=?* 'phase 7))))
                       (fill* 'out_prop (send-buf-get* 'out_prop_val))
                       'out_prop_bal
                       (send-buf-next* 'out_prop_val))))
    ;; Phase 7->0: complete
    (:=* (vars* 'phase)
         (list
          (case-exprs* (and* (send-buf-empty?* 'out_prop_val)
                             (=?* 'phase 7))
                       0))))))


(provide proposer
         acceptor)
