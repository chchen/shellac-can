#lang rosette/safe

(struct arduino* (setup loop) #:transparent)
(struct setup* (statements) #:transparent)
(struct loop* (statements) #:transparent)

;; Bool -> Bool
(struct not* (expr) #:transparent)
;; Bool -> Bool -> Bool
(struct and* (left right) #:transparent)
(struct or* (left right) #:transparent)
(struct bool-eq* (left right) #:transparent)


;; Int -> Int
(struct bwnot* (left) #:transparent)
;; Int -> Int -> Bool
(struct lt* (left right) #:transparent)
(struct int-eq* (left right) #:transparent)
;; Int -> Int -> Int
(struct add* (left right) #:transparent)
(struct bwand* (left right) #:transparent)
(struct bwor* (left right) #:transparent)
(struct bwxor* (left right) #:transparent)
(struct shl* (bitvector shift-by) #:transparent)
(struct shr* (bitvector shift-by) #:transparent)

;; Input
;; Pin -> Bool
(struct read* (pin) #:transparent)

;; Type declaration/constant statements
;; Symbol -> Unit
(struct bool* (ident) #:transparent)
;; do we need a byte type?
(struct byte* (ident) #:transparent)
(struct const-int* (ident val) #:transparent)
(struct unsigned-int* (ident) #:transparent)

;; Symbol -> Symbol -> Unit
(struct pin-mode* (pin mode) #:transparent)

;; Output
;; Pin -> Bool -> Unit
(struct write* (pin expr) #:transparent)

;; Variable
;; Var -> Val -> Unit
(struct :=* (var expr) #:transparent)

;; Conditional Execution
;; Bool -> t:Unit -> e:Unit -> Unit
(struct if* (test left right) #:transparent)

(provide arduino*
         setup*
         setup*-statements
         loop*
         loop*-statements
         not*
         and*
         or*
         lt*
         bool-eq*
         int-eq*
         bwnot*
         add*
         bwand*
         bwor*
         bwxor*
         shl*
         shr*
         read*
         bool*
         byte*
         const-int*
         unsigned-int*
         pin-mode*
         write*
         :=*
         if*)
