#lang rosette/safe

;; Encode the type signature of a syntactic form
(struct ->
  (form
   domain
   codomain)
  #:transparent)

(provide ->)
