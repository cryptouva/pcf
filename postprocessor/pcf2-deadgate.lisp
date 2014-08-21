;;; this scans a list of instructions to examine def-use chains and eliminates gates from the oplist that never contribute to output

(defpackage :pcf2-deadgate
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-dataflow)
  )

(in-package :pcf2-deadgate)


;; this analysis tracks the uses of wires through a program to determine which are actually useful to the output and which are merely dead weight
;; we do this by tracing wire values backwards from their outputs with output_alice and output_bob to their initialization. wires that are not marked, i.e. do not contribute to the output, may be discarded


