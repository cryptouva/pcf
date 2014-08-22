;;; this scans a list of instructions to examine def-use chains and eliminates gates from the oplist that never contribute to output

(defpackage :pcf2-deadgate
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-dataflow)
  (:export faint-analysis-flow)
  )

(in-package :pcf2-deadgate)


;; this analysis tracks the uses of wires through a program to determine which are actually useful to the output and which are merely dead weight
;; we do this by tracing wire values backwards from their outputs with output_alice and output_bob to their initialization. wires that are not marked, i.e. do not contribute to the output, may be discarded

;;; Gen_n(x) = ConstGen_n Union DepGen_n(x)
;;; Kill_n(x) = ConstKill_n Union DepKill_n(x)

;;; in general, Gen and Kill for faint variable analysis are:
;;; (clearly, this is a backward data flow problem)
;;;
;;; In_n = F_n(Out_n)
;;; Out_n = { BI   n is end
;;;           Meet (s in succ(n) In_s)  Otherwise

;;; ConstGen_n = { {x} n is assignment x=e, x /e Opd(e) 
;;;                {x} n is read(x) ;; read is always alice() or bob()
;;;                /0  otw
;;; DepGen_n(x) = /0

;;; ConstKill_n    = { {x}  n is use(x) ;; here, out use(x) comes in output_alice and output_bob, or 
;;;                    /0   otw
;;; DepKill_n(x)   = { Opd(e) Meet Var   n is assignment x=e, x/e *x*
;;;                     /0                otw

(defparameter *confluence-operator* set-inter)
;; "top" is Var

(defgeneric faint-flow-fn (op)
  (:documentation "this function describes how an operation performs its flow function")
  )

(defmacro def-flow (type &body body)
  `(defmethod (op ,type)
       ,@body
     ))


