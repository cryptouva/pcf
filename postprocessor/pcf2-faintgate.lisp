;;; this iterates through a control-flow graph to perform faint-variable analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-faintgate
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-dataflow)
  (:export faint-analysis-flow)
  )

(in-package :pcf2-faintgate)

;; this analysis tracks the uses of wires through a program to determine which are actually useful to the output and which are merely dead weight
;; we do this by tracing wire values backwards from their outputs with output_alice and output_bob to their initialization. wires that do not contribute to the output may be discarded
;;; definition: A variable x e Var is _faint_ at a program point *u* if along every path from u to End, it is neither not used before being defined or is used to define a faint variable (this is the complement of liveness)

;;; f_n(x) = (x-Kill_n(x) Union Gen_n

;;; Faint variable analysis is a backwards data flow problem
;;; since it is an all-paths analysis, the confluence operator is Intersection
;;; and since the most aggressive optimization is to declare all variables faint, the "top" value is Var (or all variables)

;;; Gen_n(x) = ConstGen_n Union DepGen_n(x)
;;; Kill_n(x) = ConstKill_n Union DepKill_n(x)

;;; in general, Gen and Kill for faint variable analysis are:
;;;
;;; In_n = F_n(Out_n)
;;; Out_n = { BI   n is end
;;;           Meet (s in succ(n) In_s)  Otherwise
;;; (remember, In_s is passed to the next block, Out_n is an input to this block)

;;; ConstGen_n = { {x} n is assignment x=e, x /e Opd(e) 
;;;                {x} n is read(x) ;; read is always alice() or bob()
;;;                /0  otw
;;; DepGen_n(x) = /0
;;;
;;; explanation: variables become faint before every assignment to them (because this is a backwards flow, this is like saying that a variable becomes faint when it will be redefined before its next use)

;;; ConstKill_n    = { {x}  n is use(x) ;; here, out use(x) comes in output_alice and output_bob, or 
;;;                    /0   otw
;;; DepKill_n(x)   = { Opd(e) Intersect Var   n is assignment x=e, x/e *x*
;;;                     /0                otw
;;; explanation: x is not faint if it is used towards the output of the program
;;; DepKill simply states that all of the operands of the expression which are variables hit the kill list; if a variable appears on both the lhs and rhs of an assignment, it does not become faint before the assignment, since that value is still important to the output

(defparameter confluence-operator #'set-inter)
;; "top" is Var

(defmacro top-set ()
  `(set-insert (empty-set) "top"))

(defun confluence-op (set1 set2)
  ;; if either set is "top," return the other set
  (cond
    ((set-equalp set1 (top-set)) set2)
    ((set-equalp set2 (top-set)) set1)
    (t 
     (funcall confluence-operator set1 set2))))

(defun get-out-sets (blck cfg)
  (reduce
   (lambda (temp-out succ)
     (let ((succ-out (get-block-out-set (get-block-by-id succ cfg))))
       (funcall confluence-op temp-out succ-out)))
   (get-block-succs blck)
   :initial-value (get-block-out-set (get-block-by-id blck))))

(defgeneric faint-flow-fn (blck)
  (:documentation "this function describes how an operation performs its flow function")
  )

(defgeneric gen (op blck)
  (:documentation "this function describes how to compute the gen part of the flow function for each op") 
  )

(defgeneric kill (op blck)
  (:documentation "this function describes how to compute the kill part of the flow function for each op")
)

(defmethod faint-flow-fn (blck)
  (set-union
   (set-diff (get-out-sets blck) (kill (get-block-op blck)))
   (gen (get-block-op blck))))
