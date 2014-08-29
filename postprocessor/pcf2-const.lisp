;;; this iterates through a control-flow graph to perform constant-propagation analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-const
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-dataflow)
  (:export const-flow-fn)
  )

(in-package :pcf2-faintgate)

;;; this analysis tracks the uses of constants through a program to determine if we can eliminate some gates by propagating constants and to help with other dataflow analyses
;;; A variable x e Var has a constant value c e Const at a program point u if for every path reaching u along which a definition of x reaches u, the value of x is c.

;;; f_n(x) = (x-Kill_n(x) Union Gen_n

;;; Constant propagation is forward data flow problem
;;; the confluence operation is defined in terms of applying conf-hat on pairs of the same variable
;; ForAll x1,x2 e L, x1 conf x2 = { <z, dx conf-hat dy > | <z,dx> e x1, <z,dy> e x2, x e Var }

;;; Gen_n(x) = ConstGen_n Union DepGen_n(x)
;;; Kill_n(x) = ConstKill_n Union DepKill_n(x)

;;; in general, Gen and Kill for constant propagation are:
;;;
;;; In_n = { BI                          n is Start
;;;          Meet (p in pred(n)) Out_p   Otw
;;; Out_n = f_n(In_n)
;;; (remember, Out_n is passed to the next block, In_n is an input to this block)

;;; ConstGen_n = { {<x,eval(e,Top)>}  n is assignment x=e, Opd(e) subset Const 
;;;                {<x,bottom-hat>} n is read(x) ;; read is always alice() or bob()
;;;                /0  otw
;;; DepGen_n(x) = { <x,d>    n is assignment x=e, <x,d> e *x*
;;;                  \0      otw
;;;


;;; ConstKill_n    =  /0
;;; DepKill_n(x)   = { {<x,d>} n is assignment x=e, <x,d> e *x*
;;;                    {<x,d>} n is read(x), <x,d> e *x*
;;;                     /0                otw
;;; explanation:

;;; val(e,x) = { c if e is c e Const
;;;              d if e is x e Car, <x,d> e *x*
;;;

(defparameter confluence-operator #'set-inter)
;; "top" is Var and is represented by *lattice-top* from pcf2-dataflow

(defmacro top-set ()
  `(set-insert (empty-set) *lattice-top*))

(defun constcmp (x y)
  (typecase x
    (number (typecase y
              (number (< x y))
              (symbol (if (equalp y 'unknown)
                          t
                          nil))))
    (symbol (if (equalp x 'unknown)
                nil
                (typecase y ;;must compare if both x and y are symbols
                  (number t)
                  (symbol (if (equalp y 'unknown)
                              t
                              nil ;; x and y are both 'not-const
                              ))))

(defun pcf2-const-join-fn (x y)
  
  )

(defun confluence-op (set1 set2)
  ;; if either set is "top," return the other set
  (cond
    ((set-equalp set1 (top-set)) set2)
    ((set-equalp set2 (top-set)) set1)
    (t 
     (funcall confluence-operator set1 set2))))


(defun get-out-sets (blck cfg conf)
  (reduce
   (lambda (temp-out succ)
     (let ((succ-out (get-block-out-set (get-block-by-id succ cfg))))
       (funcall #'conf temp-out succ-out)))
   (get-block-succs blck)
   :initial-value (get-block-out-set blck)))

(defun const-flow-fn (blck cfg state)
  (declare (optimize (speed 0) (debug 3)))
  (let ((flow (set-union
               (set-diff (get-out-sets blck cfg #'confluence-op) (kill (get-block-op blck)))
               (gen (get-block-op blck)))))
    flow))

(defgeneric gen (op)
  (:documentation "this function describes how to compute the gen part of the flow function for each op") 
  )

(defgeneric kill (op)
  (:documentation "this function describes how to compute the kill part of the flow function for each op")
)

(defgeneric const-gen (op)
  (:documentation "this function describes how to compute the constant gen part of the flow function for each op")
)

(defgeneric dep-gen (op)
  (:documentation "this function describes how to compute the dependent gen part of the flow function for each op")
)

(defgeneric const-kill (op)
  (:documentation "this function describes how to compute the constant kill part of the flow function for each op")
)

(defgeneric dep-kill (op)
  (:documentation "this function describes how to compute the dependent kill part of the flow function for each op")
)

(defmethod gen (op)
  ;; gen = const_gen union dep_gen
  (let ((gen-set (set-union (const-gen op) (dep-gen op))))
    (print "op:")
    (print op)
    (print "gen:")
    (print gen-set)
    gen-set))

(defmethod kill (op)
  ;; kill = const-kill union gep_kill
  ;;(break)
  (let ((kill-set (set-union (const-kill op) (dep-kill op))))
    (print "kill:")
    (print kill-set)))

(defmacro gen-kill-standard ()
  ;; for faint variable analysis, standard is always empty set
  `(empty-set))

;;; macros to define const-gen, dep-gen, const-kill, and dep-kill

(defmacro def-const-gen (type &body body)
  `(defmethod const-gen ((op ,type))
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

(defmacro def-dep-gen (type &body body)
  `(defmethod dep-gen ((op ,type))
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

(defmacro def-const-kill (type &body body)
  `(defmethod const-kill ((op ,type))
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

(defmacro def-dep-kill (type &body body)
  `(defmethod dep-kill ((op ,type))
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

;; and the macro to write const-gen, dep-gen, const-kill, and dep-kill for each instruction
(defmacro def-gen-kill (type &key (const-gen nil) (dep-gen nil) (const-kill nil) (dep-kill nil))
  `(progn
     (def-const-gen ,type ,const-gen)
     (def-dep-gen ,type ,dep-gen) ; dep-gen always /0 in faint analysis
     (def-const-kill ,type ,const-kill)
     (def-dep-kill ,type ,dep-kill)
  ))

(def-gen-kill bits)

(def-gen-kill join)

(def-gen-kill gate)

(def-gen-kill const)

(def-gen-kill add)

(def-gen-kill sub)

(def-gen-kill mul)

(def-gen-kill copy)

(def-gen-kill initbase)
(def-gen-kill clear)

(def-gen-kill mkptr)
(def-gen-kill copy-indir)
(def-gen-kill indir-copy)
(def-gen-kill call)
(def-gen-kill ret)
(def-gen-kill branch)
(def-gen-kill label)
