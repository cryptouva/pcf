;;; this iterates through a control-flow graph to perform faint-variable analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-faintgate
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-dataflow)
  (:export faint-flow-fn)
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
;;; DepKill_n(x)   = { Opd(e) Intersect Var   n is assignment x=e, x /e *x*
;;;                     /0                otw
;;; explanation: x is not faint if it is used towards the output of the program
;;; DepKill states that if a variable is not faint after a definition, then all of the variables used to define it are not faint before the definition. This achieves the transitive property we're looking for.


;;; it is less efficient to track all of the faint gates than it is to track all of the live gates, in reverse, as they move through the program. This alters the calculation of flow function
;;; in standard faint variable analysis, T = Var, Bottom = {}, and conf = Union
;;; but this tracks all of the _faint_ gates, while it is more efficient for us to track Live Gates
;;; so instead, **WE SWITCH THE DEFINITIONS OF GEN AND KILL** (their const and dep versions follow respectively)
;;; now, Top is {}, bottom is {Var}, and the confluence operator is set-union

(defparameter confluence-operator #'set-union)  ;;#'set-inter)
;; "top" is Var and is represented by *lattice-top* from pcf2-dataflow

(defparameter output-functions (set-from-list (list "output_alice" "output_bob") :comp #'string<))
(defparameter input-functions (set-from-list (list "alice" "bob") :comp #'string<))

(defmacro top-set ()
  `(empty-set))
  ;;`(set-insert (empty-set) *lattice-top*))

(defun confluence-op (set1 set2)
  ;; if either set is "top," return the other set
  (declare (optimize (debug 3)(speed 0)))
  (cond
    ((set-equalp set1 (top-set)) set2)
    ((set-equalp set2 (top-set)) set1)
    (t 
     (funcall confluence-operator set1 set2))))

(defun conf-union (set1 set2)
  (cond
    ((set-equalp set1 (top-set)) set2)
    ((set-equalp set2 (top-set)) set1)
    (t (set-union set1 set2))))

(defun get-out-sets (blck cfg conf)
  (reduce
   (lambda (temp-out succ)
     (let ((succ-out (get-block-out-set (get-block-by-id succ cfg))))
       (funcall conf temp-out succ-out)))
   (get-block-succs blck)
   :initial-value (get-block-out-set blck)))

(defun faint-flow-fn (blck cfg state)
  (declare (optimize (speed 0) (debug 3)))
  (let* ((in-flow (get-out-sets blck cfg #'confluence-op)) 
         (flow (conf-union
                (set-diff in-flow (kill (get-block-op blck) in-flow))
                (gen (get-block-op blck) in-flow))))
    ;;(print flow)
    flow))

(defgeneric gen (op flow-data)
  (:documentation "this function describes how to compute the gen part of the flow function for each op") 
  )

(defgeneric kill (op flow-data)
  (:documentation "this function describes how to compute the kill part of the flow function for each op")
)

(defgeneric const-gen (op)
  (:documentation "this function describes how to compute the constant gen part of the flow function for each op")
)


(defgeneric dep-gen (op flow-data)
  (:documentation "this function describes how to compute the dependent gen part of the flow function for each op")
)

(defgeneric const-kill (op)
  (:documentation "this function describes how to compute the constant kill part of the flow function for each op")
)

(defgeneric dep-kill (op)
  (:documentation "this function describes how to compute the dependent kill part of the flow function for each op")
)

(defmethod gen (op flow-data)
  ;; gen = const_gen union dep_gen
  (conf-union (const-gen op) (dep-gen op flow-data)))

(defmethod kill (op flow-data)
  (declare (ignore flow-data))
  ;; kill = const-kill uniond ep_kill
  (conf-union (const-kill op)(dep-kill op)))

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
  `(defmethod dep-gen ((op ,type) flow-data)
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


(def-gen-kill join
    ;; if any of the wires are live, then the join should be live
    :dep-gen (with-slots (dest op1) op
                 (let ((opwires (set-from-list op1)))
                   (if (set-equalp (empty-set) (set-inter opwires flow-data))
                       (empty-set)
                       (singleton dest))))

    ;; if the dest is a member of the bits to join, then don't kill it; else, do since this is its definition
    :const-kill (with-slots (dest op1) op
                    (if (member dest op1)
                        (empty-set)
                        (singleton dest)))
    )

(def-gen-kill bits
    ;; if the wire is live, then the outputs should be
    :dep-gen (with-slots (dest op1) op
                 (if (set-member op1 flow-data)
                     (set-from-list dest)
                     (empty-set)))
    ;; if the op is a member of the dests, then don't kill it
    :const-kill (with-slots (dest op1) op
                  (if (member op1 dest)
                      (empty-set)
                      (singleton op1)))
    )


(def-gen-kill const
    ;; if x = const, add x to gen
    :const-kill (with-slots (dest) op
                  (singleton dest)))

(def-gen-kill gate
    :dep-gen (with-slots (op1 op2 dest) op
               (if (set-member dest flow-data)
                   (set-from-list (list op1 op2))
                   (empty-set)))
    :const-kill (with-slots (op1 op2 dest) op
                  (if (or (equalp op1 dest) (equalp op2 dest))
                      (empty-set)
                      (singleton dest))))

(def-gen-kill add
    :dep-gen (with-slots (op1 op2 dest) op
               (if (set-member dest flow-data)
                   (set-from-list (list op1 op2))
                   (empty-set)))
    :const-kill (with-slots (op1 op2 dest) op
                  (if (or (equalp op1 dest) (equalp op2 dest))
                      (empty-set)
                      (singleton dest))))
(def-gen-kill sub
    :dep-gen (with-slots (op1 op2 dest) op
               (if (set-member dest flow-data)
                   (set-from-list (list op1 op2))
                   (empty-set)))
    :const-kill (with-slots (op1 op2 dest) op
                  (if (or (equalp op1 dest) (equalp op2 dest))
                      (empty-set)
                      (singleton dest))))
(def-gen-kill mul
    :dep-gen (with-slots (op1 op2 dest) op
               (if (set-member dest flow-data)
                   (set-from-list (list op1 op2))
                   (empty-set)))
    :const-kill (with-slots (op1 op2 dest) op
                  (if (or (equalp op1 dest) (equalp op2 dest))
                      (empty-set)
                      (singleton dest))))
(def-gen-kill copy
    ;; if dest is live (not faint), then whatever it copies will also be useful
    :dep-gen (with-slots (op1 op2 dest) op
               (if (set-member dest flow-data)
                   (if (equalp op2 1)
                       (singleton op1)
                       (set-from-list (loop for i from op1 to (+ op1 op2) collect i)))
                   (empty-set)))
    :const-kill (with-slots (op1 op2 dest) op
                  (if (and (> dest op1) (< dest (+ op1 op2)))
                      (empty-set)
                      (if (equalp 1 op2)
                          (singleton dest)
                          (set-from-list
                           (loop for i from dest to (+ dest op2) collect i)))))
    )

(def-gen-kill initbase)
(def-gen-kill clear)

;; the following instructions need to know more about the previous ones
;; it is unlikely that the indirection instructions will really alter the flow of a program, since we seldom perform operations directly on them; however, where global state is important to the program we must keep track

(def-gen-kill mkptr
;; has no effect; the loaded constant will take care of this for us.
)

(def-gen-kill copy-indir
    ;; 
)
(def-gen-kill indir-copy
    ;;
)

(def-gen-kill call
    :const-gen (with-slots (newbase fname) op
                 (if (set-member fname output-functions)
                     (set-from-list (loop for i from (- newbase 32) to (- newbase 1) collect i))
                     (empty-set)))
    :const-kill (with-slots (newbase fname) op
                  (if (set-member fname input-functions)
                      (set-from-list (loop for i from (- newbase 32) to (- newbase 1) collect i))
                      (empty-set)))
)

(def-gen-kill ret)
(def-gen-kill branch)
(def-gen-kill label)
