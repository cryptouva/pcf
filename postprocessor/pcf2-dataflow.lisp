;; Dataflow analysis frameworks for PCF2 bytecode. We use this to eliminate unnecessary gates that don't contribute to the output

(defpackage :pcf2-dataflow
  (:use :common-lisp :pcf2-bc :setmap :utils)
                                        ; (:export ;wire
                                        ;get-wire-by-idx
                                        ;get-wire-by-lbl
                                        ;get-idx-by-lbl
                                        ;add-succ
                                        ;add-pred
                                        ;live-wire
                                        ;new-wire
                                        ;create-wire
                                        ;map-update-wire
                                        ;map-upsert
                                        ;	   )
                                        ;  (:import-from :pcf2-bc :read-bytecode)
  )
(in-package :pcf2-dataflow)

;;; need:

;;; basic blocks (from pcf2 opcodes)
;;; the pred/succ map (or cfg) is explained:
;;;   must figure out which gates are connected using simple methods (and probably some way of tracking indirection)
;;;   we can accomplish this by tracking the inputs and outputs to gates
;;;   the idea is that given a control flow graph of pcf opcodes, we can generate a more granular graph that idenitifies wires and gates
;;;   we can accomplish some amount of analysis implicitly in this framework by, e.g. observing gen/kill rules during construction. for example: "const" creates a new wire that need not be linked back to its predecessors. Additionally, we will want to propagate constants in the dataflow program using partial gate information (AND w/0 is like loading a 0 const; OR with 1 is like loading a 1 const; NOTs are easy to propagate; XOR is not possible)
;;; right now, we will only deal with constructing the cfg. next we can do gate analysis in a number of ways

;;; ops:
;;;  this is a list of all the PCF2 ops for a given circuit

(defstruct (basic-block
             (:print-function
              (lambda  (struct stream depth)
                (declare (ignore depth))
                (format stream "~&PCF2 basic block: ~A~%" (basic-block-id struct))
                ;; (format stream "Gates: ~A~%" (basic-block-gates struct))
                (format stream "Preds: ~AA%" (basic-block-preds struct))
                (format stream "Succs: ~AA%" (block-block-succs struct)) 
                )))
  (id)
  ;; (gates nil :type list) ;; placeholder for later
  (preds nil :type list)
  (succs nil :type list)
  )


(defgeneric update-cfg (op wiremap wiretable idx)
  (:documentation "update the entities in the wiremap and the cfg for each op that we encounter from ops")
  )

(defmacro definstr (type &body body)
  "PCF instruction processing methods are defined with this macro.  It is a convenience macro that ensures that the method takes the right number of arguments."
  `(defmethod update-cfg ((op ,type) wiremap wiretable idx)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (error "State is null")
          )))



(definstr bits
  ;; TODO: fill in
  ;; no predecessors
  )

(definstr const
  ;; TODO: fill in
  ;; no predecessors
  )

(definstr gate
  ;; TODO: fill in
  ;; predecessors are whichever were the last to use the input gates
  )

;;(defmethod update-cfg ((op mkptr) wiremap wiretable idx)
(definstr mkptr
  ;; TODO: fill in
  ;; predecessor given by argument, successor to come; successor might actually remove this block or move through it, adding its predecessor as well as it
  )
                                        ;(defmethod update-cfg ((op copy) wiremap wiretable idx)
(definstr copy
  ;; TODO: fill in
  ;; predecessors given by op1 (location) and op2 (length)
  )

#|
Let's talk about the following instructions

    1. (CONST :DEST 227 :OP1 1 )
    2. (MKPTR :DEST 227 )
    3. (CONST :DEST 228 :OP1 65 )
    4. (MKPTR :DEST 228 )
    5. (COPY-INDIR :DEST 229 :OP1 228 :OP2 32 )
    6. (INDIR-COPY :DEST 227 :OP1 229 :OP2 32 )

1. load const 1 into 227
2. turn address 227 into a pointer; it now references address 1
3. load const 65 into 228
4. turn 228 into a pointer; it now addresses 1
5. 229 - 260 will now contain copy of 65-96
6. the location that 227 points to will contain a copy of 229-260 which was just loaded there.
So over the course of these 6 instructions, we've copied 65-96 into locations 1-32.
|#

;;(defmethod update-cfg ((op copy-indir) wiremap wiretable idx)
(definstr copy-indir
  ;; TODO: fill in
)

(definstr indir-copy
  ;; TODO: fill in
)

(definstr call
  ;; TODO: fill in
)

(definstr ret
  ;; TODO: fill in
)

(definstr branch
  ;; TODO: fill in
)

(definstr join
  ;; TODO: fill in
)

#| are these obsolete?
(definstr add
  ;; TODO: fill in
)

(definstr sub
  ;; TODO: fill in
)

(definstr mul
  ;; TODO: fill in
)
|#

;; the next few don't need to do anything
(definstr label
  (close-update)
  )

;;(defmethod update-cfg ((op initbase) wiremap wiretable idx)
(definstr initbase
  (close-update)
)

;;(defmethod update-cfg ((op clear) wiremap wiiretable idx)
(definstr clear
  (close-update)
)

(defun make-cfg (ops)
  (reduce #'(lambda(x y) 
              (apply #'update-cfg (cons y x)))
          ops
          :initial-value (list (map-empty :comp string<) (map-empty :comp string<)  0)))
					;wiremap             wiretable               idx

#|
(defun load-ops (fname) 
  (with-open-file (inpt fname :direction :input)
    (read-btyecode inpt)))
;      (break))))
|#
#|
(defun get-bytecode ()
  (let bc (((read-bytecode)))
       (break)
       )
)
|#
