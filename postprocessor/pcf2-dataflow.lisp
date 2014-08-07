;; Dataflow analysis framework for PCF2 bytecode. We use this to eliminate unnecessary gates that don't contribute to the output

(defpackage :pcf2-dataflow
  (:use :common-lisp :pcf2-bc :setmap :utils)
  (:export make-pcf-cfg
           get-label-map)
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
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&PCF2 basic block ~A:~%" (basic-block-id struct))
                (format stream "Ops: ~A~%" (basic-block-ops struct))
                (format stream "Preds: ~A~%" (basic-block-preds struct))
                (format stream "Succs: ~A~%" (basic-block-succs struct))
                )
              )
             )
  (id)
  (ops nil :type list)
  (preds nil :type list)
  (succs nil :type list)
  (:documentation "This represents a basic block in the control flow graph.")
  )

(defmacro new-block (&key id op)
  `(make-basic-block
   :id (write-to-string ,id)
   :ops (list ,op)))

;; op is an opcode, bb is the block itself
(defmacro add-op (op bb &body body)
  `(let ((,bb (make-basic-block
               :id (basic-block-id ,bb)
               :ops (cons ,op (basic-block-ops ,bb))
               :preds (basic-block-preds ,bb)
               :succs (basic-block-succs ,bb)
               )))
     ,@body))

;; prd is an index, bb is the block itself
(defmacro add-pred (prd bb &body body)
  `(let ((,bb (make-basic-block
               :id (basic-block-id ,bb)
               :ops (basic-block-ops ,bb)
               :preds (cons ,prd (basic-block-preds ,bb))
               :succs (basic-block-succs ,bb))))
     ,@body))

;; succ is an index, bb is the block itself
(defmacro add-succ (succ bb &body body)
  `(let ((,bb (make-basic-block
               :id (basic-block-id ,bb)
               :ops (basic-block-ops ,bb)
               :preds (basic-block-preds ,bb)
               :succs (cons ,succ (basic-block-succs ,bb)))))
     ,@body))

(defgeneric find-basic-block (op curblock blocks lbls idx)
  (:documentation "update the entities in the wiremap and the cfg for each op that we encounter from ops")
  ;; blocks is a map of all idx to basic blocks
  ;; lbls is a map of all of the label names to idxs
  ;; idx is the index of current op
  )

(defmacro definstr (type &body body)
  "PCF instruction processing methods are defined with this macro.  It is a convenience macro that ensures that the method takes the right number of arguments."
  `(defmethod find-basic-block ((op ,type) curblock blocks lbls idx)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (add-standard-block)
          )))
#|
(defmacro close-update ()
  (list block blocks lbls idx)
)
|#

(defmacro add-standard-block ()
  `(let ((newblock 
          (new-block :id idx :op op))
          ;(make-basic-block :ops (list (write-to-string op)) :id idx)) ; our new block
        ) ; id of last block
    (add-succ idx curblock
        (list newblock ; new last block
              (map-insert (basic-block-id curblock) curblock blocks) ; blocks
              lbls ; lbls
              (1+ idx) ; idx
              ))))

;;; the following instructions need no special treatment
(definstr bits)

(definstr const)

(definstr gate)

(definstr mkptr)

(definstr copy)

(definstr add)

(definstr sub)

(definstr mul)

(definstr initbase)

(definstr clear)


;;(defmethod update-cfg ((op copy-indir) wiremap wiretable idx)
(definstr copy-indir
  (add-standard-block))

(definstr indir-copy
  (add-standard-block))

(definstr call
  (add-standard-block))

(definstr ret
  (add-standard-block))

(definstr branch
  ;; this one gets two successors
  ;; note that unconditional jumps are accomplished with a branch instruction and a constant condition wire. it will be necessary later to remove the second successor.
  (with-slots (targ) op 
    (let ((newblock 
           (new-block :id idx  :op op))
          ;;            (make-basic-block :ops op :id idx)) ; our new block
          ) ; id of last block
      (add-succ idx curblock
          (add-succ (map-find targ lbls) newblock ; the other succ will be added at the next instruction
              (list newblock ; new last block
                    (map-insert (basic-block-id curblock) curblock blocks) ; blocks
                    lbls ; lbls
                    (1+ idx) ; idx
                    ))))))

(definstr label
  ;; adding a label has been taken care of already by get-label-map
)

(definstr join
)

(defun get-label-map (ops)
  ;; iterate through all of the ops; when hit a label, insert its (name->idx) pair into lbls
  (reduce #'(lambda(y op)
              (declare (optimize (debug 3) (speed 0)))
              (let ((lbls (first y))
                    (idx (second y)))
                (typecase op
                  (label (with-slots (str) op
                           (list 
                            (map-insert str idx lbls)
                            (+ 1 idx))))
                  (t (list lbls (+ 1 idx))))))
          ops
          :initial-value (list (map-empty :comp string<) 0)))
#|
(defun find-preds (blocks)
  (declare (optimize (debug 3) (speed 0)))
  ;; for every item in blocks, get its successors and update those to identify a predecessor
  ;; must do this one backwards; can't get it on the first pass through because of branching jumps
)
|#

(defun make-pcf-cfg (ops)
  (declare (optimize (debug 3) (speed 0)))
  (let ((forward-cfg
         (reduce #'(lambda(x y) 
                     ;;(break)
                     (apply #'find-basic-block (cons y x)))
                 ops
                 :initial-value (list (make-basic-block :id "$START" :ops nil) (map-empty :comp string<) (first (get-label-map ops))  1))
          ))
    (print forward-cfg)
    ))
                                        ;curblock             blocks             lbls                   idx 

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
