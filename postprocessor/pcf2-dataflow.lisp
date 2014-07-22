;; Dataflow analysis frameworks for PCF2 bytecode. We use this to eliminate unnecessary gates that don't contribute to the output

(defpackage :pcf2-postprocessor
  (:use :cl :pcf2-bc :setmap :utils)
#|  (:export make-cfg
           make-cfg-single-ops
           get-function-body
           map-cfg
           ops-from-cfg
           get-lbls-in-order
           basic-block
           basic-block-ops
           basic-block-preds
           basic-block-succs
           flow-backwards
           flow-forwards
           ) |#
)

;;; need:
;;; basic blocks (from pcf2 opcodes)
;;;  predecessors and successors work in the following manner:
;;; inputs to a gate are predecessors
;;; outputs from a gate are successors
;;; simple enough!

;;; the pred/succ map (or cfg) is pretty simple here
;;; will need a function to interpret pcf2 instructions that are not simply GATE
;;; gate --> inputs and outputs are straightforward
;;; copy --> must find the input and output gates and update them accordingly
;;;    we can probably use a simple map for all of this

(defstruct (gate-unit
	     (:print-function 
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "~&PCF Gate Unit: ~A~%" (gate-unit-id struct))
		(format stream "Preds: ~AA%" (gate-unit-preds struct))
		(format stream "Succs: ~AA%" (gate-unit-succs struct))
	     )
  (id)
  (preds nil :type list)
  (succs nil :type list)
  (:documentation "This represents a gate, which is a component of a basic block in the CFG. A block may contain a number of independent gates. "
)

(defstruct basic-block
  
  (id)
  (gates nil :type list)
  (preds nil :type list)
  (succs nil :type list)
)
