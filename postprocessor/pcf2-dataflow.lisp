;; Dataflow analysis frameworks for PCF2 bytecode. We use this to eliminate unnecessary gates that don't contribute to the output

(defpackage :pcf2-dataflow
  (:use :common-lisp :pcf2-bc :setmap :utils)
  (:export load-ops)
;  (:import-from :pcf2-bc :read-bytecode)
)
(in-package :pcf2-dataflow)

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



#|
(defstruct (basic-block
	     (:print-function
	      (lambda  (struct stream depth)
		(declare (ignore depth))
		(format stream "~&PCF2 basic block: ~A~%" (basic-block-id struct))
		(format stream "Gates: ~A~%" (basic-block-gates struct))
		(format stream "Preds: ~AA%" (basic-block-preds struct))
		(format stream "Succs: ~AA%" (block-block-succs struct)) 
	     )))
  (id)
  (gates nil :type list)
  (preds nil :type list)
  (succs nil :type list)
)
|#

(defstruct (wire
	     (:print-function 
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "~&Wire: ~A~%" (wire-id struct))
		(format stream "Index: ~A~%" (wire-idx struct))
		(format stream "Preds: ~AA%" (wire-preds struct))
		(format stream "Succs: ~AA%" (wire-succs struct))
	     )))
  (id) ; the number that comes associated with the wire; analagous to a location in memory because it holds a value but might be reused over the course of a program
  (idx) ; the index of the wire among all wires seen. (Some wires may appear more than once; this is a unique identifier, analagous to a label that idenitifies a piece of memory's value in time.)
  (preds nil :type list) ; a list of indexes
  (succs nil :type list) ; a list of indexes
  (:documentation "This represents a wire, with pointers to its input and output wires. We must pay attention to each occurrence of the wire and not just each wire id, since wire ids may be reused over the course of a circuit.")
  )

;; rules for creating a new wire with unique index rather than re-using the previous:
;; whenever a wire appears in the destination: create a new index for it
;; whenever a wire appears in the ops: do not create a new index, loop it up in the wire map
;; when the same id appears in both the ops and the dest, be sure to get the op's index first!

;; (GATE :DEST 1078 :OP1 947 :OP2 949 :TRUTH-TABLE #*1001 )
;; the wires associated with this gate are all we need to know about it
;;   947 --
;;         \
;;          -- 1078
;;         /
;;   949 --
;; therefore, 947 and 949 are preds of 1078, and 1078 is a succ of each of them

;; (CONST :DEST 1277 :OP1 0 )
;; create a new wire at dst, the value does not matter

;; (COPY :DEST 618 :OP1 585 :OP2 32 )
;; new wire for evertyhing in 618 -> 618 + 32, with links from each wire in 585 + i to 618 + i

;; (MKPTR :DEST 485 )
;; still don't understand this one

;; (INDIR-COPY :DEST 485 :OP1 518 :OP2 32 )
;; figure this one out

;; (COPY-INDIR :DEST 553 :OP1 552 :OP2 32 )
;; see indir-copy

;; (BITS :DEST (453 454 455 456 457 458 459 460 461 462 463 464 465 466 467 468 469 470 471 472 473 474 475 476 477 478 479 480 481 482 483 484) :OP1 453 )
;; I think everything in DEST needs a new wire

(defmacro add-succ (succ wir &body body)
  `(let ((,wir (make-wire
		:id (wire-id ,wir)
		:idx (wire-idx ,wir)
		:preds (wire-preds ,wir)
		:succs (cons ,succ (wire-succs ,wir))
		)))
     ,@body))

(defmacro add-pred (prd wir &body body)
  `(let ((,wir (make-wire
		:id (wire-id ,wir)
		:idx (wire-idx ,wir)
		:preds (cons ,prd (wire-preds ,wir))
		:succs (wire-succs ,wir)
		)))
     ,@body))

(defmacro new-wire (wireid idx wiremap &body body)
  `(let* ((newwire (make-wire 
		    :id ,wireid
		    :idx ,idx))
	  (,wiremap (if (null (map-find ,wireid ,wiremap t))
		     (map-insert ,wireid newwire ,wiremap)
		     (map-insert ,wireid newwire (map-remove ,wireid ,wiremap))))
	(,idx (1+ ,idx)))
    ,@body))

(defmacro close-update ()
  `(list wiremap idx)
)

(defgeneric update-cfg (op wiremap idx)
  (:documentation "update the entities in the wiremap and the cfg for each op that we encounter from ops")
)

(defmethod update-cfg ((op bits) wiremap idx)
  (with-slots (dest) locs
    ;; should create a new wire for every member in locs with no preds and no succs, adding them to the map 
    (close-update)
))

(defmethod update-cfg ((op gate) wiremap idx)
  (with-slots ((dst dest) (o1 op1) (o2 op2)) op
      (new-wire dst idx wiremap
		(add-succ dst o1
		    (add-succ dst o2
			     (add-pred o1 dst
				 (add-pred o2 dst
				     (close-update)
		)))))))

(defun make-cfg (ops)
  (reduce #'(lambda(x y) 
	      (apply #'update-cfg (cons y x)))
	  ops
	  :initial-value (list (map-empty :comp string<)  0)))


(defun load-ops (fname) 
  (with-open-file (inpt fname :direction :input)
    (read-btyecode inpt)))
;      (break))))

#|
(defun get-bytecode ()
  (let bc (((read-bytecode)))
       (break)
       )
)
|#
