;; Dataflow analysis frameworks for PCF2 bytecode. We use this to eliminate unnecessary gates that don't contribute to the output

(defpackage :pcf2-dataflow
  (:use :common-lisp :pcf2-bc :setmap :utils)
  (:export wire
	   get-wire-by-idx
	   get-wire-by-lbl
	   get-idx-by-lbl
	   add-succ
	   add-pred
	   live-wire
	   new-wire
	   create-wire
	   map-update-wire
	   map-upsert
	   )
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

;;; DATASTRUCTURES:
;;; wire:
;;;  - labal (this is the number identified in the gate)
;;;  - idx (this is the unique wire identifier, which is a running count of all unique wires)
;;;  - preds (this is a list of idxs of predecessor wires)
;;;  - succs (this is a list of idxs of successor wires)

;;; wiremap:
;;; this is a map of idxs to wires

;;; wiretable:
;;;  this is a map of wire ids to current assigned idx

;;; ops:
;;;  this is a list of all the PCF2 ops for a given circuit


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
		(format stream "~&Label: ~A~%" (wire-lbl struct))
		(format stream "Index: ~A~%" (wire-idx struct))
		(format stream "Preds: ~A~%" (wire-preds struct))
		(format stream "Succs: ~A~%" (wire-succs struct))
		(format stream "Live: ~A~%" (wire-live struct))
	     )))
  (lbl) ; the number that comes associated with the wire; analagous to a location in memory because it holds a value but might be reused over the course of a program
  (idx) ; the index of the wire among all wires seen. (Some wires may appear more than once; this is a unique identifier, analagous to a label that idenitifies a piece of memory's value in time.)
  (preds nil :type list) ; a list of indexes
  (succs nil :type list) ; a list of indexes
  (live nil :type boolean) ; tells whether the wire is live or can be optimized away
  (:documentation "This represents a wire, with pointers to its input and output wires. We must pay attention to each occurrence of the wire and not just each wire id, since wire ids may be reused over the course of a circuit.")
  )

(defun create-wire (&key label index)
  (make-wire :lbl label
	     :idx index))

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

(defun map-upsert (x val map)
  (if (null (map-find x map t))
      (map-insert x val map)
      (map-insert x val (map-remove x map))))

(defun map-update-wire (wire map)
  (map-upsert (wire-idx wire) wire map))

(defun get-wire-by-idx (idx wiremap)
  (cdr (map-find idx wiremap))
)

(defun get-wire-by-lbl (lbl wiremap wiretable)
  "gets the wire from wiremap with label described by lbl. requires 2 map lookups"
  ;; this will throw an error if either is null - not to be used for checking if a wire location exists
  (get-wire-by-idx (cdr (map-find lbl wiretable)) wiremap)
)

(defun get-idx-by-lbl (lbl wiremap wiretable)
  (wire-idx (get-wire-by-lbl lbl wiremap wiretable))
)

(defmacro close-update ()
  `(list wiremap wiretable idx)
)

(defmacro add-succ (succ wire &body body)
  `(let ((,wire (make-wire
		:lbl (wire-lbl ,wire)
		:idx (wire-idx ,wire)
		:preds (wire-preds ,wire)
		:succs (cons ,succ (wire-succs ,wire))
		:live (wire-live ,wire) ; shouldn't really need this assignment, keep it for good measure
		)))
     ,@body))

(defmacro add-pred (prd wire &body body)
  `(let ((,wire (make-wire
		:lbl (wire-lbl ,wire)
		:idx (wire-idx ,wire)
		:preds (cons ,prd (wire-preds ,wire))
		:succs (wire-succs ,wire)
		:live (wire-live ,wire) ; shouldn't really need this assignment, keep it for good measure
		)))
     ,@body))

;; should this be a macro or a function?
(defmacro live-wire (wire &body body)
  `(let ((,wire (make-wire
		 :lbl (wire-lbl ,wire)
		 :idx (wire-idx ,wire)
		 :preds (wire-preds ,wire)
		 :succs (wire-succs, wire)
		 :live t)))
     ;;,wire)
     ,@body)
)

(defmacro new-wire (lbl wiremap wiretable idx &body body)
  ;; new entry in wire table
  ;; new wire entered into wiremap
  (let ((newwire (gensym)))
  `(let* ((,newwire (create-wire :label ,lbl :index ,idx))
	  (,wiretable (map-upsert ,lbl ,idx ,wiretable))
	  (,wiremap (map-update-wire ,newwire ,wiremap))
	  (,idx (1+ ,idx)))
     ,@body)))


(defgeneric update-cfg (op wiremap wiretable idx)
  (:documentation "update the entities in the wiremap and the cfg for each op that we encounter from ops")
)

(defmethod update-cfg ((op bits) wiremap wiretable idx)
  (with-slots (dest) op ; dest should be a list of destination wires
    ;; should create a new wire for every member in dest with no preds and no succs, adding them to the map 
    (reduce (lambda (x y)
	      (let ((wmap (first x))
		    (wtable (second x))
		    (ix (third x)))
		(new-wire y wmap wtable ix
		  (list wmap wtable ix))))
	    dest
	    :initial-value (list wiremap wiretable idx))))

(defmethod update-cfg ((op const) wiremap wiretable idx)
  (with-slots (dest) op
    (new-wire dest wiremap wiretable idx
      (close-update))))

(defmethod update-cfg ((op gate) wiremap wiretable idx)
  ;; bear in mind that wires may be inputs to themselves
  (with-slots ((dst dest) (o1 op1) (o2 op2)) op
    (let ((op1-wire (get-wire-by-lbl o1 wiremap wiretable))
	  (op2-wire (get-wire-by-lbl o2 wiremap wiretable))
	  (op1-idx (get-idx-by-lbl o1 wiremap wiretable))
	  (op2-idx (get-idx-by-lbl o2 wiremap wiretable)))
      (new-wire dst idx wiremap wiretable
	(let ((dstwire (get-wire-by-lbl dst wiremap wiretable)))
	  (add-succ dst op1-wire 
	      (add-succ dst op2-wire
		  (add-pred op1-idx dstwire
		      (add-pred op2-idx dstwire
			  (close-update)
			)))))))))

(defmethod update-cfg ((op mkptr) wiremap wiretable idx)
  ;; TODO: fill in
)

(defmethod update-cfg ((op copy) wiremap wiretable idx)
  ;; TODO: fill in
)

(defmethod update-cfg ((op copy-indir) wiremap wiretable idx)
  ;; TODO: fill in
)

(defmethod update-cfg ((op indir-copy) wiremap wiretable idx)
  ;; TODO: fill in
)

(defmethod update-cfg ((op call) wiremap wiretable idx)
  ;; TODO: fill in
)

(defmethod update-cfg ((op ret) wiremap wiretable idx)
  ;; TODO: fill in
)

;; the next few don't need to do anything
(defmethod update-cfg ((op label) wiremap wiretable idx)
  (close-update)
  )

(defmethod update-cfg ((op initbase) wiremap wiretable idx)
  (close-update)
)

(defmethod update-cfg ((op clear) wiremap wiiretable idx)
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
