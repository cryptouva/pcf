;;; This is a temporary file to hold useful work while we generalize the pcf2-dataflow file.

;;;  predecessors and successors work in the following manner:
;;; inputs to a gate are predecessors
;;; outputs from a gate are successors

;;; will need a function to interpret pcf2 instructions that are not simply GATE
;;; gate --> inputs and outputs are straightforward
;;; copy --> must find the input and output gates and update them accordingly
;;;    we can probably use a simple map for all of this

;;; DATASTRUCTURES:
;;; wire:
;;;  - label (this is the number identified in the gate)
;;;  - preds (this is a list of idxs of predecessor wires)
;;;  - succs (this is a list of idxs of successor wires)
;;;  - idx (this is a unique identifier)
;;; note: we no longer keep an "index," or a unique wire identifier, because
;;; (a) it will require too much memory for large circuits
;;; (b) uniqueness can be achieved with reaching definition analysis
;;; (c) it is not a good generalization for a dataflow framework

;;; wiremap:
;;; this is a map of idxs to wires

;;; wiretable:
;;;  this is a map of wire ids to current assigned idx

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

(defmacro dead-wire (wire &body body)
  `(let ((,wire (make-wire
                 :lbl (wire-lbl ,wire)
                 :idx (wire-idx ,wire)
                 :preds (wire-preds ,wire)
                 :succs (wire-succs, wire)
                 :live nil)))
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

(definstr bits
    (with-slots (dest) op ; dest should be a list of destination wires
      ;; should create a new wire for every member in dest with no preds and no succs, adding them to the map 
      (reduce (lambda (x y)
                (let ((wmap (first x))
                      (wtable (second x))
                      (ix (third x)))
                  (new-wire y wmap wtable ix
                            (list wmap wtable ix))))
              dest
              :initial-value (list wiremap wiretable idx)))
  )

(definstr const
  (with-slots (dest) op
    (new-wire dest wiremap wiretable idx
      (close-update))))

(definstr gate
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



;;;
;;;
;;;  Dead Gate
;;;
;;;
;;;

(defgeneric def-use-map-update (op idmap output-wires id)
  (:documentation "this function updates our def-use map per instruction so that we may identify gates that never matter")
  )

(defmacro close-update ()
  `(list idmap output-wires uid))

(defmethod def-use-map-update ((op instruction) idmap output-wires uid)
  (close-update))

(defmethod def-use-map-update ((op gate) idmap output-wires uid)
  ()
  )

(defmethod def-use-map-update ((op bits) idmap output-wires uid)
  (with-slots (dest) op
    (reduce (lambda (state d)
              (let ((uid (third state))
                    (idmap (first state))
                    (output-wires (second state))
                    )
                (list 
                 (map-insert d uid idmap)
                 output-wires
                 (1+ uid))))
              dest
              :initial-value (list idmap output-wires uid)
              )))

(defmethod def-use-map-update ((op join) idmap output-wires uid)
  
  )

;; use dest, op1, and op2 (len) to update chains for all of the wires
(defmethod def-use-map-update ((op copy) idmap output-wires uid)
  (close-update)
)

;; end old wire, begin new one
(defmethod def-use-map-update ((op const) idmap output-wires uid)
  (with-slots (dest) op
    (list
     (map-insert (1+ uid) dest idmap)
     output-wires
     (1+ uid))))

(defun make-def-use-map (ops)
  ;; we start with a simple, linear program without any indirection and only one function (main). we use the list of ops to identify def/use chains and eliminate the gates that are inconsequential to the program. later, this function may be extended and adapted for use of code segments rather than whole programs
  ;; the next version of this will use a def-use framework rather than the less general form of this analysis
  ;; we can do this by backtracking from the cfg that we already have on each output_alice and output_bob statement
  ;; use a list of gate # -> unique gate identifier
  (reduce (lambda (state op)
            (apply #'def-use-map-update (cons op state)))
          ops
          :initial-value (list (map-empty) (map-empty) 0)))
