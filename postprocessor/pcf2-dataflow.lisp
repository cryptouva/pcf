;; Dataflow analysis framework for PCF2 bytecode. We use this to eliminate unnecessary gates that don't contribute to the output

(defpackage :pcf2-dataflow
  (:use :common-lisp :pcf2-bc :setmap :utils)
  (:export make-pcf-cfg
           get-label-map
           get-block-preds
           get-block-succs)
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

(defmacro get-block-id (blck)
  (let ((blocksym (gensym)))
    `(let ((,blocksym ,blck))
      (parse-integer (basic-block-id ,blocksym)))))

(defmacro get-block-preds (blck)
  (let ((blocksym (gensym)))
    `(let ((,blocksym ,blck))
      (basic-block-preds ,blocksym))))

(defmacro get-block-succs (blck)
  (let ((blocksym (gensym)))
    `(let ((,blocksym ,blck))
       (basic-block-succs ,blocksym))))

(defmacro get-idx-by-label (targ lbls)
  `(cdr (map-find ,targ ,lbls)))

(defmacro get-block-by-id (id blocks)
  `(cdr (map-find (write-to-string ,id) ,blocks)))

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

(defgeneric cfg-basic-block (op curblock blocks lbls fns idx)
  (:documentation "update the entities in the cfg for each op that we encounter from ops")
  ;; blocks is a map of all idx to basic blocks
  ;; lbls is a map of all of the label names to idxs
  ;; fns is the set of function names
  ;; idx is the index of current op
  )

(defmacro definstr (type &body body)
  "PCF instruction processing methods are defined with this macro.  It is a convenience macro that ensures that the method takes the right number of arguments."
  `(defmethod cfg-basic-block ((op ,type) curblock blocks lbls fns idx)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (add-standard-block)
          )))
#|
(defmacro close-update ()
  (list block blocks lbls fns idx)
)
|#

;; arguments are op curblock blocks lbls idx; 
(defmacro add-standard-block ()
  `(let ((newblock 
          (new-block :id idx :op op))
          ;(make-basic-block :ops (list (write-to-string op)) :id idx)) ; our new block
        ) ; id of last block
    (add-succ idx curblock
        (list newblock ; new last block
              (map-insert (basic-block-id curblock) curblock blocks) ; blocks
              lbls ; lbls
	      fns ; fns
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

(definstr copy-indir)

(definstr indir-copy)

(definstr call
;; this must find its successor in lbls and push its following index to the callstack
)

(definstr ret
;; this must use the callstack to find its immediate successor
)

(definstr branch
  ;; this one gets two successors
  ;; note that unconditional jumps are accomplished with a branch instruction and a constant condition wire. it will be necessary later to remove the second successor.
  (with-slots (targ) op 
    (let ((newblock 
           (new-block :id idx  :op op))
          ;;            (make-basic-block :ops op :id idx)) ; our new block
          ) ; id of last block
      (add-succ idx curblock
          (add-succ (get-idx-by-label targ lbls) newblock ; the other succ will be added at the next instruction
              (list newblock ; new last block
                    (map-insert (basic-block-id curblock) curblock blocks) ; blocks
                    lbls ; lbls
		    fns ; fns
                    (1+ idx) ; idx
                    ))))))

(definstr label
  ;; adding a label has been taken care of already by get-label-map
)

(definstr join
)

(defun get-label-and-fn-map (ops)
  ;; iterate through all of the ops; when hit a label, insert its (name->idx) pair into lbls
  ;; also get the names of all of the functions (other than main) that are called
  (reduce #'(lambda(y op)
                     (declare (optimize (debug 3) (speed 0)))
                     (let ((lbls (first y))
			   (fns (second y))
                           (idx (third y)))
                       (typecase op
                         (label (with-slots (str) op
                                  (list 
                                   (map-insert str idx lbls)
                                   fns
				   (+ 1 idx))))
			 (call (with-slots (fname) op
				 (list
				  lbls
				  (set-insert fns fname)
				  (+ 1 idx))))
                         (t (list lbls fns (+ 1 idx))))))
                 ops
                 :initial-value (list (map-empty :comp string<) (empty-set :comp string<) 0)))


(defun find-preds (f-cfg)
  (declare (optimize (debug 3) (speed 0)))
  ;; for every item in blocks, get its successors and update those to identify a predecessor
  (map-reduce #'(lambda(cfg blockid blck) 
		  (reduce (lambda (cfg* succ)
			    (break)
			    (let ((updateblock (get-block-by-id succ cfg*))
				  (blockid (parse-integer blockid)))
			      (print updateblock)
			      (print succ)
			      (print blockid)
			      (print (get-block-id updateblock))
			      (add-pred blockid updateblock
				  (let ((tstmap 
					 (map-remove
					  (write-to-string (get-block-id updateblock))
					  cfg*)
					 ;(map-upsert
					  ;(write-to-string (get-block-id updateblock))
					  ;updateblock
					  ;cfg*); and let that be the new cfg
					  ))
				    (break)
				    tstmap)
				)))
			  (get-block-succs blck) ; for each successor, add the pred
		 	  :initial-value cfg))
	      f-cfg ;map
	      f-cfg ;state
	      ))

#|
for now, we use a map of strings -> blocks in the "blocks" position, which s the second argument to the reduce.
|#
(defun make-pcf-cfg (ops)
  (declare (optimize (debug 3) (speed 0)))
  (let ((op1 (first ops))
        (restops (rest ops))
	(lbl-fn-map (get-label-and-fn-map ops)))
    (let* ((reduce-forward
            (reduce #'(lambda(x y)
                        ;; (break)
                        (apply #'cfg-basic-block (cons y x)))
                    restops
                    :initial-value (list (new-block :id 0 :op op1)
					 (map-empty :comp string<) 
					 (first lbl-fn-map)
					 (second lbl-fn-map)
					 1)))
           (forward-cfg (map-insert (write-to-string (1- (fifth reduce-forward)))
				    (first reduce-forward)
				    (second reduce-forward)))) ;; insert the last block
      (print (second lbl-fn-map))
      ; (print (third reduce-forward))
      ;; (print forward-cfg)
      (find-preds forward-cfg)
      )))
