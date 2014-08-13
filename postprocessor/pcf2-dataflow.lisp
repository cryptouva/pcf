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



(defmacro push-stack (val stack &body body)
  `(let ((,stack (cons ,val stack)))
     ,@body))

(defmacro pop-stack (val stack &body body)
  `(let ((,val (car ,stack))
         (,stack (cdr ,stack)))
     ,@body))

(defmacro insert-block (id val blocks &body body)
  `(let ((,blocks (map-insert ,id ,val ,blocks)))
     ,@body))

(defgeneric cfg-basic-block (next-op cur-op blocks lbls fns idx callstack)
  (:documentation "update the entities in the cfg for each op that we encounter from ops")
  ;; blocks is a map of all idx to basic blocks
  ;; lbls is a map of all of the label names to idxs
  ;; fns is the set of function names
  ;; idx is the index of current op
  )

(defmacro definstr (type &body body)
  "PCF instruction processing methods are defined with this macro.  It is a convenience macro that ensures that the method takes the right number of arguments."
  `(defmethod cfg-basic-block (next-op (cur-op ,type) blocks lbls fns idx callstack)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (add-standard-block)
          )))

(defmacro close-add-block ()
  `(insert-block idx newblock blocks
                 (list next-op
                       blocks
                       lbls
                       fns
                       (1+ idx)
                       callstack)))


(defmacro add-standard-block () ; next-op cur-op blocks lbls fns idx
  `(let ((newblock (new-block :id idx :op cur-op)))
     (add-succ (1+ idx) newblock
         (close-add-block))))

(defmethod cfg-basic-block ((next-op label) (cur-op instruction) blocks lbls fns idx callstack)
  (with-slots (str) next-op
    (cond
      ((set-member str fns) ;; if we're about to declare a function, it doesn't get added as a successor right now. main is preceded by initbase (this is handled elsewhere) and functions will get their successors from the call instruction 
       (let ((newblock (new-block :id idx :op cur-op)))
          (close-add-block))) 
      (t 
       (typecase cur-op
         ;; not every instruction can be followed by "label," so here we identify them
         (branch (branch-instr))
         (t (add-standard-block)))))))

(defmethod cfg-basic-block (next-op (cur-op instruction) blocks lbls fns idx callstack)
  (add-standard-block))

(definstr initbase
  (let ((newblock (new-block :id idx :op cur-op)))
    ;; this one's successor is ALWAYS main
    (add-succ (get-idx-by-label "main" lbls) newblock
        (close-add-block))))

(definstr call
;; this must find it successor in lbls and somehow communicate its location to the ret that follows.
;; this about recursive procedures
)

(definstr ret ;; doesn't get an immediate successor
  (let ((newblock (new-block :id idx :op cur-op)))
    (close-add-block)))
;; this must use the callstack to find its immediate successor

(defmacro branch-instr ()
  `(with-slots (targ) cur-op
    (let ((newblock (new-block :id idx :op cur-op)))
      (add-succ (1+ idx) newblock
          (add-succ (get-idx-by-label targ lbls) newblock
              (close-add-block))))))

(definstr branch
  (branch-instr))
;; this one gets two successors
  ;; note that unconditional jumps are accomplished with a branch instruction and a constant condition wire. it will be necessary later to remove the second successor.

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
                 :initial-value (list (map-empty :comp #'string<) (empty-set :comp #'string<) 0)))


(defun find-preds (f-cfg)
  (declare (optimize (debug 3) (speed 0)))
  ;; for every item in blocks, get its successors and update those to identify a predecessor
  (map-reduce #'(lambda(cfg blockid blck) 
		  (reduce (lambda (cfg* succ)
			    ; (break)
			    (let ((updateblock (get-block-by-id succ cfg*))
				  (blockid (parse-integer blockid)))
			      (add-pred blockid updateblock
				  (map-insert
                                   (write-to-string (get-block-id updateblock))
                                   updateblock
                                   cfg*)
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
    (print lbl-fn-map)
    (let* ((reduce-forward
            (reduce #'(lambda(x y)
                        ;; (break)
                        (apply #'cfg-basic-block (cons y x)))
                    restops
                    :initial-value (list op1
					 (map-empty :comp #'string<) 
					 (first lbl-fn-map)
					 (second lbl-fn-map)
					 1
                                         nil)))
           (forward-cfg (map-insert (write-to-string (fifth reduce-forward))
                                    (first reduce-forward)
                                    (second reduce-forward)))) ;; insert the last block
;      (find-preds forward-cfg)
      forward-cfg
      )))
