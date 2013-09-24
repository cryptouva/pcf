;; Dataflow analysis frameworks for the optimizer
;;
;; This should at least perform constant propagation and dead code
;; elimination

(defpackage :dataflow
  (:use :cl :pcf2-bc :setmap)
  (:export make-cfg
           ops-from-cfg
           get-lbls-in-order
           get-gen-kill
           )
  )

(in-package :dataflow)

(defstruct (basic-block
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&Basic block:~%")
                (format stream "Preds: ~A~%" (basic-block-preds struct))
                (format stream "Succs: ~A~%" (basic-block-succs struct))
                )
              )
             )
  (ops)
  (preds)
  (succs)
  (:documentation "This represents a basic block in the control flow graph.")
  )

(defmacro add-op (op bb &body body)
  `(let ((,bb (make-basic-block
               :ops (cons ,op (basic-block-ops ,bb))
               :preds (basic-block-preds ,bb)
               :succs (basic-block-succs ,bb)
               )
           )
         )
     ,@body
     )
  )

(defmacro add-pred (prd bb &body body)
  `(let ((,bb (make-basic-block
               :ops (basic-block-ops ,bb)
               :preds (cons ,prd (basic-block-preds ,bb))
               :succs (basic-block-succs ,bb)
               )
           )
         )
     ,@body
     )
  )

(defmacro add-succ (prd bb &body body)
  `(let ((,bb (make-basic-block
               :ops (basic-block-ops ,bb)
               :preds (basic-block-preds ,bb)
               :succs (cons ,prd (basic-block-succs ,bb))
               )
           )
         )
     ,@body
     )
  )

(defgeneric find-basic-blocks (op blocks curblock blkid)
  (:documentation "Construct a set of basic blocks from a list of opcodes")
  )

;; For most instructions, we do not terminate the block
(defmethod find-basic-blocks ((op instruction) blocks curblock blkid)
  (add-op op curblock
    (list blocks curblock blkid)
    )
  )

(defmethod find-basic-blocks ((op label) blocks curblock blkid)
  (let ((new-block (make-basic-block :ops (list op)))
        )
    (with-slots (str) op
      (add-succ str curblock
        (list (map-insert blkid curblock blocks) new-block str)
        )
      )
    )
  )

(defmethod find-basic-blocks ((op branch) blocks curblock blkid)
  (declare (optimize (debug 3) (speed 0)))
  (let ((new-block (make-basic-block))
        )
    (with-slots (targ) op
      (add-op op curblock
        (add-succ targ curblock
          (add-succ (concatenate 'string "fall" blkid) curblock
            (list (map-insert blkid curblock blocks) new-block (concatenate 'string "fall" blkid))
            )
          )
        )
      )
    )
  )

(defmethod find-basic-blocks ((op call) blocks curblock blkid)
  (let ((new-block (make-basic-block))
        )
    (add-op op curblock
      (add-succ (concatenate 'string "call" blkid) curblock
        (list (map-insert blkid curblock blocks) new-block (concatenate 'string "call" blkid))
        )
      )
    )
  )

(defmethod find-basic-blocks ((op ret) blocks curblock blkid)
  (let ((new-block (make-basic-block))
        )
    (add-op op curblock
      (add-succ (concatenate 'string "ret" blkid) curblock
        (list (map-insert blkid curblock blocks) new-block (concatenate 'string "ret" blkid))
        )
      )
    )
  )

(defun find-preds (blocks)
  (declare (optimize (debug 3) (speed 0)))
  (map-reduce (lambda (st k v)
                (reduce
                 (lambda (st x)
                   (map-insert x
                               (let ((bb (cdr (map-find x st)))
                                     )
                                 (add-pred k bb
                                   bb
                                   )
                                 )
                               st
                               )
                   )
                 (basic-block-succs v)
                 :initial-value st)
                )
              blocks blocks;(map-empty :comp string<)
              )
  )

(defun make-cfg (ops)
  "Return a control-flow graph from a list of PCF ops.  This is a map
of basic block IDs to basic blocks."
  (declare (optimize (debug 3) (speed 0)))
  (let ((cfg (reduce #'(lambda (x y)
                         (declare (optimize (debug 3) (speed 0))
                                  (type instruction y))
                         (apply #'find-basic-blocks (cons y x))
                         )
                     ops :initial-value (list (map-empty :comp string<) (make-basic-block) ""))
          )
        )
    (let ((cfg (map-insert (third cfg) (second cfg) (first cfg)))
          )
      
      (find-preds
       cfg
       )
      )
    )
  )

(defun get-lbls-in-order (ops res &optional (c ""))
  (declare (optimize (debug 3) (speed 0)))
  (if (null ops)
      (reverse res)
      (let ((str (typecase
                     (first ops)
                   (label (with-slots (str) (first ops)
                            str))
                   (branch (concatenate 'string "fall" c))
                   (ret (concatenate 'string "ret" c))
                   (call (concatenate 'string "call" c))
                   (t c)
                   )
              )
            )
        (get-lbls-in-order (rest ops) 
                           (typecase
                               (first ops)
                             (branch
                              (cons str res))
                             (label
                              (cons str res))
                             (ret
                              (cons str res))
                             (call
                              (cons str res))
                             (t res))
                           str)
        )
      )
  )

(defun ops-from-cfg (cfg lbls-in-order)
  (declare (optimize (debug 3) (speed 0)))
  (labels ((flatten-ops (lbls)
             (if lbls
                 (append
                  (reverse (basic-block-ops (cdr (map-find (first lbls) cfg))))
                  (flatten-ops (rest lbls))
                  )
                 )
             )
           )
    (flatten-ops lbls-in-order)
    )
  )

;; Dead code elimination

(defstruct deadcode-state
  (in-sets)
  (out-sets)
  )

(defmacro update-in-sets (st new-in &body body)
  `(let ((old-in (deadcode-state-in-sets ,st))
         )
     (let ((,st (make-deadcode-state
                 :in-sets ,new-in
                 :out-sets (deadcode-state-out-sets ,st))
             )
           )
       ,@body
       )
     )
  )

(defmacro update-out-sets (st new-out &body body)
  `(let ((old-out (deadcode-state-out-sets ,st))
         )
     (let ((,st (make-deadcode-state
                 :out-sets ,new-out
                 :in-sets (deadcode-state-in-sets ,st))
             )
           )
       ,@body
       )
     )
  )

(defgeneric gen (op)
  (:documentation "Get the gen set for this op")
  )

(defgeneric kill (op)
  (:documentation "Get the kill set for this op")
  )

(defun get-gen-kill (bb)
  "Get the gen and kill sets for a basic block"
  (declare (type basic-block bb)
           (optimize (debug 3) (speed 0)))
  (let ((gen (apply #'nconc (mapcar #'gen (basic-block-ops bb)))
          )
        (kill (apply #'nconc (mapcar #'kill (basic-block-ops bb)))
          )
        )
    (list (set-from-list gen) (set-from-list kill))
    )
  )

(defmacro def-gen-kill (type &key gen kill)
  `(progn
     (defmethod gen ((op ,type))
       (the list ,gen)
       )

     (defmethod kill ((op ,type))
       (the list ,kill)
       )
     )
  )

(def-gen-kill instruction
    :gen nil
    :kill nil
    )

(def-gen-kill two-op
    :gen (with-slots (op1 op2) op
           (declare (type integer op1 op2))
           (list op1 op2)
           )
    :kill (with-slots (dest) op
            (declare (type integer dest))
            (list dest)
            )
    )

(def-gen-kill one-op
    :gen (with-slots (op1) op
           (declare (type integer op1))
           (list op1)
           )
    :kill (with-slots (dest) op
            (declare (type integer dest))
            (list dest)
            )
    )

(def-gen-kill bits
    :gen (with-slots (op1) op
           (declare (type integer op1))
           (list op1)
           )
    :kill (with-slots (dest) op
            (declare (type list dest))
            dest
            )
    )

(def-gen-kill join
    :gen (with-slots (op1) op
           (declare (type list op1))
           op1
           )
    :kill (with-slots (dest) op
            (declare (type (integer 0) dest))
            (list dest)
            )
    )

(def-gen-kill copy
    :gen (with-slots (op1 op2) op
           (declare (type integer op1)
                    (type (integer 1) op2))
           (loop for i from 0 to (1- op2) collect (+ op1 i))
           )
    :kill (with-slots (dest op2) op
            (declare (type integer dest)
                     (type (integer 1) op2))
            (loop for i from 0 to (1- op2) collect (+ dest i))
            )
    )

(def-gen-kill copy-indir
    :gen (loop for i from 0 to 19999 collect
            i
              )
    :kill (with-slots (dest op2) op
            (declare (type integer dest)
                     (type (integer 1) op2))
            (loop for i from 0 to (1- op2) collect (+ dest i))
            )
    )

(def-gen-kill indir-copy
    :gen (with-slots (op1 op2) op
           (declare (type (integer 0) op1)
                    (type (integer 1) op2))
           (loop for i from 0 to (1- op2) collect (+ op1 i))
           )
    :kill nil
    )