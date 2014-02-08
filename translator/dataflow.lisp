;; Dataflow analysis frameworks for the optimizer
;;
;; This should at least perform constant propagation and dead code
;; elimination

(defpackage :dataflow
  (:use :cl :pcf2-bc :setmap :utils)
  (:export make-cfg
           make-cfg-single-ops
           map-cfg
           ops-from-cfg
           get-lbls-in-order
           basic-block
           basic-block-ops
           basic-block-preds
           basic-block-succs
           flow-backwards
           flow-forwards
           flow-forwards*
           )
  )

(in-package :dataflow)

(defstruct (basic-block
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&Basic block ~A:~%" (basic-block-id struct))
                (format stream "Ops: ~A~%" (basic-block-ops struct))
                (format stream "Preds: ~A~%" (basic-block-preds struct))
                (format stream "Succs: ~A~%" (basic-block-succs struct))
                )
              )
             )
  (id)
  (ops nil :type list)
  (preds)
  (succs)
  (:documentation "This represents a basic block in the control flow graph.")
  )

(defmacro add-op (op bb blkid &body body)
  `(let ((,bb (make-basic-block
               :id ,blkid
               :ops (cons ,op (basic-block-ops ,bb))
               :preds (basic-block-preds ,bb)
               :succs (basic-block-succs ,bb)
               )
           )
         )
     ,@body
     )
  )

(defmacro add-pred (prd bb blkid &body body)
  `(let ((,bb (make-basic-block
               :id ,blkid
               :ops (basic-block-ops ,bb)
               :preds (cons ,prd (basic-block-preds ,bb))
               :succs (basic-block-succs ,bb)
               )
           )
         )
     ,@body
     )
  )

(defmacro add-succ (prd bb blkid &body body)
  `(let ((,bb (make-basic-block
               :id ,blkid
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
  (add-op op curblock blkid
    (list blocks curblock blkid)
    )
  )

(defmethod find-basic-blocks ((op label) blocks curblock blkid)
  (let ((new-block (make-basic-block :ops (list op)))
        )
    (with-slots (str) op
      (add-succ str curblock blkid
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
      (add-op op curblock blkid
        (add-succ targ curblock blkid
          (add-succ (concatenate 'string "fall" blkid) curblock blkid
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
    (add-op op curblock blkid
      (add-succ (concatenate 'string "call" blkid) curblock blkid
        (list (map-insert blkid curblock blocks) new-block (concatenate 'string "call" blkid))
        )
      )
    )
  )

(defmethod find-basic-blocks ((op ret) blocks curblock blkid)
  (let ((new-block (make-basic-block))
        )
    (add-op op curblock blkid
      (add-succ (concatenate 'string "ret" blkid) curblock blkid
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
                                 (add-pred k bb (basic-block-id bb)
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
  (declare (optimize (debug 3) (speed 0))
           (type list ops))
  (let ((cfg (reduce #'(lambda (x y)
                         (declare (optimize (debug 3) (speed 0))
                                  (type instruction y)
                                  (type list x))
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

(defun make-cfg-single-ops (ops)
  "Create a CFG where each basic block consists of a single operation.
This is mainly to simplify the dataflow frameworks i.e. to allow us to
assume that each block is exactly one opcode."
  (declare (optimize (debug 3) (speed 0))
           (type list ops)
           )
  (let ((cfg (reduce (lambda (st x)
                       (let ((cfg (first st))
                             (cblock (second st))
                             (idx (third st))
                             )
                         (declare (type integer idx)
                                  (type avl-set cfg)
                                  (type basic-block cblock)
                                  (type instruction x)
                                  )
                         (let* ((nblock (make-basic-block
                                         :id (typecase x
                                               (label (with-slots (str) x
                                                         (the string str)))
                                               (otherwise (write-to-string (1+ idx)))
                                               )
                                         :ops (list x)
                                         :preds nil
                                         :succs nil
                                         )
                                  )
                                (cblock (make-basic-block
                                         :id (basic-block-id cblock)
                                         :ops (basic-block-ops cblock)
                                         :preds (basic-block-preds cblock)
                                         :succs (cons (basic-block-id nblock)
                                                      (basic-block-succs cblock))
                                         )
                                  )
                                )
                           (list
                            (map-insert (basic-block-id cblock) cblock cfg)
                            nblock
                            (1+ idx)
                            )
                           )
                         )
                       )
                     ops
                     :initial-value (list (map-empty :comp string<) (make-basic-block) 0))
          )
        )
    (let ((cfg (map-insert (basic-block-id (second cfg)) (second cfg) (first cfg)))
          )
      (find-preds cfg)
      )
    )
  )

(defun get-lbls-in-order (ops res &optional (c ""))
  (declare (optimize (debug 3) (speed 0))
           (type list res))
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

(defun map-cfg (cfg fn)
  "Change the CFG according to \"fn\", which rewrites the operations in this cfg node"
  (declare (type (function (string list) list) fn)
           (type avl-set cfg))
  (map-map #'(lambda (k bb)
               (declare (type string k)
                        (type basic-block bb))
               (make-basic-block :id k
                                 :ops (funcall fn k
                                               (basic-block-ops bb))
                                 :preds (basic-block-preds bb)
                                 :succs (basic-block-succs bb))
               )
           cfg)
  )


(defun flow-backwards (join-fn flow-fn cfg out-sets empty-set)
  "Perform a backwards dataflow analysis i.e.:

out(i) = reduce(join-fn, in-sets(succs(i)))
"
  (declare (type avl-set cfg out-sets empty-set)
           (type function join-fn flow-fn)
           (optimize (debug 3) (speed 0)))
  (the avl-set 
    (labels ((out-for-block (bb)
               (let ((new-out (reduce (lambda (x y)
                                        (funcall join-fn x
                                                 (funcall flow-fn 
                                                          (aif (map-find y out-sets)
                                                               (cdr it)
                                                               empty-set
                                                               )
                                                          (cdr (map-find y cfg)))))
                                      (basic-block-succs bb)
                                      :initial-value empty-set))
                     )
                 (list new-out (set-equalp (aif (map-find (basic-block-id bb) out-sets)
                                                (cdr it)
                                                empty-set)
                                           new-out))
                 )
               )
             )
      (let ((res
             (map-reduce #'(lambda (x id y)
                             (declare (optimize (debug 3) (speed 0)))
                             (let ((o-sets (first x))
                                   (done (second x))
                                   )
                               (let ((res (out-for-block y))
                                     )
                                 (list (map-insert id
                                                   (first res)
                                                   o-sets)
                                       (and (second res) done))
                                 )
                               )
                             )
                         cfg 
                         (list out-sets t)
                         )
              )
            )
        (if (second res)
            (first res)
            (flow-backwards join-fn flow-fn cfg (first res) empty-set)
            )
        )
      )
    )
  )

(defun flow-forwards (join-fn flow-fn cfg in-sets empty-set)
  "Perform a backwards dataflow analysis i.e.:

out(i) = reduce(join-fn, in-sets(preds(i)))
"
  (declare (type avl-set cfg in-sets empty-set)
           (type function join-fn flow-fn)
           (optimize (debug 3) (speed 0)))
  (the avl-set 
    (labels ((out-for-block (bb)
               (let ((new-out (reduce (lambda (x y)
                                        (funcall join-fn x
                                                 (funcall flow-fn 
                                                          (aif (map-find y in-sets)
                                                               (cdr it)
                                                               empty-set
                                                               )
                                                          (cdr (map-find y cfg)))))
                                      (basic-block-preds bb)
                                      :initial-value empty-set))
                     )
                 (list new-out (set-equalp (aif (map-find (basic-block-id bb) in-sets)
                                                (cdr it)
                                                empty-set)
                                           new-out))
                 )
               )
             )
      (let ((res
             (map-reduce #'(lambda (x id y)
                             (declare (optimize (debug 3) (speed 0)))
                             (let ((o-sets (first x))
                                   (done (second x))
                                   )
                               (let ((res (out-for-block y))
                                     )
                                 (list (map-insert id
                                                   (first res)
                                                   o-sets)
                                       (and (second res) done))
                                 )
                               )
                             )
                         cfg 
                         (list in-sets t)
                         )
              )
            )
        (if (second res)
            (first res)
            (flow-forwards join-fn flow-fn cfg (first res) empty-set)
            )
        )
      )
    )
  )

(defun flow-forwards* (join-fn flow-fn cfg in-sets1 in-sets2 empty-set)
  "Perform a forwards dataflow analysis for binary flow functions e.g. pointer analysis"
  (declare (type avl-set cfg in-sets1 in-sets2 empty-set)
           (type (function (avl-set avl-set basic-block) list) flow-fn)
           (type (function (list list) list) join-fn)
           (optimize (debug 3) (speed 0)))
  (the avl-set 
    (labels ((out-for-block (bb)
               (let ((new-out (reduce (lambda (x y)
                                         (funcall join-fn x
                                                  (funcall flow-fn 
                                                           (aif (map-find y in-sets1)
                                                                (cdr it)
                                                                empty-set
                                                                )
                                                           (aif (map-find y in-sets2)
                                                                (cdr it)
                                                                empty-set)
                                                           (cdr (map-find y cfg)))))
                                      (basic-block-preds bb)
                                      :initial-value (list empty-set empty-set)))
                     )
                 (list (first new-out)
                       (second new-out)
                       (and (set-equalp (aif (map-find (basic-block-id bb) in-sets1)
                                             (cdr it)
                                             empty-set)
                                        (first new-out))
                            (set-equalp (aif (map-find (basic-block-id bb) in-sets2)
                                             (cdr it)
                                             empty-set)
                                        (second new-out)
                                        )
                            )
                       )
                 )
               )
             )
      (let ((res
             (map-reduce #'(lambda (x id y)
                             (declare (optimize (debug 3) (speed 0)))
                             (let ((o-sets1 (first x))
                                   (o-sets2 (second x))
                                   (done (third x))
                                   )
                               (let ((res (out-for-block y))
                                     )
                                 (list (map-insert id
                                                   (first res)
                                                   o-sets1)
                                       (map-insert id
                                                   (second res)
                                                   o-sets2)
                                       (and (third res) done))
                                 )
                               )
                             )
                         cfg 
                         (list in-sets1 in-sets2 t)
                         )
              )
            )
        (if (third res)
            (first res)
            (flow-forwards* join-fn flow-fn cfg (first res) (second res) empty-set)
            )
        )
      )
    )
  )