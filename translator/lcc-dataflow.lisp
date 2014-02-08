;; Dataflow analysis frameworks for LCC bytecode.  We need this to
;; determine, at the very least, where things are dependent on inputs.

(defpackage :lcc-dataflow
  (:use :cl :pcf2-bc :setmap :utils :lcc-bc)
  (:export make-cfg
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
           )
  (:shadowing-import-from :lcc-bc import export)
  )

(in-package :lcc-dataflow)

(defstruct (basic-block
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&LCC basic block ~A:~%" (basic-block-id struct))
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
               :succs (typecase ,bb
                        (jumpv (basic-block-succs ,bb))
                        (otherwise (cons ,prd (basic-block-succs ,bb)))
                        )
               )
           )
         )
     ,@body
     )
  )

(defgeneric find-basic-blocks (op blocks curblock blkid idx)
  (:documentation "Construct a set of basic blocks from a list of opcodes")
  )

(defmethod find-basic-blocks ((op lcc-instruction) blocks curblock blkid idx)
  ;; (add-op op curblock blkid
  ;;   (list blocks curblock blkid)
  ;;   )
  (let ((new-block (make-basic-block :ops (list op)))
        )
    (add-succ (write-to-string idx) curblock blkid
      (list (map-insert blkid curblock blocks) new-block (write-to-string idx) (1+ idx))
      )
    )
  )

(defmethod find-basic-blocks ((op labelv) blocks curblock blkid idx)
  (let ((new-block (make-basic-block :ops (list op)))
        )
    (with-slots (s-args) op
      (let ((str (first s-args))
            )
        (add-succ str curblock blkid
          (list (map-insert blkid curblock blocks) new-block str (1+ idx))
          )
        )
      )
    )
  )

(defmethod find-basic-blocks ((op proc) blocks curblock blkid idx)
  (let ((new-block (make-basic-block :ops (list op)))
        )
    (with-slots (s-args) op
      (let ((str (first s-args))
            )
        (add-succ str curblock blkid
          (list (map-insert blkid curblock blocks) new-block str (1+ idx))
          )
        )
      )
    )
  )

(defmethod find-basic-blocks ((op cnd-jump-instruction) blocks curblock blkid idx)
  (let ((new-block (make-basic-block :ops (list op)))
        )
    (with-slots (s-args) op
      (let ((targ (first s-args))
            )
        (add-succ targ curblock blkid
          (add-succ (concatenate 'string "fall" blkid) curblock blkid
            (list (map-insert blkid curblock blocks) new-block (concatenate 'string "fall" blkid) (1+ idx))
            )
          )
        )
      )
    )
  )

(defmethod find-basic-blocks ((op jumpv) blocks curblock blkid idx)
  (let ((new-block (make-basic-block :ops (list op)))
        (adrop (first (basic-block-ops curblock)))
        )
    (declare (type addrgp adrop))
    (with-slots (s-args) adrop
      (let ((targ (first s-args))
            )
        (add-succ (write-to-string idx) curblock blkid
          (list (map-insert blkid curblock blocks) new-block (write-to-string idx) (1+ idx))
          )
        )
      )
    )
  )

(defmethod find-basic-blocks ((op callv) blocks curblock blkid idx)
  (let ((new-block (make-basic-block :ops (list op)))
        )
    (add-succ (concatenate 'string "call" blkid) curblock blkid
      (list (map-insert blkid curblock blocks) new-block (concatenate 'string "call" blkid) (1+ idx))
      )
    )
  )

(defmethod find-basic-blocks ((op calli) blocks curblock blkid idx)
  (let ((new-block (make-basic-block :ops (list op)))
        )
;    (add-op op curblock blkid
      (add-succ (concatenate 'string "call" blkid) curblock blkid
        (list (map-insert blkid curblock blocks) new-block (concatenate 'string "call" blkid) (1+ idx))
        )
;      )
    )
  )

(defmethod find-basic-blocks ((op callu) blocks curblock blkid idx)
  (let ((new-block (make-basic-block :ops (list op)))
        )
;    (add-op op curblock blkid
      (add-succ (concatenate 'string "call" blkid) curblock blkid
        (list (map-insert blkid curblock blocks) new-block (concatenate 'string "call" blkid) (1+ idx))
        )
      )
;    )
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
                                  (type lcc-instruction y)
                                  (type list x))
                         (apply #'find-basic-blocks (cons y x))
                         )
                     ops :initial-value (list (map-empty :comp string<) (make-basic-block) "" 0))
          )
        )
    (let ((cfg (map-insert (third cfg) (second cfg) (first cfg)))
          )
      (map-map (lambda (key val)
                 (declare (ignore key))
                 (make-basic-block
                  :id (basic-block-id val)
                  :ops (reverse (basic-block-ops val))
                  :preds (basic-block-preds val)
                  :succs (basic-block-succs val)
                  )
                 )
               (find-preds
                cfg
                )
               )
      )
    )
  )

(defun make-cfg-single-ops (ops)
  (make-cfg ops)
  )

(defun make-cfg-single-ops* (ops)
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
                                  (type lcc-instruction x)
                                  )
                         (let* ((nblock (make-basic-block
                                         :id (typecase x
                                               (labelv (with-slots (s-args) x
                                                         (the string (first s-args))))
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
                     (the lcc-instruction (first ops))
                   ((or proc labelv) (with-slots (str) (first ops)
                                       str))
                   (cnd-jump-instruction (concatenate 'string "fall" c))
                   ((or retu reti) (concatenate 'string "ret" c))
                   ((or callu calli callv) (concatenate 'string "call" c))
                   (t c)
                   )
              )
            )
        (get-lbls-in-order (rest ops) 
                           (typecase
                               (the lcc-instruction (first ops))
                             (cnd-jump-instruction
                              (cons str res))
                             ((or proc labelv)
                              (cons str res))
                             ((or retu reti)
                              (cons str res))
                             ((or callv callu calli)
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

(defun get-function-body (ops &optional (rmap (map-empty :comp string<)))
  "Divide the list of ops into a map of function names to function bodies."
  (declare (type avl-set rmap)
           (type list ops))
  (if (null ops)
      rmap
      (typecase (first ops)
        (proc (with-slots (s-args) (first ops)
                (get-function-body (rest ops)
                                   (map-insert (first s-args)
                                               (loop for op in ops 
                                                  until (typep op 'endproc)
                                                  collect op) rmap))
                )
              )
        (otherwise (get-function-body (rest ops) rmap)
                   )
        )
    )
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

(defun flow-forwards (join-fn flow-fn cfg in-sets in-stacks valmaps empty-set &optional (cnt 0))
  (declare (optimize (debug 3) (speed 0))
           (ignorable cnt))
  (the avl-set
    (labels ((do-flow-forwards (cblock in-sets in-stacks valmaps visited done)
               (declare (optimize (debug 3) (speed 0)))
               (if (or (null cblock) (null (basic-block-id cblock)))
                   (list in-sets in-stacks valmaps done)
                   (let ((new-out (reduce (lambda (x y)
                                        (let ((res (funcall flow-fn 
                                                          (aif (map-find y in-sets t)
                                                               (cdr it)
                                                               empty-set
                                                               )
                                                          (cdr (map-find y in-stacks t))
                                                          (cdr (map-find y valmaps t));(third x) ;valmap 
                                                          (cdr (map-find y cfg t))))
                                              )
                                          (list (first res)
                                                (funcall join-fn 
                                                         (second x)
                                                         (second res))
                                                (third res))
                                          )
                                        )
                                      (basic-block-preds cblock)
                                      :initial-value (list (cdr (map-find (basic-block-id cblock) in-stacks t)) 
                                                           empty-set
                                                           (aif (cdr (map-find (basic-block-id cblock) valmaps t))
                                                                it
                                                                (map-empty)
                                                                )
                                                           )))
                         )
                     (let* ((nblock 
                             (map-find
                              (loop 
                                for id in (basic-block-succs cblock)
                                 when (not (map-find id visited t))
                                 unless (null id)
                                 return id)
                              cfg)
                              )
                            (bbid (basic-block-id cblock))
                            (nbid (car nblock))
                            (done (and done (set-equalp 
                                             (aif (map-find bbid in-sets t)
                                                  (cdr it)
                                                  empty-set
                                                  )
                                             (second new-out))))
                            ;(ostack (first (funcall flow-fn (second new-out) (first new-out) (third new-out) cblock)))
                            )
                       (declare (type string bbid nbid))
                       ;(format *error-output* "~&bbid: ~A~%nbid: ~A~%nblock: ~A~%new-out: ~A~%" bbid nbid nblock new-out)
                       (do-flow-forwards 
                           (cdr nblock)
                         (map-insert bbid (second new-out) in-sets)
                         (map-insert bbid (first new-out) in-stacks) ;; <-- We need to use bbid here, because new-out is the result of our *predecessor* operations; hence, the stack from new-out is the *input* stack for this operation.
                         (map-insert nbid (third new-out) valmaps)
                         (map-insert bbid t visited)
                         done)
                       )
                     )
                   )
               )
             )
      (let ((ret (do-flow-forwards (cdr (map-find "1" cfg)) in-sets in-stacks valmaps (map-empty :comp string<) t)))
        ;(print (cons (fourth ret) cnt))
        (if (fourth ret)
            (values (first ret)
                    (second ret)
                    (third ret)
                    )
            (flow-forwards join-fn flow-fn cfg (first ret) (second ret) (third ret) empty-set (1+ cnt))
            )
        )
      )
    )
  )

(defun flow-forwards* (join-fn flow-fn cfg in-sets in-stacks empty-set &optional (cnt 0))
  "Perform a forwards dataflow analysis i.e.:

out(i) = reduce(join-fn, in-sets(preds(i)))
"
  (declare (type avl-set cfg in-sets empty-set)
           (type function join-fn flow-fn)
           (type (integer 0 10) cnt)
           (optimize (debug 3) (speed 0)))
  (the avl-set 
    (labels ((out-for-block (bb)
               (let ((new-out (reduce (lambda (x y)
                                        (let ((res (funcall flow-fn 
                                                          (aif (map-find y in-sets)
                                                               (cdr it)
                                                               empty-set
                                                               )
                                                          (car x);(map-find y in-stacks)
                                                          (cdr (map-find y cfg))))
                                              )
                                          (list (first res)
                                                (funcall join-fn 
                                                         (second x)
                                                         (second res)))
                                          )
                                        )
                                      (basic-block-preds bb)
                                      :initial-value (list (map-find (basic-block-id bb) in-stacks) empty-set)))
                     )
                 (list (second new-out)
                       (first new-out)
                       (set-equalp (aif (map-find (basic-block-id bb) in-sets)
                                        (cdr it)
                                        empty-set)
                                   (second new-out))
                       )
                 )
               )
             )
      (let ((res
             (map-reduce #'(lambda (x id y)
                             (declare (optimize (debug 3) (speed 0)))
                             (let ((o-sets (first x))
                                   (o-stacks (second x))
                                   (done (third x))
                                   )
                               (let ((res (out-for-block y))
                                     )
                                 (list (map-insert id
                                                   (first res)
                                                   o-sets)
                                       (map-insert id (second res) o-stacks)
                                       (and (third res) done))
                                 )
                               )
                             )
                         cfg 
                         (list in-sets in-stacks t)
                         )
              )
            )
        (if (third res)
            (values (first res) cnt)
            (flow-forwards join-fn flow-fn cfg (first res) (second res) empty-set (1+ cnt))
            )
        )
      )
    )
  )