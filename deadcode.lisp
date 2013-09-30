;; Dead code elimination

(defpackage :deadcode 
  (:use :cl 
        :dataflow 
        :pcf2-bc
        :setmap
        :utils)
  (:export liveness-flow-fn)
  )
(in-package :deadcode)

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

(defun liveness-flow-fn (out-set bb)
  "(out-set - kill) + gen"
  (let ((genkill (get-gen-kill bb))
        )
    (set-union (first genkill)
               (set-diff out-set
                         (second genkill)))
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
  (let ((gen (reduce #'(lambda (&optional x y)
                         (set-union (aif x
                                         x
                                         (empty-set))
                                    (set-from-list y)))
                     (mapcar #'gen (basic-block-ops bb))
                     :initial-value (empty-set)))
        (kill (reduce #'(lambda (&optional x y)
                         (set-union (aif x
                                         x
                                         (empty-set))
                                    (set-from-list y)))
                      (mapcar #'kill (basic-block-ops bb))
                      :initial-value (empty-set))
          )
        )
    (list gen kill)
    )
  )

(defun update-live-for-op (live-in op)
  "live_out = gen \union (live_in - kill)"
  (let ((gen (gen op))
        (kill (kill op))
        )
    (set-union gen (set-diff live-in kill))
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
    :gen (loop for i from 0 to 500
            collect
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

(def-gen-kill call
    :gen (with-slots (newbase) op
           (loop for i from 0 to newbase collect i))
    :kill nil
    )

(def-gen-kill mkptr
    :gen (with-slots (dest) op
           (list dest)
           )
    :kill (with-slots (dest) op
            (list dest)
            )
    )