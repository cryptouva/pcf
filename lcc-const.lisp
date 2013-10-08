;; Constant detection for LCC
;;
;; This analysis is meant to detect memory locations that are constant
;; within the scope of some function e.g. loop indices.  We will just
;; assume that these locations are not tampered with outside of the
;; function via e.g. some indirection.
;;
;; We use gen/kill sets here.  The gen set contains any locations
;; where a constant is placed; the kill set contains all locations
;; that are assigned to.  For binary operations, the result is
;; constant iff both operands are constants.
;;
;; This can be viewed as "constant propagation lite"

(defpackage :lcc-const 
  (:use :cl :utils :setmap :lcc-bc :lcc-dataflow)
  (:export lcc-const-flow-fn)
  (:shadow import export)
  )
(in-package :lcc-const)

(defgeneric gen (op stack)
  (:documentation "Get the gen set for this op")
  )

(defgeneric kill (op stack)
  (:documentation "Get the kill set for this op")
  )

(defun lcc-const-flow-fn (in-set in-stack bb)
  (let ((genkill (get-gen-kill bb in-stack))
        )
    (list (third genkill)
          (set-union (first genkill)
                     (set-diff in-set
                               (second genkill)))
          )
    )
  )

(defun get-gen-kill (bb instack)
  "Get the gen and kill sets for a basic block"
  (declare (type basic-block bb)
           (optimize (debug 3) (speed 0)))
  (let ((gen ;(set-from-list (mapcan #'gen (basic-block-ops bb))))
         (reduce #'(lambda (&optional x y)
                     (set-union (aif x
                                     x
                                     (empty-set))
                                (set-from-list y)))
                 (car (reduce #'(lambda (st x)
                                  (let ((stack (car st))
                                        (lsts (cdr st)))
                                    (let ((val (gen x stack))
                                          )
                                      (cons (cons (car val) lsts)
                                            (cdr val))
                                      )
                                    )
                                  ) (basic-block-ops bb) :initial-value (cons instack nil)))
                 :initial-value (empty-set)))
        (kill ;(set-from-list (mapcan #'kill (basic-block-ops bb)))
         (reduce #'(lambda (&optional x y)
                     (set-union (aif x
                                     x
                                     (empty-set))
                                (set-from-list y)))
                 (car (reduce #'(lambda (st x)
                                  (let ((stack (car st))
                                        (lsts (cdr st)))
                                    (let ((val (kill x stack))
                                          )
                                      (cons (cons (car val) lsts)
                                            (cdr val))
                                      )
                                    )
                                  ) (basic-block-ops bb) :initial-value (cons instack nil)))
                 :initial-value (empty-set)))
          (stck (cdr (reduce #'(lambda (st x)
                                  (let ((stack (car st))
                                        (lsts (cdr st)))
                                    (let ((val (kill x stack))
                                          )
                                      (cons (cons (car val) lsts)
                                            (cdr val))
                                      )
                                    )
                                  ) (basic-block-ops bb) :initial-value (cons instack nil))))
        )
    (list gen kill stck)
    )
  )

(defmacro def-gen-kill (type &key stck gen kill)
  `(progn
     (defmethod gen ((op ,type) stack)
       (the (cons list list) (cons ,gen ,stck))
       )

     (defmethod kill ((op ,type) stack)
       (the (cons list list) (cons ,kill ,stck))
       )
     )
  )

(def-gen-kill lcc-instruction
    :stck stack
    :gen nil
    :kill nil
    )

(def-gen-kill callu
    :stck (cons 'notconst (cdr stack))
    :gen nil
    :kill nil
    )

(def-gen-kill calli
    :stck (cons 'notconst (cdr stack))
    :gen nil
    :kill nil
    )

(def-gen-kill callv
    :stck (cdr stack)
    :gen nil
    :kill nil
    )

(def-gen-kill addrgp
    :stck (cons 'glob stack)
    :gen nil
    :kill nil
    )

(def-gen-kill addrfp
    :stck (cons 'args stack)
    :gen nil
    :kill nil
    )

(def-gen-kill two-arg-instruction
    :stck (let ((o1 (first stack))
                (o2 (second stack))
                )
            (if (or (eql 'not-const o1)
                    (eql 'not-const o2)
                    )
                (cons 'not-const (cddr stack))
                (cons 'const (cddr stack))
                )
            )
    )

(def-gen-kill cmp-jump-instruction
    :stck (let ((o1 (first stack))
                (o2 (second stack))
                )
            (if (or (eql 'not-const o1)
                    (eql 'not-const o2)
                    )
                (cons 'not-const (cddr stack))
                (cons 'const (cddr stack))
                )
            )
    )

(def-gen-kill one-arg-instruction
    :stck (let ((op (first stack))
                )
            (if (eql 'not-const op)
                (cons 'not-const (cdr stack))
                (cons 'const (cdr stack))
                )
            )
    )

(def-gen-kill asgnu
    :stck (cddr stack)
    :gen (cond
           ((eql (first stack) 'glob) nil)
           ((eql (first stack) 'args) nil)
           ((eql (second stack) 'not-const) nil)
           (t (first stack))
           )
    :kill (cond
           ((eql (first stack) 'glob) nil)
           ((eql (first stack) 'args) nil)
           (t (first stack))
           )
    )

(def-gen-kill asgni
    :stck (cddr stack)
    :gen (cond
           ((eql (first stack) 'glob) nil)
           ((eql (first stack) 'args) nil)
           ((eql (second stack) 'not-const) nil)
           (t (first stack))
           )
    :kill (cond
           ((eql (first stack) 'glob) nil)
           ((eql (first stack) 'args) nil)
           (t (first stack))
           )
    )

(def-gen-kill indiru
    :stck (cons 'valat stack)
    )

(def-gen-kill indiri
    :stck (cons 'valat stack)
    )