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

(defgeneric gen (op stack valmap)
  (:documentation "Get the gen set for this op")
  )

(defgeneric kill (op stack valmap)
  (:documentation "Get the kill set for this op")
  )

(defun lcc-const-flow-fn (in-set in-stack valmap bb)
  (let ((genkill (get-gen-kill bb in-stack valmap))
        )
    (print bb)
    (print genkill)
    (list (third genkill)
          (set-union (first genkill)
                     (set-diff in-set
                               (second genkill)))
          (fourth genkill)
          )
    )
  )

(defun get-gen-kill (bb instack valmap)
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
                                  (let ((valmap (third st))
                                        (stack (second st))
                                        (lsts (first st)))
                                    (let ((val (gen x stack valmap))
                                          )
                                      (cons (cons (car val) lsts)
                                            (cdr val))
                                      )
                                    )
                                  ) (basic-block-ops bb) :initial-value (list nil instack valmap)))
                 :initial-value (empty-set)))
        (kill ;(set-from-list (mapcan #'kill (basic-block-ops bb)))
         (reduce #'(lambda (&optional x y)
                     (set-union (aif x
                                     x
                                     (empty-set))
                                (set-from-list y)))
                 (car (reduce #'(lambda (st x)
                                  (let ((stack (second st))
                                        (valmap (third st))
                                        (lsts (first st)))
                                    (let ((val (kill x stack valmap))
                                          )
                                      (cons (cons (car val) lsts)
                                            (cdr val))
                                      )
                                    )
                                  ) (basic-block-ops bb) :initial-value (list nil instack valmap)))
                 :initial-value (empty-set)))
        (stck (second (reduce #'(lambda (st x)
                               (let ((stack (second st))
                                     (lsts (first st))
                                        (valmap (third st)))
                                 (let ((val (kill x stack valmap))
                                       )
                                   (cons (cons (car val) lsts)
                                         (cdr val))
                                   )
                                 )
                               ) (basic-block-ops bb) :initial-value (list nil instack valmap))))
        (valmap (third (reduce #'(lambda (st x)
                                 (let ((stack (second st))
                                       (lsts (first st))
                                       (valmap (third st)))
                                   (let ((val (kill x stack valmap))
                                         )
                                     (cons (cons (car val) lsts)
                                           (cdr val))
                                     )
                                   )
                                 ) (basic-block-ops bb) :initial-value (list nil instack valmap))))
        )
    (list gen kill stck valmap)
    )
  )

(defmacro def-gen-kill (type &key stck vals gen kill)
  `(progn
     (defmethod gen ((op ,type) stack valmap)
       (the (cons list (cons list (cons avl-set t))) (list ,gen ,stck 
                                                           (aif ,vals
                                                                it
                                                                valmap)))
       )

     (defmethod kill ((op ,type) stack valmap)
       (the (cons list (cons list (cons avl-set t))) (list ,kill ,stck 
                                                           (aif ,vals
                                                                it
                                                                valmap)))
       )
     )
  )

(def-gen-kill lcc-instruction
    :stck stack
    :gen nil
    :kill nil
    )

(def-gen-kill callu
    :stck (cons 'not-const (cdr stack))
    :gen nil
    :kill nil
    )

(def-gen-kill calli
    :stck (cons 'not-const (cdr stack))
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

(def-gen-kill addrlp
    :stck (the (cons integer t)
            (cons
             (parse-integer (second (slot-value op 's-args))) stack))
    )

(def-gen-kill cnstu
    :stck (the (cons integer t)
            (cons
             (parse-integer (second (slot-value op 's-args))) stack))
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

(def-gen-kill argu
    :stck (cdr stack)
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
    :vals (cond
            ((or (eql (second stack) 'glob)
                 (eql (second stack) 'args)
                 (null (second stack)))
             valmap)
            (t (let ((addr (second stack)))
                 (declare (type integer addr))
                 (map-insert (second stack) (first stack) valmap)
                 )
               )
            )
    :gen (cond
           ((eql (second stack) 'glob) nil)
           ((eql (second stack) 'args) nil)
           ((eql (first stack) 'not-const) nil)
           ((null (second stack)) nil)
           (t (list (the integer (second stack))))
           )
    :kill (cond
           ((eql (second stack) 'glob) nil)
           ((eql (second stack) 'args) nil)
           ((null (second stack)) nil)
           (t (list (the integer (second stack))))
           )
    )

(def-gen-kill asgni
    :stck (cddr stack)
    :gen (cond
           ((eql (first stack) 'glob) nil)
           ((eql (first stack) 'args) nil)
           ((eql (second stack) 'not-const) nil)
           (t (list (the integer (first stack))))
           )
    :kill (cond
           ((eql (first stack) 'glob) nil)
           ((eql (first stack) 'args) nil)
           (t (list (the integer (first stack))))
           )
    )

(def-gen-kill indiru
    :stck (cons (cdr (map-find (car stack) valmap))
                (cdr stack))
    )

(def-gen-kill indiri
    :stck (cons (cdr (map-find (car stack) valmap)) (cdr stack))
    )