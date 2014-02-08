;; Constant propagation framework for PCF2 bytecode

(defpackage :constprop
  (:use :cl
        :dataflow
        :pcf2-bc
        :setmap
        :utils)
  (:export const-dataflow-funs)
  )
(in-package :constprop)

(defun constcmp (x y)
  (typecase x
    (number (typecase y
              (number (< x y))
              (symbol (if (equalp y 'unknown)
                          t
                          nil))
              )
            )
    (symbol (if (equalp x 'unknown)
                nil
                t)
            )
    )
  )

(defun const-join-fn (x y)
  "Compute the join operation on the constant propagation lattice."
  (map-map (lambda (k v)
             (aif (map-find k y)
                  (typecase v
                    (number (typecase (cdr it)
                              (number (if (= v (cdr it))
                                          v
                                          'not-const)
                                      )
                              (symbol (cond
                                        ((equalp (cdr it) 'not-const)
                                          'not-const)
                                        ((equalp (cdr it) 'unknown) v)
                                        (t (error "Bad value for (cdr it)"))
                                        )
                                      )
                              )
                            )
                    (symbol (if (equalp v 'not-const)
                                'not-const
                                (cdr it)
                                )
                            )
                    )
                  (error "Address is not present in map of values?!")
                  )
             )
           x
           )
  )

(defparameter empty-s (map-empty :comp constcmp))

(defun const-dataflow-funs (ops)
  "Compute the locations that store constants in this stream of
opcodes.  This returns a map that is keyed by the index of the
operation, with the set of memory locations that store constant values
as its value."
  (let ((funs nil);;(dataflow:get-function-body ops))
        )
    (setmap:map-map (lambda (k v)
                      (declare (ignore k))
                      (multiple-value-bind (in-set in-stack valmap) 
                          (dataflow:flow-forwards #'const-join-fn #'const-flow-fn
                                                  (dataflow:make-cfg-single-ops v)
                                                  (setmap:map-empty :comp string<) 
                                        ;(setmap:set-from-list (loop for i from 0 to 8 collect 
                                        ;                           (cons (* 4 i) 'unknown)) :comp #'constcmp)
                                                  (reduce (lambda (x y)
                                                            (setmap:map-insert (car y) (cdr y) x)
                                                            )
                                                          (loop for i from 0 to 30000 collect (cons i 'unknown))
                                                          :initial-value empty-s
                                                          )
                                                  )
                        (declare (ignore valmap))
                        (list in-set in-stack)
                        )
                      ;
                      )
                    funs)
    )
  )

(defun const-flow-fn (in-set bb)
  (let ((genkill (get-gen-kill bb in-set))
        )
;    (list (third genkill)
          (set-union (first genkill)
                     (set-diff in-set
                               (second genkill)))
;          (fourth genkill)
;          )
    )
  )

(defun get-gen-kill (bb valmap)
  "Get the gen and kill sets for a basic block"
  (declare (type basic-block bb)
           (optimize (debug 3) (speed 0)))
  (let ((gen ;(set-from-list (mapcan #'gen (basic-block-ops bb))))
         (reduce #'(lambda (&optional x y)
                     (set-union (aif x
                                     x
                                     empty-s)
                                ;(map-empty :comp constcmp))
                                (alist->map* y :empty-m empty-s)))
                 (car (reduce #'(lambda (st x)
                                  (let ((lsts (first st)))
                                    (let ((val (gen x valmap bb))
                                          )
                                      (cons (cons (car val) lsts)
                                            (cdr val))
                                      )
                                    )
                                  ) (basic-block-ops bb) :initial-value (list nil nil valmap)))
                 :initial-value empty-s));(map-empty :comp constcmp)))
        (kill ;(set-from-list (mapcan #'kill (basic-block-ops bb)))
         (reduce #'(lambda (&optional x y)
                     (set-union (aif x
                                     x
                                     empty-s);(map-empty :comp constcmp))
                                (alist->map* y :empty-m empty-s)));:comp #'constcmp)))
                 (car (reduce #'(lambda (st x)
                                  (let ((lsts (first st)))
                                    (let ((val (kill x valmap bb))
                                          )
                                      (cons (cons (car val) lsts)
                                            (cdr val))
                                      )
                                    )
                                  ) (basic-block-ops bb) :initial-value (list nil nil valmap)))
                 :initial-value empty-s));(map-empty :comp constcmp)))
        )
    (list gen kill)
    )
  )


(defgeneric gen (op in-set bb)
  )

(defgeneric kill (op in-set bb)
  )

(defmacro def-gen-kill (type &key gen kill)
  `(progn
     (defmethod gen ((op ,type) in-set bb)
       ,gen)
     (defmethod kill ((op ,type) in-set bb)
       ,kill
       )
     )
  )

(def-gen-kill two-op
    :gen (with-slots (dest) op
           (setmap:map-insert dest 'not-const in-set)
           )
    :kill (with-slots (dest) op
            (setmap:map-insert dest 'unknown in-set)
            )
    )

(def-gen-kill instruction)