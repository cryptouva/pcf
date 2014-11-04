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
  (:use :cl :utils :setmap :setmap-rle :lcc-bc :lcc-dataflow)
  (:export lcc-const-flow-fn const-dataflow-funs not-const)
  (:shadow import export)
  )
(in-package :lcc-const)

(defun constcmp (x y)
  (typecase x
    (number (typecase y
              (number (< x y))
              (symbol (if (equalp y 'not-const) ;unknown)
                          t
                          nil))))
    (symbol (if (equalp x 'not-const) ;'unknown)
                nil
                t)))
  )

(defun lcc-const-join-fn (x y)
  "Compute the join operation on the constant propagation lattice."
  (rle-map-reduce (lambda (map key val)
                (let ((new-val (aif (rle-map-find key map t)
                                    (typecase val
                                      (number (typecase (cdr it)
                                                (number (if (= val (cdr it))
                                                            val
                                                            'not-const))
                                                (symbol (cond
                                                          ((equalp (cdr it) 'not-const)
                                                           'not-const)
                                                          ;;((equalp (cdr it) 'unknown) val)
                                                          (t (error "Bad value for (cdr it)"))))
                                                (t (error "Bad value for it - not symbol or number"))))
                                      (symbol (if (equalp val 'not-const)
                                                  'not-const
                                                  (cdr it))))
                                    'not-const))) ;; not in both maps, so the value is unknown
                  (rle-map-insert key new-val map))) ;; computing a join on two value maps
              x
              y)
  )

(defparameter empty-s (rle-map-empty :comp #'constcmp))

(defun const-dataflow-funs (ops)
  "Compute the locations that store constants in this stream of
opcodes.  This returns a map that is keyed by the index of the
operation, with the set of memory locations that store constant values
as its value."
  (let ((funs (lcc-dataflow:get-function-body ops)))
    (setmap:map-map (lambda (k v)
                      (declare (ignore k))
                      (multiple-value-bind (in-set in-stack valmap) 
                          (lcc-dataflow:flow-forwards #'lcc-const-join-fn
                                                      #'lcc-const:lcc-const-flow-fn
                                                      (lcc-dataflow:make-cfg-single-ops v) ; cfg
                                                      (setmap-rle:rle-map-empty :comp #'<) ; in-sets
                                                      (setmap-rle:rle-map-empty :comp #'<) ; in-stacks
                                                      (setmap-rle:rle-map-empty :comp #'<) ; valmaps
                                        ;(setmap:set-from-list (loop for i from 0 to 8 collect 
                                        ;                           (cons (* 4 i) 'unknown)) :comp #'constcmp)
                                                      #|
                                                      (reduce (lambda (x y)
                                                                (setmap:map-insert (car y) (cdr y) x)
                                                                )
                                                              (loop for i from 0 to 128 collect (cons i 'unknown))
                                                              :initial-value empty-s)) |#
                                                      empty-s) ;; empty-set (may not be needed soon)
                        (declare (ignore valmap))
                        (list in-set in-stack)))
                    funs))
  )


;; a note: lsize is currently unused and can be removed during cleaning
(defgeneric gen (op stack valmap lsize)
  (:documentation "Get the gen set for this op")
  )

(defgeneric kill (op stack valmap lsize)
  (:documentation "Get the kill set for this op")
  )

(defun lcc-const-flow-fn (in-set in-stack valmap bb &optional (lsize *byte-width*))
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (let ((genkill (get-gen-kill bb in-stack in-set #|valmap|# lsize)))
    (list (third genkill) ;; stack
          (rle-set-union (first genkill)
                     (rle-set-diff in-set
                               (second genkill))) ;; (gen U (in - kill))
          (fourth genkill))) ;; valmap
  )

(defun get-gen-kill (bb instack valmap lsize)
  "Get the gen and kill sets for a basic block"
  (declare (type basic-block bb)
           (optimize (debug 3) (speed 0)))
  (let ((gen ;(set-from-list (mapcan #'gen (basic-block-ops bb))))
         (reduce #'(lambda (&optional x y)
                     (rle-set-union (aif x
                                     x
                                     empty-s)
                                ;(map-empty :comp constcmp))
                                (alist->rle-map* y :empty-m empty-s)))
                 (car (reduce #'(lambda (st x)
                                  (let ((valmap (third st))
                                        (stack (second st))
                                        (lsts (first st)))
                                    (let ((val (gen x stack valmap lsize)))
                                      (cons (cons (car val) lsts)
                                            (cdr val)))))
                              (basic-block-ops bb)
                              :initial-value (list nil instack valmap)))
                 :initial-value empty-s));(map-empty :comp constcmp)))
        (kill ;(set-from-list (mapcan #'kill (basic-block-ops bb)))
         (reduce #'(lambda (&optional x y)
                     (rle-set-union (aif x
                                     x
                                     empty-s);(map-empty :comp constcmp))
                                (alist->rle-map* y :empty-m empty-s)));:comp #'constcmp)))
                 (car (reduce #'(lambda (st x)
                                  (let ((stack (second st))
                                        (valmap (third st))
                                        (lsts (first st)))
                                    (let ((val (kill x stack valmap lsize)) )
                                      (cons (cons (car val) lsts)
                                            (cdr val)))))
                              (basic-block-ops bb) 
                              :initial-value (list nil instack valmap)))
                 :initial-value empty-s));(map-empty :comp constcmp)))
        (stck (second (reduce #'(lambda (st x)
                               (let ((stack (second st))
                                     (lsts (first st))
                                     (valmap (third st)))
                                 (let ((val (kill x stack valmap lsize)))
                                   (cons (cons (car val) lsts)
                                         (cdr val)))))
                              (basic-block-ops bb)
                              :initial-value (list nil instack valmap))))
        (valmap (third (reduce #'(lambda (st x)
                                   (let ((stack (second st))
                                         (lsts (first st))
                                         (valmap (third st)))
                                     (let ((val (kill x stack valmap lsize)))
                                       (cons (cons (car val) lsts)
                                             (cdr val)))))
                               (basic-block-ops bb)
                               :initial-value (list nil instack valmap)))))
    (list gen kill stck valmap)))

(defmacro def-gen-kill (type &key stck vals gen kill)
  `(progn
     (defmethod gen ((op ,type) stack valmap lsize)
       (the (cons list (cons list (cons rle-avl-set t))) (list ,gen ,stck 
                                                           (aif ,vals
                                                                it
                                                                valmap))))
     
     (defmethod kill ((op ,type) stack valmap lsize)
       (the (cons list (cons list (cons rle-avl-set t))) (list ,kill ,stck 
                                                           (aif ,vals
                                                                it
                                                                valmap)))))
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
    :stck (cons 'not-const (cdr stack)) ;; pop off location of the call, make room for the return value
    :gen nil
    :kill nil
    )

(def-gen-kill callv
    :stck (cdr stack) ;; pop off the location of the call
    :gen nil
    :kill nil
    )

;; note: ret is still unimplemented

(def-gen-kill jumpv
    :stck (cdr stack) ;; pop off location of the jump
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

(def-gen-kill cnsti
    :stck (the (cons integer t)
            (cons
             (parse-integer (second (slot-value op 's-args))) stack))
    )

(def-gen-kill two-arg-instruction
    :stck (cons 'not-const (cddr stack))
    )

(def-gen-kill cmp-jump-instruction
    :stck (let ((o1 (first stack))
                (o2 (second stack)))
            (if (or (eql 'not-const o1)
                    (eql 'not-const o2))
                (cons 'not-const (cddr stack))
                (cons 'const (cddr stack))))
    )

(def-gen-kill argu
    :stck (cdr stack)
    )

(def-gen-kill argp
    :stck (cdr stack)
    )

(def-gen-kill addp
    :stck (let ((op1 (first stack))
                (op2 (second stack)))
            (cons
             (cond
               ((or (eql op1 'glob)
                    (eql op2 'glob)) 
                'glob)
               ((or (eql op1 'args)
                    (eql op2 'args))
                'args)
               ((or (eql op1 'not-const)
                    (eql op2 'not-const))
                'not-const)
               (t (typecase op1
                    (integer (typecase op2
                               (integer (+ op1 op2))
                               (t 'glob)))
                    (t 'glob))))
             (cddr stack)))
    )

(def-gen-kill one-arg-instruction
    :stck (let ((op (first stack)))
            (if (eql 'not-const op)
                (cons 'not-const (cdr stack))
                (cons 'const (cdr stack))))
    )

(def-gen-kill asgnu
    :stck (cddr stack)
    :vals (cond
            ((or (eql (second stack) 'glob)
                 (eql (second stack) 'args)
                 (null (second stack)))
             valmap)
            (t (let ((addr (second stack)))
                 ;(declare (type integer addr))
                 (typecase addr
                     (integer (rle-map-insert addr (first stack) valmap))
                     (t valmap)))))
    :gen (cond
           ((eql (first stack) 'not-const) (list (cons (second stack) 'not-const)))
           ((eql (second stack) 'glob) nil)
           ((eql (second stack) 'args) nil)
           ;((eql (second stack) 'not-const) (error "Bad address -- address is indeterminate and not global?"))
           ((null (second stack)) nil)
           (t (list (cons (the integer (second stack)) (first stack))))
           )
    :kill (cond
           ((eql (second stack) 'glob) nil)
           ((eql (second stack) 'args) nil)
           ;((eql (second stack) 'const)
           ; (loop for i from 0 to lsize collect (* 4 i)))
           ((null (second stack)) nil)
           (t (list (cons (the integer (second stack)) (first stack)))))
    )

(def-gen-kill asgni
    :stck (cddr stack)
    :vals (cond
            ((or (eql (second stack) 'glob)
                 (eql (second stack) 'args)
                 (null (second stack)))
             valmap)
            (t (let ((addr (second stack)))
                 ;(declare (type integer addr))
                 (typecase addr
                     (integer (rle-map-insert addr (first stack) valmap))
                     (t valmap)))))
    :gen (cond
           ((eql (first stack) 'not-const) (list (cons (second stack) 'not-const)))
           ((eql (second stack) 'glob) nil)
           ((eql (second stack) 'args) nil)
           ;((eql (second stack) 'not-const) (error "Bad address -- address is indeterminate and not global?"))
           ((null (second stack)) nil)
           (t (list (cons (the integer (second stack)) (first stack)))))
    :kill (cond
           ((eql (second stack) 'glob) nil)
           ((eql (second stack) 'args) nil)
           ;((eql (second stack) 'const)
           ; (loop for i from 0 to lsize collect (* 4 i)))
           ((null (second stack)) nil)
           (t (list (cons (the integer (second stack)) (first stack)))))
    )

;; (def-gen-kill asgni
;;     :stck (cddr stack)
;;     :vals (cond
;;             ((or (eql (second stack) 'glob)
;;                  (eql (second stack) 'args)
;;                  (null (second stack)))
;;              valmap)
;;             (t (let ((addr (second stack)))
;;                                         ;(declare (type integer addr))
;;                  (typecase addr
;;                    (integer (map-insert addr (the integer (first stack)) valmap))
;;                    (t valmap)
;;                    )
;;                  )
;;                )
;;             )
;;     :gen (cond
;;            ((eql (first stack) 'glob) nil)
;;            ((eql (first stack) 'args) nil)
;;            ((eql (second stack) 'not-const) nil)
;;            (t (list (the integer (first stack))))
;;            )
;;     :kill (cond
;;            ((eql (first stack) 'glob) nil)
;;            ((eql (first stack) 'args) nil)
;;            ((eql (second stack) 'const)
;;             (loop for i from 0 to lsize collect (* 4 i)))
;;            (t (list (the integer (first stack))))
;;            )
;;     )

(def-gen-kill indiru
    :stck (cons (cond
                  ((or (eql (car stack) 'glob)
                       (eql (car stack) 'const))
                   'glob)
                  ((or (eql (car stack) 'args))
                   'args)
                  (t (cdr (rle-map-find (car stack) valmap))))
                (cdr stack))
    )

(def-gen-kill indirp
    :stck (cons 'glob (cdr stack)))

(def-gen-kill indiri
    :stck (cons (cond
                  ((or (eql (car stack) 'glob)
                       (eql (car stack) 'const))
                   'glob)
                  ((or (eql (car stack) 'args))
                   'args)
                  (t (cdr (rle-map-find (car stack) valmap))))
                (cdr stack))
    ;;    :stck (cons (cdr (map-find (car stack) valmap)) (cdr stack)))
    )

(defmacro arithmetic-shift (fn op1 op2)
  `(cons (typecase ,op1
           (integer (typecase ,op2
                      (integer (funcall ,fn ,op1 ,op2))
                      (t 'not-const)))
           (t 'not-const))
         (cddr stack)))

(defmacro arithmetic-stack (fn op1 op2)
  `(cons (typecase ,op1
           (number (typecase ,op2
                     (number (funcall ,fn ,op1 ,op2))
                     (t 'not-const)))
           (t 'not-const))
         (cddr stack)))

(def-gen-kill lshu
    :stck (let ((op1 (first stack))
                (op2 (second stack)))
            (arithmetic-shift #'ash op2 op1)
            ))

(def-gen-kill lshi
    :stck (let ((op1 (first stack))
                (op2 (second stack)))
            (arithmetic-shift #'ash op2 op1)    
            ))

(def-gen-kill rshu
    :stck (let ((op1 (first stack))
                (op2 (second stack)))
           (arithmetic-shift #'ash op2 (aif (numberp op1) (* -1 op1) 'not-const))
           ))

(def-gen-kill rshi
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
            (arithmetic-shift #'ash op2 (aif (numberp op1) (* -1 op1) 'not-const))
            ))

(def-gen-kill addu
    :stck (let ((op1 (first stack))
                (op2 (second stack)))
           (arithmetic-stack #'+ op1 op2)
           ))

(def-gen-kill addi
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'+ op1 op2)
           ))

(def-gen-kill subu
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'- op2 op1)
           ))

(def-gen-kill subi
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'- op2 op1)
    ))

(def-gen-kill mulu
    :stck (let ((op1 (first stack))
                (op2 (second stack)))
            (arithmetic-stack #'* op2 op1)   
  ))

(def-gen-kill muli
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
            (arithmetic-stack #'* op2 op1)
            ))

(def-gen-kill divu
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
            (arithmetic-stack #'/ op2 op1) 
            ))

(def-gen-kill divi
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'/ op2 op1)
    ))

(def-gen-kill modu
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'mod op2 op1)
           ))

(def-gen-kill modi
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'mod op2 op1)
    ))

(def-gen-kill boru
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'logior op2 op1)
    ))

(def-gen-kill bori
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'logior op2 op1)
))

(def-gen-kill bandu
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'logand op2 op1)
))

(def-gen-kill bandi
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'logand op2 op1)
  ))

(def-gen-kill bxoru
    :stck (let ((op1 (first stack))
               (op2 (second stack)))
           (arithmetic-stack #'logxor op2 op1)
 ))

(def-gen-kill bxori
    :stck (let ((op1 (first stack))
                (op2 (second stack)))
            (arithmetic-stack #'logxor op2 op1)
    ))

(def-gen-kill bcomu
    :stck (let ((o1 (first stack)))
            (cons
             (typecase o1
               (number (lognot o1))
               (symbol 'not-const)
               (t (error "unknown item on stack")))
             (cddr stack)))
    )

(def-gen-kill bcomi
    :stck (let ((o1 (first stack)))
            (cons
             (typecase o1
               (integer (lognot o1))
               (symbol 'not-const)
               (t (error "unknown item on stack")))
             (cddr stack)))
    )

(def-gen-kill negi
    :stck (let ((o1 (first stack)))
            (cons
             (typecase o1
               (number (* -1 o1))
               (symbol 'not-const))
             (cddr stack)))
    )
