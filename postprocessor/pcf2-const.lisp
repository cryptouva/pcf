;;; this iterates through a control-flow graph to perform constant-propagation analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-const
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-dataflow)
  (:export const-flow-fn
           const-confluence-op
           const-weaker-fn)
  )

(in-package :pcf2-const)

;;; this analysis tracks the uses of constants through a program to determine if we can eliminate some gates by propagating constants and to help with other dataflow analyses
;;; A variable x e Var has a constant value c e Const at a program point u if for every path reaching u along which a definition of x reaches u, the value of x is c.

;;; f_n(x) = (x-Kill_n(x) Union Gen_n

;;; Constant propagation is forward data flow problem
;;; we represent the constants available at a program point as a map from variable to constant. if the variable is not a constant, we exclude it from the map (this is a memory consideration)
;;; the confluence operation is defined in terms of applying conf-hat on pairs of the same variable
;; ForAll x1,x2 e L, x1 conf x2 = { <z, dx conf-hat dy > | <z,dx> e x1, <z,dy> e x2, x e Var }
;; our analysis will merge maps of constant values by taking all of the key-value pairs that are common to both maps and all of the key-value pairs for which the keys are unique to one map. Pairs from two maps with the same key but different values will be discarded.
;; Our map-intersect operation uses the same idea, but takes only the key-value pairs that the maps have in common, discarding everything else.

;;; Gen_n(x) = ConstGen_n Union DepGen_n(x)
;;; Kill_n(x) = ConstKill_n Union DepKill_n(x)

;;; in general, Gen and Kill for constant propagation are:
;;;
;;; In_n = { BI                          n is Start
;;;          Meet (p in pred(n)) Out_p   Otw
;;; Out_n = f_n(In_n)
;;; (remember, Out_n is passed to the next block, In_n is an input to this block)

;;; ConstGen_n = { {<x,eval(e,Top)>}  n is assignment x=e, Opd(e) subset Const 
;;;                **{<x,bottom-hat>} n is read(x) ;; read is always alice() or bob()
;;;                /0  otw
;;; DepGen_n(x) = { <x,d>    n is assignment x=e, <x,d> e *x*
;;;                  \0      otw
;;;

;;; ConstKill_n    =  **/0
;;; DepKill_n(x)   = { {<x,d>} n is assignment x=e, <x,d> e *x*
;;;                    {<x,d>} n is read(x), <x,d> e *x*
;;;                     /0                otw
;;; explanation:

;;; val(e,x) = { c if e is c e Const
;;;              d if e is x e Car, <x,d> e *x*
;;;

;;; **because read(x) is a way for us to input non-consts, we move <x,bottom-hat> from ConstGen to ConstKill


(defun map-union-without-conflicts (map1 map2)
  (map-reduce (lambda (map-accum key val)
                (aif (map-find key map2 t)
                     (if (eq it val)
                         map-accum ;; already have the element
                         (map-remove key map-accum)) ;; element duplicates not equivalent
                     (map-insert key val map-accum))) ;; add element
              map1
              map2))

(defparameter confluence-operator #'map-union-without-conflicts) ;; this is not set-inter, needs to be updated with a form of map-inter

(defmacro top-set ()
  `(map-empty))

(defun map-intersect (map1 map2)
  (map-reduce (lambda (map-accum key val)
                (aif (map-find key map2 t)
                     (if (eq it val)
                         (map-insert key val map-accum) ;; values correspond
                         map-accum) ;; values do not correspond
                     map-accum ;; value not in both maps
                ))
              map1
              (map-empty)))

(defun map-diff (map1 map2)
  ;; map1 without the elements from map2
  (map-reduce (lambda (map key val)
                (declare (ignore val))
                (if (map-find key map t)
                    (map-remove key map)
                    map))
              map2 ;; remove elements from map2
              map1 ;; use map1 as initial
              ))

(defun const-confluence-op (set1 set2)
  ;; if either set is "top," return the other set
  (funcall confluence-operator set1 set2))

(defun const-flow-fn (blck cfg)
  ;;(declare (optimize (speed 0) (debug 3)))
  (let ((in-flow (get-out-sets blck cfg #'map-intersect)))
    (map-union-without-conflicts
     (map-diff in-flow (kill (get-block-op blck) in-flow))
     (gen (get-block-op blck) in-flow))))

(defun const-weaker-fn (set1 set2)
  ;; set 1 is weaker than (safely estimates) set 2 if set 1 is a subset of set2
  (set-subset set1 set2))

(defun get-out-sets (blck cfg conf)
  (print (get-block-preds blck))
  (reduce
   (lambda (temp-out pred)
     (let ((pred-out (get-block-consts (get-block-by-id pred cfg))))
       (format t "pred out: ~A~%" (print pred-out))
       (funcall conf temp-out pred-out)))
   (get-block-preds blck)
   :initial-value (get-block-consts blck)))

(defgeneric gen (op flow-data)
  (:documentation "this function describes how to compute the gen part of the flow function for each op") 
  )

(defgeneric kill (op flow-data)
  (:documentation "this function describes how to compute the kill part of the flow function for each op")
)

(defgeneric const-gen (op)
  (:documentation "this function describes how to compute the constant gen part of the flow function for each op")
)

(defgeneric dep-gen (op flow-data)
  (:documentation "this function describes how to compute the dependent gen part of the flow function for each op")
)

(defgeneric const-kill (op)
  (:documentation "this function describes how to compute the constant kill part of the flow function for each op")
)

(defgeneric dep-kill (op flow-data)
  (:documentation "this function describes how to compute the dependent kill part of the flow function for each op")
)

(defmethod gen (op flow-data)
  ;; gen = const_gen union dep_gen
  (const-confluence-op (const-gen op) (dep-gen op flow-data)))

(defmethod kill (op flow-data)
  ;; kill = const-kill union gep_kill
  ;;(break)
  (const-confluence-op (const-kill op) (dep-kill op flow-data)))
  
;;; macros to define const-gen, dep-gen, const-kill, and dep-kill
(defmacro empty-gen ()
  `(map-empty))

(defmacro empty-kill ()
  `(empty-set))

(defmacro def-const-gen (type &body body)
  `(defmethod const-gen ((op ,type))
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (empty-gen))))

(defmacro def-dep-gen (type &body body)
  `(defmethod dep-gen ((op ,type) flow-data)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (empty-gen))))

(defmacro def-const-kill (type &body body)
  `(defmethod const-kill ((op ,type))
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (empty-kill))))

(defmacro def-dep-kill (type &body body)
  `(defmethod dep-kill ((op ,type) flow-data)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (empty-kill))))

;; and the macro to write const-gen, dep-gen, const-kill, and dep-kill for each instruction
(defmacro def-gen-kill (type &key (const-gen nil) (dep-gen nil) (const-kill nil) (dep-kill nil))
  `(progn
     (def-const-gen ,type ,const-gen)
     (def-dep-gen ,type ,dep-gen) ; dep-gen always /0 in faint analysis
     (def-const-kill ,type ,const-kill)
     (def-dep-kill ,type ,dep-kill)
  ))

;; gen sets are represented as maps of variable -> value
;; kill sets are represented as sets of variables since their values aren't necessary for the kill

(defmacro with-not-nil-from (a b &body body)
  `(let ((it (if ,a ,a ,b)))
     ,@body ))

(defmacro loginot (a)
  `(if (eq ,a 1) 1 0))


(defmacro singleton-if-found ()
  `(if (map-find dest flow-data t)
       (singleton dest)
       (empty-kill)))

(defun to-32-bit-binary-list (num)
  (labels ((to-32-bit (n depth)
             (if (eq depth 0)
                 (list (mod n 2))
                 (append (list (mod n 2)) (to-32-bit (floor (/ n 2)) (- depth 1))))))
  (to-32-bit num 31)))


(def-gen-kill bits
    :dep-gen (with-slots (dest op1) op
               (aif (map-find op1 flow-data t)
                    (let ((bin-list (to-32-bit-binary-list it)))
                      (reduce (lambda (state bit)
                                (let ((map (first state))
                                      (wire (car (second state))))
                                  (list (map-insert wire bit map) (cdr (second state)))))
                              bin-list
                              :initial-value (list (map-empty) dest)))
                    (empty-gen)))
    :dep-kill (with-slots (dest) op
                (reduce (lambda (set wire)
                          (if (map-find wire flow-data t)
                              (set-insert set wire)
                              set))
                        dest
                        :initial-value (empty-set)))
    )

(def-gen-kill join
    :dep-gen (labels ((all-list-found (map lst)
                        (if (null lst)
                            t
                            (and (map-find (car lst) map t) (all-list-found map (cdr lst))))))
               (with-slots (dest op1) op
                 (if (all-list-found flow-data op1)
                     (let ((val (loop for i in op1
                                   for count from 0 to (- (length op1) 1)
                                   with x = (map-find i flow-data)
                                   summing (* x (expt 2 count)) into dec-var
                                   finally (return dec-var)
                                     )))
                       (map-singleton dest val))
                     (empty-gen))))
    :dep-kill (with-slots (dest) op
                (if (map-find dest flow-data t)
                     (singleton dest)
                     (empty-kill)))
    )

(def-gen-kill gate
    ;; this is where we propagate ANDs with 0, ORs with 1, and NOTs on a const
    :dep-gen (with-slots (dest op1 op2 truth-table) op
               (let ((o1 (map-find op1 flow-data t))
                     (o2 (map-find op2 flow-data t)))
                 (if (or o1 o2)
                     (cond 
                       ((and o1 o2) ;; if both are constant, we can precompute the gate
                        (let ((out-val (case truth-table
                                         (#*0001 (logand o1 o2))
                                         (#*1100 (lognot o1))
                                         (#*0111 (logior o1 o2))
                                         (#*0110 (logxor o1 o2))
                                         (#*1001 (lognot (logxor o1 o2)))
                                         (otherwise 'undef))))
                          (if (equalp out-val 'undef)
                              (empty-gen)
                              (map-singleton dest out-val))))
                       (t (with-not-nil-from o1 o2
                            (case truth-table
                              (#*0001 (if (zerop it)
                                          (map-singleton dest 0)
                                          (map-empty)))
                              (#*0111 (if (zerop it)
                                          (map-empty)
                                          (map-singleton dest 1)))
                              (otherwise (map-empty))))))
                     (empty-gen))))
    :dep-kill (with-slots (dest) op
                (singleton-if-found))
)

(def-gen-kill const
    :const-gen (with-slots (dest op1) op
                 (map-singleton dest op1))
    :dep-kill (with-slots (dest) op
                (singleton-if-found))
    )

(def-gen-kill add
    :dep-gen (with-slots (dest op1 op2) op
                   (let ((o1 (map-find op1 flow-data))
                         (o2 (map-find op2 flow-data)))
                     (assert (and o1 o2)) ;; can only add on constants
                     (map-singleton dest (+ o1 o2))))
    :dep-kill (with-slots (dest) op
                (singleton-if-found))
)

(def-gen-kill sub
    :dep-gen (with-slots (dest op1 op2) op
                    (let ((o1 (map-find op1 flow-data))
                          (o2 (map-find op2 flow-data)))
                     (assert (and o1 o2)) ;; can only add on constants
                     (map-singleton dest (- o1 o2))))
    :dep-kill (with-slots (dest) op
                (singleton-if-found))
    )

(def-gen-kill mul
    :dep-gen (with-slots (dest op1 op2) op
                    (let ((o1 (map-find op1 flow-data))
                         (o2 (map-find op2 flow-data)))
                     (assert (and o1 o2)) ;; can only add on constants
                     (map-singleton dest (* o1 o2))))
    :dep-kill (with-slots (dest) op
                (singleton-if-found))
    )

(def-gen-kill copy
    :dep-gen (with-slots (dest op1 op2) op
                 (if (equal op2 1)
                     (let ((o1 (map-find op1 flow-data t)))
                       (if o1
                           (map-singleton dest o1)
                           (empty-gen)))
                     (reduce (lambda (map var)
                               (let ((data (map-find var flow-data t)))
                                 (if data
                                     (map-insert var data map)
                                     map)))
                             (loop for i from op1 to (+ op1 op2) collect i)
                             :initial-value (map-empty))))
    :dep-kill (with-slots (dest op1 op2) op
                (if (equal 1 op2)
                    (if (map-find dest flow-data t)
                        (singleton dest)
                        (empty-kill))
                    (reduce (lambda (set var)
                              (let ((data (map-find var flow-data t)))
                                (if data
                                    (set-insert set var)
                                    set)))
                            (loop for i from op1 to (+ op1 op2) collect i)
                            :initial-value (empty-set))))
)


(def-gen-kill mkptr) ;; no consts

(defmacro gen-for-indirection (source-address dest-address length)
  `(if (equal ,length 1)
       (aif (map-find ,dest-address flow-data nil)
            (map-singleton ,dest-address it)
            (empty-gen))
       (reduce (lambda (state oldwire)
                 (let ((map (first state))
                       (newwire (car (second state))))
                   (aif (map-find oldwire flow-data t)
                        (list (map-insert newwire it map) (cdr (second state)))
                        (list map (cdr (second state))))))
               (loop for i from ,source-address to (+ ,source-address ,length) collect i)
               :initial-value (list (empty-gen) (loop for i from ,dest-address to (+ ,dest-address ,length))))))

(defmacro kill-for-indirection (source-address dest-address length)
  `(if (equal ,length 1)
      (if (map-find ,dest-address flow-data nil)
          (singleton ,dest-address)
          (empty-kill))
      (reduce (lambda (state oldwire)
                (let ((set (first state))
                      (newwire (car (second state))))
                  (aif (map-find oldwire flow-data t)
                       (list (set-insert set newwire) (cdr (second state)))
                       (list set (cdr (second state))))))
              (loop for i from ,source-address to (+ ,source-address ,length) collect i)
              :initial-value (list (empty-kill) (loop for i from ,dest-address to (+ ,dest-address ,length))))))

(def-gen-kill copy-indir
    :dep-gen (with-slots (dest op1 op2) op
               (let ((addr (map-find op1 flow-data)))
                 (gen-for-indirection addr dest op2)))
    :dep-kill (with-slots (dest op1 op2) op
                (let ((addr (map-find op1 flow-data)))
                  (kill-for-indirection addr dest op2))))

(def-gen-kill indir-copy
    :dep-gen (with-slots (dest op1 op2) op
               (let ((addr (map-find dest flow-data)))
                 (gen-for-indirection op1 addr op2)))
    :dep-kill (with-slots (dest op1 op2) op
                (let ((addr (map-find dest flow-data)))
                  (kill-for-indirection op1 addr op2))))

(def-gen-kill initbase) ;; no consts
(def-gen-kill clear) ;; no consts
(def-gen-kill call) ;; no consts
(def-gen-kill ret) ;; no consts
(def-gen-kill branch) ;; no consts
(def-gen-kill label) ;; no consts
