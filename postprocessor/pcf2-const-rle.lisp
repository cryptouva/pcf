;;; this iterates through a control-flow graph to perform constant-propagation analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-const
  (:use :common-lisp :pcf2-bc :setmap-rle :setmap :utils :pcf2-block-graph :pcf2-flow-utils)
  (:export const-flow-fn
           const-confluence-op
           const-weaker-fn
           )
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
;; Our hmap-intersect operation uses the same idea, but takes only the key-value pairs that the maps have in common, discarding everything else.

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

;; all unassigned wires are 0, so if something is not found in the consts then it is implicitly 0 (otherwise it will be an integer or pcf2-block-graph:pcf-not-const)

(defparameter input-functions (set-from-list (list "alice" "bob") :comp #'string<))

(defparameter confluence-operator #'rle-map-union-without-conflicts) ;; this is not set-inter, needs to be updated with a form of hmap-inter

;;(defparameter ostream (open "rle-const-out.txt" :direction :output :if-exists :supersede))

;;; macros to define const-gen, dep-gen, const-kill, and dep-kill
(defmacro empty-gen ()
  `(rle-map-empty))

(defmacro empty-kill ()
  `(rle-empty-set))

(defun rle-map-diff (map1 map2)
  ;; map1 without the elements from map2
  (rle-map-reduce (lambda (map key val)
                    (declare (ignore val))
                    (if (rle-map-val key map t)
                        (rle-map-remove key map)
                    map))
              map2 ;; remove elements from map2
              map1 ;; use map1 as initial
              ))

(defun rle-map-remove-key-set (map set)
  (rle-set-reduce (lambda (newmap key)
                    (if (rle-map-find key newmap t)
                        (rle-map-remove key newmap)
                        newmap))
                  set
                  map))

(defun const-confluence-op (set1 set2)
  (funcall confluence-operator set1 set2))

(defun const-flow-fn (blck cfg use-map)
  ;; this function contains a bit at the end to eliminate extraneous const information we may be carrying around
  (declare 
   (optimize (speed 0) (debug 3)))
  ;;(if (equal (get-block-id blck) 2844) (break))
  (let ((in-flow (get-out-sets blck cfg #'rle-map-union-without-conflicts)))
    (let ((flow (rle-map-union-without-conflicts
                 (rle-map-remove-key-set in-flow (kill blck in-flow))
                 (gen blck in-flow))))
      ;; (if (zerop (mod (get-block-id blck) 50))
      ;;     (eliminate-extra-consts flow blck use-map)          
      ;;     flow))))
      flow)))

#|      (typecase (get-block-op blck)
        (gate
         (let ((base (get-block-base blck)))
           (with-slots (op1 op2 dest) (get-block-op blck)
             (with-true-addresses (op1 op2 dest)
               (let ((o1 (rle-map-extract-val op1 in-flow))
                     (o2 (rle-map-extract-val op2 in-flow))
                     (o1* (rle-map-extract-val op1 flow))
                     (o2* (rle-map-extract-val op2 flow))
                     (d (rle-map-extract-val dest flow)))
                 (progn
                   (format ostream "~A ~%" (get-block-id blck))
                   (format ostream "~A ~%" (get-block-op blck))
                   (format ostream "~A ~A // ~A ~A // ~A ~%" o1 o2 o1* o2* d)
                   ;;(if (and (< (get-block-id blck) 300) (> (get-block-id blck) 293)) (break))
                   ))))))
        (otherwise t)) |#
;;flow)))
      
(defun const-weaker-fn (map1 map2)
  (rle-map-weaker-fn map1 map2))

(defun get-out-sets (blck cfg conf)
  ;;(format t "block preds: ~A~%" (get-block-preds blck))
  (reduce
   (lambda (temp-out pred)
     (let ((pred-out (get-block-consts (get-block-by-id pred cfg))))
       ;;(format t "pred out: ~A~%" pred-out)
       (funcall conf pred-out temp-out)))
   (get-block-preds blck)
   :initial-value (rle-map-empty)
   ))

(defgeneric gen (blck flow-data)
  (:documentation "this function describes how to compute the gen part of the flow function for each op") 
  )

(defgeneric kill (blck flow-data)
  (:documentation "this function describes how to compute the kill part of the flow function for each op")
)

(defgeneric const-gen (op blck)
  (:documentation "this function describes how to compute the constant gen part of the flow function for each op")
)

(defgeneric dep-gen (op blck flow-data)
  (:documentation "this function describes how to compute the dependent gen part of the flow function for each op")
)

(defgeneric const-kill (op blck)
  (:documentation "this function describes how to compute the constant kill part of the flow function for each op")
)

(defgeneric dep-kill (op blck flow-data)
  (:documentation "this function describes how to compute the dependent kill part of the flow function for each op")
)

(defmethod gen (blck flow-data)
  ;; gen = const_gen union dep_gen
  (reduce (lambda (state op)
            (const-confluence-op state
                                 (const-confluence-op (const-gen op (get-block-base blck)) (dep-gen op (get-block-base blck) flow-data))))
          (get-block-op-list blck)
          :initial-value (empty-gen)))

(defmethod kill (blck flow-data)
  ;; kill = const-kill union gep_kill
  ;;(break)
  (reduce (lambda (state op)
            (rle-set-union state
                       (rle-set-union (const-kill op (get-block-base blck)) (dep-kill op (get-block-base blck) flow-data))))
          (get-block-op-list blck)
          :initial-value (empty-kill)))

(defmacro def-const-gen (type &body body)
  `(defmethod const-gen ((op ,type) base)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (empty-gen))))

(defmacro def-dep-gen (type &body body)
  `(defmethod dep-gen ((op ,type) base flow-data)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (empty-gen))))

(defmacro def-const-kill (type &body body)
  `(defmethod const-kill ((op ,type) base)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (empty-kill))))

(defmacro def-dep-kill (type &body body)
  `(defmethod dep-kill ((op ,type) base flow-data)
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


(defmacro with-true-addresses ((&rest syms) &body body)
  `(let ,(loop for sym in syms
            collect `(,sym (+ ,sym (aif base it 0))))
     ,@body))

(defmacro with-true-address (sym &body body)
  `(let ((,sym (+ ,sym base)))
     ,@body))

(defmacro with-true-address-list (lst &body body)
  `(let ((,lst (mapcar (lambda(x) (+ x base)) ,lst)))
     ,@body))


(defmacro with-not-nil-from (a b &body body)
  `(let ((it (if ,a ,a ,b)))
     ,@body ))

(defmacro loginot (a)
  `(if (eq ,a 1) 1 0))

(defmacro singleton-if-found ()
  `(if (rle-map-val dest flow-data t)
       (rle-singleton dest)
       (empty-kill)))


(defun to-n-bit-binary-list (num bits)
  (labels ((to-binary (n depth)
             (if (eq depth 0)
                 (list (mod n 2))
                 (append (list (mod n 2)) (to-binary (floor (/ n 2)) (- depth 1))))))
  (to-binary num (- bits 1))))


(def-gen-kill bits
    :dep-gen (with-slots (dest op1) op
               (with-true-address-list dest
                 (with-true-addresses (op1)
                   (aif (rle-map-val op1 flow-data t)
                        (if (not (equalp it 'pcf2-block-graph:pcf-not-const))
                            (let ((bin-list (to-n-bit-binary-list it (length dest))))
                              ;;(break)
                              (first (reduce (lambda (state bit)
                                               ;;(break)
                                               (let ((map (first state))
                                                     (wire (car (second state)))) ;; this is the first from the list of bits
                                                 (list (rle-map-insert wire bit map) (cdr (second state))))) ;; cdr second state is the rest of the bits
                                             bin-list ;; reduce over the whole list
                                             :initial-value (list (rle-map-empty) dest))))
                            (error "bits called on non-const"))
                        (error "bits called on non-existing value"))
                   )))
    :dep-kill (with-slots (dest) op
                (with-true-address-list dest
                  (reduce (lambda (set wire)
                            (if (rle-map-val wire flow-data t)
                                (rle-set-insert set wire)
                                set))
                          dest
                          :initial-value (rle-empty-set))))
    )

(def-gen-kill join
    :dep-gen (labels ((all-list-found (map lst)
                        (if (null lst)
                            t
                            (and (not (null (rle-map-extract-val (car lst) map)))
                                 (all-list-found map (cdr lst))))))
               (with-slots (dest op1) op
                 (with-true-address dest
                   (with-true-address-list op1
                     (if (all-list-found flow-data op1)
                         (let ((val (first (reduce 
                                            (lambda (state wire)
                                              (list
                                               (+ (first state)
                                                  (* (rle-map-extract-val wire flow-data)
                                                     (expt 2 (second state))))
                                               (+ (second state) 1)))
                                            op1
                                            :initial-value (list 0 0)))))
                           (rle-map-singleton dest val))
                         (empty-gen))))))
    :dep-kill (with-slots (dest) op
                (with-true-address dest
                  (if (rle-map-val dest flow-data t)
                      (rle-singleton dest)
                      (empty-kill))))
    )

(defmacro or-defined (op1 op2 data)
  `(or (rle-map-extract-val ,op1 ,data)
       (rle-map-extract-val ,op2 ,data)))

(defmacro and-defined (op1 op2 data)
  `(and (rle-map-extract-val ,op1 ,data)
        (rle-map-extract-val ,op2 ,data)))

(defun flip-bit (o1)
  (if (zerop o1)
      1
      (if (equal o1 1)
          0
          (error "input to gate not 0 or 1"))))

(def-gen-kill gate
    ;; this is where we propagate ANDs with 0, ORs with 1, and NOTs on a const
    ;; we also precompute gate values where we know them beforehand
    :dep-gen (with-slots (dest op1 op2 truth-table) op
               (with-true-addresses (dest op1 op2)
                 (let ((o1 (rle-map-extract-val op1 flow-data))
                       (o2 (rle-map-extract-val op2 flow-data)))
                   ;;(break)
                   (if (or-defined op1 op2 flow-data)
                       (if (and-defined op1 op2 flow-data) ;; if both are constant, we can precompute the gate
                           (progn
                             (assert (or (equal o1 0)(equal o1 1)))
                             (assert (or (equal o2 0)(equal o2 1)))
                             (let ((out-val
                                    (cond
                                      ((equalp truth-table #*0001) (logand o1 o2))
                                      ((equalp truth-table #*1100) (flip-bit o1))
                                      ((equalp truth-table #*0111) (logior o1 o2))
                                      ((equalp truth-table #*0110) (assert (not (equalp op1 op2))) (logxor o1 o2))
                                      ((equalp truth-table #*1001) (flip-bit (logxor o1 o2)))
                                      (t 
                                       (print truth-table)
                                       (error "unknown truth table in gate")))))
                               (rle-map-singleton dest out-val)))
                           (cond
                             ((equalp truth-table #*0001)
                              (if (or (equal o1 0) (equal o2 0))
                                  (rle-map-singleton dest 0)
                                  (cond
                                    ((equal o1 1)
                                     (rle-map-singleton dest (aif o2 it 'pcf2-block-graph:pcf-not-const))) ;; whatever o2 is
                                    ((equal o2 1)
                                     (rle-map-singleton dest (aif o1 it 'pcf2-block-graph:pcf-not-const))) ;; whatever o1 is
                                    (t (rle-map-singleton dest 'pcf2-block-graph:pcf-not-const)))))
                             ((equalp truth-table #*0111)
                              (if (or (equal o1 1)(equal o2 1))
                                  (rle-map-singleton dest 1)
                                  (cond
                                    ((equal 0 o1)
                                     (rle-map-singleton dest (aif o2 it 'pcf2-block-graph:pcf-not-const)))
                                    ((equal 0 o2)
                                     (rle-map-singleton dest (aif o1 it 'pcf2-block-graph:pcf-not-const)))
                                    (t (rle-map-singleton dest 'pcf2-block-graph:pcf-not-const)))))
                             (t (rle-map-singleton dest 'pcf2-block-graph:pcf-not-const))))
                       (rle-map-singleton dest 'pcf2-block-graph:pcf-not-const)))))
    :dep-kill (with-slots (dest) op
                (with-true-address dest
                  (singleton-if-found)))
    )

(def-gen-kill const
    :const-gen (with-slots (dest op1) op
                 (with-true-addresses (dest)
                   ;;(break)
                   (rle-map-singleton dest op1)))
    :dep-kill (with-slots (dest) op
                  (with-true-addresses (dest)
                    (singleton-if-found)))
    )

(def-gen-kill add
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1 op2)
                 (let ((o1 (rle-map-extract-val op1 flow-data))
                       (o2 (rle-map-extract-val op2 flow-data)))
                   ;;(format t "o1: ~A o2: ~A~%" o1 o2) ;; can only add on constants
                   (rle-map-singleton dest (if (and o1 o2) (+ o1 o2) 'pcf2-block-graph:pcf-not-const)))))
    :dep-kill (with-slots (dest) op
                (with-true-address dest
                  (singleton-if-found)))
    )

(def-gen-kill sub
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1 op2)
                 (let ((o1 (rle-map-extract-val op1 flow-data))
                       (o2 (rle-map-extract-val op2 flow-data)))
                   ;;(format t "o1: ~A ot ~A~%" o1 o2) ;; can only add on constants
                   ;;(assert (and o1 o2))
                   (rle-map-singleton dest (if (and o1 o2) (- o1 o2) 'pcf2-block-graph:pcf-not-const)))))
    :dep-kill (with-slots (dest) op
                (with-true-address dest
                  (singleton-if-found)))
    )

(def-gen-kill mul
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1 op2)
                 (let ((o1 (rle-map-extract-val op1 flow-data))
                       (o2 (rle-map-extract-val op2 flow-data)))
                   ;;(format t "o1: ~A ot ~A~%" o1 o2) ;; can only add on constants
                   (rle-map-singleton dest (if (and o1 o2) (* o1 o2) 'pcf2-block-graph:pcf-not-const)))))
    :dep-kill (with-slots (dest) op
                (with-true-address dest
                  (singleton-if-found)))
    )

(def-gen-kill copy
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1)  
                 (if (equal op2 1)
                     (aif (rle-map-extract-val op1 flow-data)
                          (rle-map-singleton dest it)
                          (rle-map-singleton dest 'pcf2-block-graph:pcf-not-const))
                     (first (reduce (lambda (state oldwire)
                               (let ((map (first state))
                                     (lst (cdr (second state)))
                                     (newwire (car (second state))))
                                 (let ((data (rle-map-val oldwire flow-data t)))
                                   (if data
                                       (list (rle-map-insert newwire data map) lst)
                                       (list (rle-map-insert newwire 'pcf2-block-graph:pcf-not-const map) lst)))))
                               (loop for i from op1 to (+ op1 op2 -1) collect i)
                               :initial-value (list (rle-map-empty) (loop for i from dest to (+ dest op2 -1) collect i)))))))
    :dep-kill (with-slots (dest op2) op
                (with-true-addresses (dest)
                  (if (equal 1 op2)
                      (if (rle-map-val dest flow-data t)
                          (rle-singleton dest)
                          (empty-kill))
                      (reduce (lambda (set var)
                                (let ((data (rle-map-extract-val var flow-data)))
                                  (if data
                                      (rle-set-insert set var)
                                      set)))
                              (loop for i from dest to (+ dest op2 -1) collect i)
                              :initial-value (rle-empty-set)))))
    )


(def-gen-kill mkptr
    ;;:const-gen (progn (break)(empty-gen)) ;; no consts
)

(defmacro gen-for-indirection (source-address dest-address length)
  `(if (equal ,length 1)
       (aif (rle-map-extract-val ,source-address flow-data)  ;; it may not always be found; but usually in this case we're copying a condition wire, which usually won't be const (or faint) anyway
            (rle-map-singleton ,dest-address it) 
            (rle-map-singleton ,dest-address  'pcf2-block-graph:pcf-not-const))
       (first (reduce (lambda (state oldwire)
                        (let ((map (first state))
                              (newwire (car (second state))))
                          ;;(break)
                          (aif (rle-map-val oldwire flow-data t)
                               (list (rle-map-insert newwire it map) (cdr (second state)))
                               (list (rle-map-insert newwire 0 map) (cdr (second state))))))
                               ;;(list (rle-map-insert newwire 'pcf2-block-graph:pcf-not-const map) (cdr (second state))))))
                               ;;(error "could not find value of copy wire")))) 
                      (loop for i from ,source-address to (+ ,source-address ,length -1) collect i)
                      :initial-value (list (empty-gen) (loop for i from ,dest-address to (+ ,dest-address ,length -1) collect i))))))

(defmacro kill-for-indirection (dest-address length)
  `(if (equal ,length 1)
       (if (rle-map-find ,dest-address flow-data t) ;; it may not always be found; but usually in this case we're copying a condition wire, which usually won't be const (or faint) anyway
           (rle-singleton ,dest-address)
           (empty-kill))
        (reduce (lambda (set var)
                  (let ((data (rle-map-val var flow-data t)))
                    (if data
                        (rle-set-insert set var)
                        set)))
                (loop for i from ,dest-address to (+ ,dest-address ,length -1) collect i)
                :initial-value (rle-empty-set))))

(def-gen-kill copy-indir
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1)
                 (let ((addr (rle-map-val op1 flow-data)))
                   (gen-for-indirection addr dest op2))))
    :dep-kill (with-slots (dest op2) op
                (with-true-addresses (dest)
                  (kill-for-indirection dest op2))))

(def-gen-kill indir-copy
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1)
                 (let ((addr (rle-map-val dest flow-data)))
                   (gen-for-indirection op1 addr op2))))
    :dep-kill (with-slots (dest op2) op
                (with-true-addresses (dest)
                  (let ((addr (rle-map-val dest flow-data)))
                    ;;(break)
                    (kill-for-indirection addr op2)))))

(def-gen-kill initbase
    ;;take this opportunity to set wire 0 as pcf2-block-graph:pcf-not-const
    :const-gen (with-slots (base) op
                 (rle-map-insert base 0 ;; the 0th wire in the frame will always point at global condition wire
                             (rle-map-singleton 0 'pcf2-block-graph:pcf-not-const)))
    )

(def-gen-kill call
    :const-gen (with-slots (newbase fname) op
                 (with-true-address newbase
                   (if (set-member fname input-functions)
                       (reduce (lambda (map x)
                                 (rle-map-insert x 'pcf2-block-graph:pcf-not-const map))
                               (loop for i from newbase to (+ 32 newbase -1) collect i)
                               :initial-value (rle-map-empty))
                       (empty-gen))))
    :dep-kill (with-slots (newbase fname) op
                (with-true-address newbase
                  (rle-set-from-list
                   (loop for i from newbase to (+ 32 newbase -1) collect i))))
    )

(def-gen-kill branch
    :const-gen (with-slots (cnd) op
                 (with-true-address cnd
                   (rle-map-singleton cnd 'pcf2-block-graph:pcf-not-const))))

(def-gen-kill ret) ;; no consts
(def-gen-kill label) ;; no consts -- might have to set base
(def-gen-kill clear) ;; no consts -- yet. when interprocedural analysis is added, this will be important.
