;;; this iterates through a control-flow graph to perform faint-variable analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-faintgate
  (:use :common-lisp :pcf2-bc :setmap :setmap-rle :utils :pcf2-block-graph :pcf2-flow-utils)
  (:export faint-flow-fn
           faint-confluence-op
           faint-weaker-fn)
  )

(in-package :pcf2-faintgate)

;; this analysis tracks the uses of wires through a program to determine which are actually useful to the output and which are merely dead weight
;; we do this by tracing wire values backwards from their outputs with output_alice and output_bob to their initialization. wires that do not contribute to the output may be discarded
;;; definition: A variable x e Var is _faint_ at a program point *u* if along every path from u to End, it is neither not used before being defined or is used to define a faint variable (this is the complement of liveness)

;;; f_n(x) = (x-Kill_n(x) Union Gen_n

;;; Faint variable analysis is a backwards data flow problem
;;; since it is an all-paths analysis, the confluence operator is Intersection
;;; and since the most aggressive optimization is to declare all variables faint, the "top" value is Var (or all variables)

;;; Gen_n(x) = ConstGen_n Union DepGen_n(x)
;;; Kill_n(x) = ConstKill_n Union DepKill_n(x)

;;; in general, Gen and Kill for faint variable analysis are:
;;;
;;; In_n = F_n(Out_n)
;;; Out_n = { BI   n is end
;;;           Meet (s in succ(n) In_s)  Otherwise
;;; (remember, In_s is passed to the next block, Out_n is an input to this block)

;;; ConstGen_n = { {x} n is assignment x=e, x /e Opd(e) 
;;;                {x} n is read(x) ;; read is always alice() or bob()
;;;                /0  otw
;;; DepGen_n(x) = /0
;;;
;;; explanation: variables become faint before every assignment to them (because this is a backwards flow, this is like saying that a variable becomes faint when it will be redefined before its next use)

;;; ConstKill_n    = { {x}  n is use(x) ;; here, out use(x) comes in output_alice and output_bob, or 
;;;                    /0   otw
;;; DepKill_n(x)   = { Opd(e) Intersect Var   n is assignment x=e, x /e *x*
;;;                     /0                otw
;;; explanation: x is not faint if it is used towards the output of the program
;;; DepKill states that if a variable is not faint after a definition, then all of the variables used to define it are not faint before the definition. This achieves the transitive property we're looking for.


;;; it is less efficient to track all of the faint gates than it is to track all of the live gates, in reverse, as they move through the program. This alters the calculation of flow function
;;; in standard faint variable analysis, T = Var, Bottom = {}, and conf = Union
;;; but this tracks all of the _faint_ gates, while it is more efficient for us to track Live Gates
;;; so instead, **WE SWITCH THE DEFINITIONS OF GEN AND KILL** (their const and dep versions follow respectively)
;;; now, Top is {}, bottom is {Var}, and the confluence operator is set-union

;; A note: because wire 0 is the global condition wire, it should always be live. we use the "ret" instruction to designate it as live because we know it will always be the last instruction (and therefore the first to be analyzed for faintness)

(defparameter faint-confluence-operator #'rle-set-union)
;; "top" is Var and is represented by *lattice-top* from pcf2-dataflow

(defparameter output-functions (set-from-list (list "output_alice" "output_bob") :comp #'string<))
(defparameter input-functions (set-from-list (list "alice" "bob") :comp #'string<))


(defun get-out-sets (blck cfg conf)
  (reduce
   (lambda (temp-out succ)
     (let ((succ-out (get-block-faints (get-block-by-id succ cfg))))
       (funcall conf succ-out temp-out)))
   (get-block-succs blck)
   :initial-value (rle-empty-set)
   ;;(get-block-faints blck)
   ))

(defun faint-confluence-op (set1 set2)
  ;; if either set is "top," return the other set
  ;;(declare (optimize (debug 3)(speed 0)))
  (funcall faint-confluence-operator set1 set2))

(defun faint-weaker-fn (set1 set2)
  ;;(declare (optimize (debug 3)(speed 0)))
  ;; set1 is weaker than (safely estimates) set2 if set1 is a superset of set2 
  (and (not (rle-set-subset-efficient set1 set2))
       (rle-set-subset-efficient set2 set1)
  ))

(defun faint-flow-fn (blck cfg use-map)
  ;;(declare (optimize (speed 0) (debug 3)))
  (let ((in-flow (get-out-sets blck cfg #'faint-confluence-op))) 
    (let ((flow (faint-confluence-op
                 ;; set-union should have the smaller set come second
                 (rle-set-diff-efficient in-flow (kill blck in-flow))
                 (gen blck in-flow))))
      #|(typecase (get-block-op blck)
        (bits (break))
        (copy (if (< (get-block-id blck) 40)
                  (break)))
        (copy-indir (if (< (get-block-id blck) 40)
                        (break)))
        (indir-copy (if (< (get-block-id blck) 40)
                        (break)))
        (otherwise t))|#
      (if (zerop (mod (get-block-id blck) 100))
          (eliminate-extra-faints flow blck use-map)
          flow))))
      ;;(break)
      ;;flow)))                           

(defgeneric gen (blck flow-data)
  (:documentation "this function describes how to compute the gen part of the flow function for each op") 
  )

(defgeneric kill (blck flow-data)
  (:documentation "this function describes how to compute the kill part of the flow function for each op")
)

(defgeneric const-gen (op base)
  (:documentation "this function describes how to compute the constant gen part of the flow function for each op")
)

(defgeneric dep-gen (op base blck flow-data)
  (:documentation "this function describes how to compute the dependent gen part of the flow function for each op")
)

(defgeneric const-kill (op base)
  (:documentation "this function describes how to compute the constant kill part of the flow function for each op")
)

(defgeneric dep-kill (op base blck flow-data)
  (:documentation "this function describes how to compute the dependent kill part of the flow function for each op")
)

(defmethod gen (blck flow-data)
  ;; gen = const_gen union dep_gen
  (reduce (lambda (state op)
            (rle-set-union state
                       (rle-set-union (const-gen op (get-block-base blck)) (dep-gen op (get-block-base blck) blck flow-data))))
          (get-block-op-list blck)
          :initial-value (rle-empty-set)))
;;  (let ((op (get-block-op blck)))
  ;;  (faint-confluence-op (const-gen op (get-block-base blck)) (dep-gen op (get-block-base blck) blck flow-data)))
 ;; )

(defmethod kill (blck flow-data)
  ;;(declare (ignore flow-data))
  ;; kill = const-kill union dep_kill
 (reduce (lambda (state op)
           (rle-set-union state
                      (rle-set-union (const-kill op (get-block-base blck))(dep-kill op (get-block-base blck) blck flow-data))))
         (get-block-op-list blck)
         :initial-value (rle-empty-set)))

(defmacro gen-kill-standard ()
  ;; for faint variable analysis, standard is always empty set
  `(rle-empty-set))

;;; macros to define const-gen, dep-gen, const-kill, and dep-kill

(defmacro def-const-gen (type &body body)
  `(defmethod const-gen ((op ,type) base)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

(defmacro def-dep-gen (type &body body)
  `(defmethod dep-gen ((op ,type) base blck flow-data)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

(defmacro def-const-kill (type &body body)
  `(defmethod const-kill ((op ,type) base)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

(defmacro def-dep-kill (type &body body)
  `(defmethod dep-kill ((op ,type) base blck flow-data)
     (declare (optimize (debug 3) (speed 0))
              (ignore flow-data))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

;; and the macro to write const-gen, dep-gen, const-kill, and dep-kill for each instruction
(defmacro def-gen-kill (type &key (const-gen nil) (dep-gen nil) (const-kill nil) (dep-kill nil))
  `(progn
     (def-const-gen ,type ,const-gen)
     (def-dep-gen ,type ,dep-gen) ; dep-gen always /0 in faint analysis
     (def-const-kill ,type ,const-kill)
     (def-dep-kill ,type ,dep-kill)
  ))


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

(def-gen-kill join
    ;; if the output wire is live, then all of the inputs should be
    :dep-gen (with-slots (dest op1) op
               (with-true-address dest
                 (with-true-address-list op1
                   (if (rle-set-member dest flow-data)
                       (rle-set-from-list op1)
                       (rle-empty-set)))))

    ;; kill at the bits in op1
    :const-kill (with-slots (op1) op
                  (with-true-address-list op1
                    (rle-set-from-list op1)))
    )
    
(def-gen-kill bits
    ;; if any of the output wires is live, then the input wire should be.
    :dep-gen (with-slots (dest op1) op
               (with-true-address-list dest
                 (with-true-address op1
                   (let ((destwires (rle-set-from-list dest)))
                     (if (rle-set-equalp (rle-empty-set) (rle-set-inter destwires flow-data))
                         (rle-empty-set)
                         (rle-singleton op1))))))
    ;; if the op is a member of the dests, then don't kill it
    :const-kill (with-slots (dest op1) op
                  (with-true-address-list dest
                    (rle-set-from-list dest))))

(def-gen-kill const
    ;; if x = const, kill x because it has been defined
    :const-kill (with-slots (dest) op
                  (with-true-addresses (dest)
                    (rle-singleton dest))))

(def-gen-kill gate
    :dep-gen (with-slots (op1 op2 dest) op
               (with-true-addresses (op1 op2 dest)
                 ;;(break)
                 (if (rle-set-member dest flow-data)
                     ;;(aif (rle-map-val dest (get-block-consts blck)) ;; if the destination has a constant value, 
                         ;; (if (equalp it 'pcf2-block-graph:pcf-not-const)
                           ;;   (rle-set-from-list (list op1 op2))
                             ;; (rle-empty-set))
                     ;; the above segment will remove MUXes that probably should be removed but also gates on conditional wires that should not. that logic should really be somewhere else, not here.
                          
                     (rle-set-from-list (list op1 op2))
                 ;;(error "input to gate does not have a value"))
                     (rle-empty-set))))
    :const-kill (with-slots (op1 op2 dest) op
                  (with-true-addresses (op1 op2 dest)
                    (if (or (equalp op1 dest) (equalp op2 dest))
                        (rle-empty-set)
                        (rle-singleton dest)))))

(def-gen-kill add
    :dep-gen (with-slots (op1 op2 dest) op
               (with-true-addresses (op1 op2 dest)
                 (if (rle-set-member dest flow-data)
                     (rle-set-from-list (list op1 op2))
                     (rle-empty-set))))
    :const-kill (with-slots (op1 op2 dest) op
                  (with-true-addresses (op1 op2 dest)
                    (if (or (equalp op1 dest) (equalp op2 dest))
                        (rle-empty-set)
                        (rle-singleton dest)))))

(def-gen-kill sub
    :dep-gen (with-slots (op1 op2 dest) op
               (with-true-addresses (op1 op2 dest)
                 (if (rle-set-member dest flow-data)
                     (rle-set-from-list (list op1 op2))
                     (rle-empty-set))))
    :const-kill (with-slots (op1 op2 dest) op
                  (with-true-addresses (op1 op2 dest)
                    (if (or (equalp op1 dest) (equalp op2 dest))
                        (rle-empty-set)
                        (rle-singleton dest)))))
(def-gen-kill mul
    :dep-gen (with-slots (op1 op2 dest) op
               (with-true-addresses (op1 op2 dest)
                 (if (rle-set-member dest flow-data)
                     (rle-set-from-list (list op1 op2))
                     (rle-empty-set))))
    :const-kill (with-slots (op1 op2 dest) op
                  (with-true-addresses (op1 op2 dest)
                    (if (or (equalp op1 dest) (equalp op2 dest))
                        (rle-empty-set)
                        (rle-singleton dest)))))

(def-gen-kill copy
    ;; if dest is live (not faint), then whatever it copies will also be useful
    :dep-gen (with-slots (op1 op2 dest) op
               (with-true-addresses (op1 dest)
                 (if (equalp op2 1)
                     (if (rle-set-member dest flow-data)
                         (rle-singleton op1)
                         (rle-empty-set))
                     (first (reduce (lambda (state oldwire)
                               (let ((set (first state))
                                     (new-wire (car (second state)))
                                     (restwires (cdr (second state))))
                                 (if (rle-set-member new-wire flow-data)
                                     (list (rle-set-insert set oldwire) restwires)
                                     (list set restwires))))
                             (loop for i from op1 to (+ op1 op2 -1) collect i) ;; old wires
                             :initial-value (list (rle-empty-set) (loop for i from dest to (+ dest op2 -1) collect i)))))))
    
    :const-kill (with-slots (op1 op2 dest) op
                  (with-true-addresses (op1 dest)
                    (if (and (> dest op1) (< dest (+ op1 op2)))
                        (error "trying to copy onto self")
                        (if (equalp 1 op2)
                            (rle-singleton dest)
                            (rle-set-from-list
                             (loop for i from dest to (+ dest op2 -1) collect i)))))))

;; the following instructions need to know more about the previous ones
;; it is unlikely that the indirection instructions will ever really alter the flow of a program, since we seldom perform operations directly on them; however, where global state is important to the program we must keep track

(def-gen-kill mkptr
;; has no effect; the loaded constant will take care of this for us.
)

(defmacro gen-for-indirection (source destination length)
  `(if (eq ,length 1)
       (if (rle-set-member ,destination flow-data)
            (rle-singleton ,source)
            (rle-empty-set))
       (rle-set-from-list (loop for i from 0 to (- ,length 1)
                             when (rle-set-member (+ i ,destination) flow-data)
                             collect (+ i ,source)))
       ))

(defmacro kill-for-indirection (destination length)
  `(if (eq ,length 1)
       (if (not (equal ,destination 0))
           (rle-singleton ,destination)
           (rle-empty-set))
       (rle-set-from-list 
        (loop for i from ,destination to (+ ,length ,destination -1) collect i))))

(def-gen-kill copy-indir
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1)
                 (let ((addr (rle-map-val op1 (get-block-consts blck))))
                   ;; addr should always be defined in consts
                   ;;(break)
                   (rle-set-union (gen-for-indirection addr dest op2)
                                  (rle-singleton op1))
                                  )))
    :const-kill (with-slots (dest op2) op
                  (with-true-address dest
                    (kill-for-indirection dest op2)))
    )

(def-gen-kill indir-copy
    :dep-gen (with-slots (dest op1 op2) op
               (with-true-addresses (dest op1)
                 (let ((addr (rle-map-val dest (get-block-consts blck))))
                   ;;(break)
                   ;; addr should always be defined in consts
                   ;;(gen-for-indirection op1 addr op2)
                  (rle-set-union (gen-for-indirection op1 addr op2)
                                 (rle-singleton dest)) ;; this info is important!
                                  )))
    :dep-kill (with-slots (dest op2) op
                (with-true-address dest
                  (let ((addr (rle-map-val dest (get-block-consts blck))))
                    ;; addr should always be defined in consts
                    ;;(break)
                    (kill-for-indirection addr op2))))
    )

(def-gen-kill call
    :const-gen (with-slots (newbase fname) op
                 (with-true-address newbase
                   ;;(break)
                   (if (or (set-member fname input-functions) 
                           (set-member fname output-functions))
                       (rle-set-from-list (loop for i from (- newbase 32) to (- newbase 1) collect i))
                       (rle-empty-set))))
    :dep-kill (with-slots (newbase fname) op
                (with-true-address newbase
                  
                  #|(if (set-member fname input-functions)
                      (rle-set-from-list (loop for i from (- newbase 32) to (- newbase 1) collect i))|#
                  (rle-empty-set)))
    )

(def-gen-kill ret
    ;; the last instruction will always be a ret, so we use this opportunity to set 0 as live -- even though it will be repeated however many times 
    :const-gen (rle-singleton 0)
)

(def-gen-kill branch
    ;; we can assume that all branch wires introduce their condition wire to the set of not-faints. this preserves control flow from the original program
    :const-gen (with-slots (cnd) op
                 (with-true-address cnd
                   (rle-singleton cnd)))
    )

(def-gen-kill label)
(def-gen-kill initbase)
(def-gen-kill clear)
