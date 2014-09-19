;;; this iterates through a control-flow graph to perform faint-variable analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-live
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-block-graph)
  (:export live-flow-fn
           live-confluence-op
           live-weaker-fn)
  )

(in-package :pcf2-live)

;; this analysis tracks the wires that are live _in the moment_ in order to help us optimize other flow functions by not carrying around too much extra information. It is a "lite" version of faint variable analysis in the sense that this one doesn't care about dep-gen or dep-kill.
;; we do this by tracing wire values backwards from their outputs with output_alice and output_bob to their initialization. wires are discarded after (before) their definitions so we know that they're not being useful and don't need to be carried around.
;;; definition: A variable x e Var is _live_ at a program point *u* if some path from *u* to END contains a use of x that is not preceded by its definition

;;; f_n(x) = ((x-Kill) Union Gen)

;;; This live variable analysis is performed as a backwards data flow problem.
;;; the confluence operator is intersection
;;; and since the most aggressive optimization is to declare all variables non-live, the "top" value is {}

;;; explanation: 
;;; Kill is the set of variables being defined
;;; Gen is the set of variables being used
;;; there is no dep gen or dep kill

;;; note: since we do this analysis without use of consts (the biggest reason for this analysis is to eliminate the amount of consts we carry around), we cannot track all of the pointed-to locations using this analysis. We account for this by simply not eliminating those wires when we don't find them in the use-map (see pcf2-use-map).


(defparameter live-confluence-operator #'set-union)  ;;#'set-inter)
;; "top" is Var and is represented by *lattice-top* from pcf2-dataflow

(defparameter output-functions (set-from-list (list "output_alice" "output_bob") :comp #'string<))
(defparameter input-functions (set-from-list (list "alice" "bob") :comp #'string<))

(defmacro top-set ()
  `(empty-set))
  ;;`(set-insert (empty-set) *lattice-top*))

(defun get-out-sets (blck cfg conf)
  (reduce
   (lambda (temp-out succ)
     (let ((succ-out (get-block-lives (get-block-by-id succ cfg))))
       (funcall conf temp-out succ-out)))
   (get-block-succs blck)
   :initial-value (get-block-lives blck)))

(defun live-confluence-op (set1 set2)
  ;; if either set is "top," return the other set
  (declare (optimize (debug 3)(speed 0)))
  (funcall live-confluence-operator set1 set2))

(defun live-weaker-fn (set1 set2)
  ;; set1 is weaker than (safely estimates) set2 if set1 is a superset of set2 
  (and (not (set-subset set1 set2))
       (set-subset set2 set1)
  ))

(defun live-flow-fn (blck cfg)
  ;;(declare (optimize (speed 0) (debug 3)))
  (let* ((in-flow (get-out-sets blck cfg #'live-confluence-op))) 
    (live-confluence-op
     (set-diff in-flow (kill (get-block-op blck)))
     (gen (get-block-op blck)))))

(defgeneric gen (op)
  (:documentation "this function describes how to compute the gen part of the flow function for each op") 
  )

(defgeneric kill (op)
  (:documentation "this function describes how to compute the kill part of the flow function for each op")
)

(defmacro gen-kill-standard ()
  ;; for live variable analysis, standard is always empty set
  `(empty-set))

(defmacro def-gen (type &body body)
  `(defmethod gen ((op ,type))
     (declare (optimize (debug 3)(speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard))))

(defmacro def-kill (type &body body)
  `(defmethod kill ((op ,type))
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (gen-kill-standard)
          )))

;; and the macro to write const-gen, dep-gen, const-kill, and dep-kill for each instruction
(defmacro def-gen-kill (type &key (gen nil) (kill nil))
  `(progn
     (def-gen ,type ,gen)
     (def-kill ,type ,kill)))


(def-gen-kill join
    ;; the list of wires being joined are encountering a use
    :gen (with-slots (op1) op
           (set-from-list op1)
           )
    ;; the desintation wire is encourtering a definition
    :kill (with-slots (dest) op
            (singleton dest)))

(def-gen-kill bits
    ;; the source wire is encountering a use
    :gen (with-slots (op1) op
           (singleton op1))
    ;; the destination wires are being defined
    :kill (with-slots (dest) op
            (set-from-list dest)))

(def-gen-kill const
    ;; the destination wire is encountering a definition
    :kill (with-slots (dest) op
            (singleton dest)))

(def-gen-kill gate
    ;; the input wires are encountering a use
    :gen (with-slots (op1 op2) op
           (set-from-list (list op1 op2)))
    ;; the destination wire is being defined
    :kill (with-slots (dest) op
            (singleton dest)))

(def-gen-kill add
    ;; the input wires are being used
    :gen (with-slots (op1 op2) op
           (set-from-list (list op1 op2)))
    ;; the output wire is being defined
    :kill (with-slots (dest) op
            (singleton dest)))

(def-gen-kill sub
    ;; the input wires are being used
    :gen (with-slots (op1 op2) op
           (set-from-list (list op1 op2)))
    ;; the output wire is being defined
    :kill (with-slots (dest) op
            (singleton dest)))

(def-gen-kill mul
    ;; the input wires are being used
    :gen (with-slots (op1 op2) op
           (set-from-list (list op1 op2)))
    ;; the output wire is being defined
    :kill (with-slots (dest) op
            (singleton dest)))

(def-gen-kill copy
    :gen (with-slots (op1 op2) op
             (if (equal op2 1)
                 (singleton op1)
                 (set-from-list (loop for i from op1 to (+ op1 op2) collect i))))
    :kill (with-slots (dest op2) op
              (if (equal op2 1)
                  (singleton dest)
                  (set-from-list (loop for i from dest to (+ dest op2) collect i)))))

;; the following instructions need to know more about the previous ones
;; it is unlikely that the indirection instructions will really alter the flow of a program, since we seldom perform operations directly on them; however, where global state is important to the program we must keep track

(def-gen-kill mkptr
;; has no effect; the loaded constant will take care of this for us.
)

(def-gen-kill copy-indir
    ;; no gen here because we can't dereference.
    ;; definition of wires from dest to dest + op2
    :kill (with-slots (dest op2) op
            (set-from-list (loop for i from dest to (+ dest op2) collect i))))

(def-gen-kill indir-copy
    ;; use of wires from op1 to op1 + op2
    :gen (with-slots (op1 op2) op
           (set-from-list (loop for i from op1 to (+ op1 op2) collect i))) 
    ;; no kill here because we can't dereference
    )

(def-gen-kill call
    ;; use of wires from (- newbase 32) to (- newbase 1)
    :gen (with-slots (newbase fname) op
           (if (set-member fname output-functions)
               (set-from-list (loop for i from (- newbase 32) to (- newbase 1) collect i))
               (empty-set)))

    ;; definition of wires from (- newbase 32) to (- newbase 1)    
    :kill (with-slots (newbase fname) op
            (if (set-member fname input-functions)
                (set-from-list (loop for i from (- newbase 32) to (- newbase 1) collect i))
                (empty-set)))
    )


(def-gen-kill ret
    ;; the last instruction will always be a ret, so we use this opportunity to set 0 as live -- even though it will be repeated however many times 
    :gen (singleton 0)
)


(def-gen-kill branch
    :gen (with-slots (cnd) op
           (singleton cnd))
    )

(def-gen-kill initbase)
(def-gen-kill clear)
(def-gen-kill label)
