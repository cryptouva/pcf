;;; this iterates through a control-flow graph to perform live variable analysis. it is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-use-map
  (:use :common-lisp :pcf2-bc :setmap :utils)
  (:export compute-used-wires)
  )

(in-package :pcf2-use-map)

;; this analysis tracks the first and last uses of wires in a program in order to make other data flow analyses more efficient.
;;; We create a map of this information and hand it to the other analyses so they don't have to track variables and keep copying them over (which slows down the analysis significantly). 

;;; rule: if a wire is dead at an "elimination point" past its final use, then it can be removed from the const and faint information. for consts, the final use is in the forward direction. for faints, the final use is in the backwards direction.

(defparameter input-functions (set-from-list (list "alice" "bob") :comp #'string<))

(defparameter output-functions (set-from-list (list "output_alice" "output_bob") :comp #'string<))

(defparameter *global-condition-wire* 0)

(defgeneric compute-used-wires (op)
  (:documentation "determines which wires are used in a block. returns a map of wires in use -> block number")
)

(defmacro def-use-block (type &body body)
  `(defmethod compute-used-wires ((op ,type))
     (declare (optimize (debug 3)(speed 0)))
     (aif (locally ,@body)
          it
          nil)))

(def-use-block bits
  (with-slots (dest op1) op
      (append (list op1) dest)))

(def-use-block join
  (with-slots (dest op1) op
      (append (list dest) op1)))

(def-use-block gate
  (with-slots (dest op1 op2) op
        (list dest op1 op2)))

(def-use-block const
  (with-slots (dest) op
    (list dest)))

(def-use-block add 
  (with-slots (dest op1 op2) op
        (list dest op1 op2)))

(def-use-block mul
  (with-slots (dest op1 op2) op
      (list dest op1 op2)))

(def-use-block sub
  (with-slots (dest op1 op2) op
        (list dest op1 op2)))

(def-use-block copy
  (with-slots (dest op1 op2) op
        (if (equal op2 1)
            (list dest op1)
            (append (loop for i from dest to (+ dest op2) collect i)
                    (loop for i from op1 to (+ op1 op2) collect i)))))

;; the following two don't do any pointer indirection because we have no constant propagation data to help us. We do the best we can be using the non-indirection information we know for eliminating information and not eliminating wires that have gone unrecognized because of indirection. The implementation of eliminating information, however, is elsewhere.
(def-use-block indir-copy 
  (with-slots (op1 op2) op
     (if (equal op2 1)
         (list op1)
         (loop for i from op1 to (+ op1 op2) collect i))))

(def-use-block copy-indir
  (with-slots (dest op2) op
       (if (equal op2 1)
           (list dest)
           (loop for i from dest to (+ dest op2) collect i))))

(def-use-block branch
  (with-slots (cnd) op
    (list cnd)))

(def-use-block initbase
  (list *global-condition-wire*)) ;; the global condition wire should be initialized _immediately_ as used.

(def-use-block mkptr
  (with-slots (dest) op
    (list dest)))

(def-use-block call
  (with-slots (newbase fname) op
    (if (or (set-member fname input-functions) (set-member fname output-functions))
        (append
          (loop for i from (- newbase 32) to (- newbase 1) collect i)
          (loop for i from newbase to (+ newbase 32) collect i))
        nil) ;; this can be filled in later
        ))

(def-use-block ret
  (list *global-condition-wire*)) ;; nothing 
(def-use-block clear) ;; nothing
(def-use-block label) ;; nothing
