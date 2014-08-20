;;; this scans a list of instructions to examine def-use chains and eliminates gates from the oplist that never contribute to output

(defpackage :pcf2-deadgate
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-dataflow)
  (:export make-def-use-map)
  )

(in-package :pcf2-deadgate)

#|
(defstruct (dead-flow-block
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&PCF2 basic block ~A:~%" (basic-block-id struct))
                )
              )
             )
  (id)
  (bb (make-basic-block) :type basic-block)
  (true-preds nil :type list)
  (true-succs nil :type list)
  (:documentation "This represents a block of the deadcode graph.")
  )
|#


;; this analysis tracks the uses of wires through a program to determine which are actually useful to the output and which are merely dead weight
;; we do this by tracing wire values backwards from their outputs with output_alice and output_bob to their initialization. wires that are not marked, i.e. do not contribute to the output, may be discarded

(defgeneric def-use-map-update (op idmap output-wires id)
  (:documentation "this function updates our def-use map per instruction so that we may identify gates that never matter")
  )

(defmacro close-update ()
  `(list idmap output-wires uid))

(defmethod def-use-map-update ((op instruction) idmap output-wires uid)
  (close-update))

(defmethod def-use-map-update ((op gate) idmap output-wires uid)
  ()
  )

(defmethod def-use-map-update ((op bits) idmap output-wires uid)
  (with-slots (dest) op
    (reduce (lambda (state d)
              (let ((uid (third state))
                    (idmap (first state))
                    (output-wires (second state))
                    )
                (list 
                 (map-insert d uid idmap)
                 output-wires
                 (1+ uid))))
              dest
              :initial-value (list idmap output-wires uid)
              )))

(defmethod def-use-map-update ((op join) idmap output-wires uid)
  
  )

;; use dest, op1, and op2 (len) to update chains for all of the wires
(defmethod def-use-map-update ((op copy) idmap output-wires uid)
  (close-update)
)

;; end old wire, begin new one
(defmethod def-use-map-update ((op const) idmap output-wires uid)
  (with-slots (dest) op
    (list
     (map-insert (1+ uid) dest idmap)
     output-wires
     (1+ uid))))

(defun make-def-use-map (ops)
  ;; we start with a simple, linear program without any indirection and only one function (main). we use the list of ops to identify def/use chains and eliminate the gates that are inconsequential to the program. later, this function may be extended and adapted for use of code segments rather than whole programs
  ;; the next version of this will use a def-use framework rather than the less general form of this analysis
  ;; we can do this by backtracking from the cfg that we already have on each output_alice and output_bob statement
  ;; use a list of gate # -> unique gate identifier
  (reduce (lambda (state op)
            (apply #'def-use-map-update (cons op state)))
          ops
          :initial-value (list (map-empty) (map-empty) 0)))
