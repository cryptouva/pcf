;;; This file analyzes the flow of information through gates in a PCF2 program and builds its own flow graph

(defpackage :pcf2-gateanalysis
  (:use :cl :pcf2-bc :pcf2-dataflow :utils)
  (:export make-circuit-graph)
  )

(in-package :pcf2-gateanalysis)

;;; build a flow graph of information through gates based on the pcf2 cfg constructed by pcf2-dataflow
;;; this will help with deadcode elimination, constant propagation, and more
;;; this analysis should perform some special kinds of constant propagation
;;;   such as optimizations on partial-information computations, e.g. AND with 0, OR with 1

(defstruct (gate-block
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&Gate basic block ~A:~%" (basic-block-id struct))
                (format stream "Ops: ~A~%" (basic-block-ops struct))
                (format stream "Inputs: ~A~%" (basic-block-preds struct))
                (format stream "Outputs: ~A~%" (basic-block-succs struct))
                )
              )
             )
  (value) ;; unknown or some const, modeled by nil or the const, which should evaluate to t
  (ops nil :type list)
  (inputs nil :type list)
  (outputs nil :type list)
  (:documentation "This represents a basic block in the gate graph.")
  )

(defgeneric circuit-update (cfg gatemap consts)
  (:documentation "update the circuit with information from a cfg block")
  )

;; AND Gate: #*0001
;; OR Gate: #*0111
;; NOT Gate: #*0011

(defmacro definstr (type &body body)
  `(defmethod circuit-update ((op ,type) cfg gatemap consts)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (basic-update))))

(defmacro basic-update ()
  (list cfg gatemap consts)
)

(definstr gate)

(definstr const
  ;; this is basically a kill on whatever value was in the gate and a new gen for that address.
  ;; we model this by creating a new entry in the map with a value field
)

(defun make-circuit-graph (cfg labels)
  
)
