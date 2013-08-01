;; New PCF bytecode format definitions/etc.
;;
;; In the new PCF format, we have the following changes:
;;
;; 1. Base pointers and indirect addressing
;; 2. Non-bitwise operations for input-independent types

(defpackage :pcf2-bc (:use :common-lisp :sb-mop))
(in-package :pcf2-bc)

(defclass instruction ()
  ()
  (:documentation "Base class for instructions in PCF2"))

(defmethod print-object ((object instruction) stream)
  (let* ((obj-class (class-of object))
         (obj-slots (class-slots obj-class))
         )
      (labels ((print-slot (slot)
             (format stream "(~A ~A) " (slot-definition-name slot) (slot-value-using-class obj-class object slot))
             )
           )
        (format stream "~A " (class-name obj-class))
        (mapc #'print-slot obj-slots)
        )
      )
  )

(defclass call (instruction)
  ((newbase :initarg :newbase)
   (fname :initarg :fname)
   )
  (:documentation "Call a function")
  )

(defclass ret (instruction)
  ((value :initarg :value)
   )
  (:documentation "Return from a function call")
  )

(defclass two-op (instruction)
  ((dest :initarg :dest)
   (op1 :initarg :op1)
   (op2 :initarg :op2)
   )
  (:documentation "This class is for instructions that take two operands and produce one result")
  )

(defclass gate (two-op)
  ((truth-table :initarg :truth-table)
   )
  )

(defclass branch (instruction)
  ((cnd :initarg :cnd)
   (targ :initarg :targ)
   )
  (:documentation "Branch instructions should only be used for loops")
  )

(defclass label (instruction)
  ((str :initarg :str)
   )
  (:documentation "Placeholder instruction for assigning a label")
  )

(defclass add (two-op)
  ()
  (:documentation "Performs addition on two input-independent values")
  )

(defclass sub (two-op)
  ()
  (:documentation "Subtracts two input-independent values")
  )

(defclass mul (two-op)
  ()
  (:documentation "Multiplies two input-independent values")
  )

(defclass mkptr (instruction)
  ((dest :initarg :dest)
   )
  (:documentation "Creates a pointer at a particular position")
  )

(defclass one-op (instruction)
  ((dest :initarg :dest)
   (op1 :initarg :op1)
   )
  (:documentation "Instructions that take one argument")
  )

(defclass copy (one-op)
  ()
  (:documentation "Copies a value from one position to another")
  )

(defclass const (one-op)
  ()
  (:documentation "Creates a constant value")
  )

(defclass copy-indir (one-op)
  ()
  (:documentation "Dereference a pointer and copy the derefenced value to a location (x = *y)")
  )

(defclass indir-copy (one-op)
  ()
  (:documentation "Copy a value to the location pointed to by a pointer (*x = y)")
  )