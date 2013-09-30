;; New PCF bytecode format definitions/etc.
;;
;; In the new PCF format, we have the following changes:
;;
;; 1. Base pointers and indirect addressing
;; 2. Non-bitwise operations for input-independent types

(defpackage :pcf2-bc 
  (:use :common-lisp #+sbcl :sb-mop #+cmu :mop)
  (:export instruction
           clear
           localsize
           initbase
           call
           ret
           branch
           add
           sub
           mul
           gate
           bits
           join
           const
           label
           copy
           copy-indir
           indir-copy
           mkptr
           make-and
           make-or
           make-xnor
           make-xor
           make-not
           make-gate
           dest
           op1
           op2
           truth-table
           value
           newbase
           fname
           cnd
           targ
           str
           read-bytecode
           base
           two-op
           one-op))
(in-package :pcf2-bc)

(defclass instruction ()
  ()
  (:documentation "Base class for instructions in PCF2"))

(defmethod print-object ((object instruction) stream)
  (let* ((obj-class (class-of object))
         (obj-slots (class-slots obj-class))
         )
      (labels ((print-slot (slot)
                 (let ((type (slot-definition-type slot))
                       )
                   (cond
                     ;; We need to treat strings in a special manner,
                     ;; so that we can read them back in as strings
                     ;; later.  This makes type definitions all the
                     ;; more important!
                     ((equalp type 'string) (format stream ":~A ~S "
                                                   (slot-definition-name slot)
                                                   (slot-value-using-class obj-class object slot)
                                                   )
                      )
                     (t (format stream ":~A ~A " 
                                (slot-definition-name slot)                     
                                (slot-value-using-class obj-class object slot)))
                     )
                   )
             )
           )
        (format stream "(~A " (class-name obj-class))
        (mapc #'print-slot obj-slots)
        (format stream ")")
        )
      )
  )

(defun parse-object (str)
  (let ((*read-eval* nil))
    (let* ((tokens (read-from-string str))
           (op (intern (map 'string #'char-upcase (symbol-name (first tokens))) :pcf2-bc))
           )
      (apply #'make-instance (cons op (rest tokens)))
      )
    )
  )

(defun read-bytecode (&optional (stream *standard-input*) (lst nil))
  "Read bytecode from a file, returning the results as a list"
  (let ((ln (read-line stream nil))
        )
    (if ln
        (read-bytecode stream (cons (parse-object ln) lst))
        (reverse lst)
        )
    )
  )

(defclass initbase (instruction)
  ((base :initarg :base))
  (:documentation "This instruction initializes the base pointer.  It should only appear once, at the very beginning of the program.")
  )

(defclass call (instruction)
  ((newbase :initarg :newbase)
   (fname :initarg :fname :type string)
   )
  (:documentation "Call a function")
  )

(defclass clear (instruction)
  ((localsize :initarg :localsize :type integer)
   )
  (:documentation "Set memory from baseptr to localsize to 0.  This is meant to be used at the entry to a function.")
  )

(defclass ret (instruction)
  ((value :initarg :value)
   )
  (:documentation "Return from a function call")
  )

(defclass two-op (instruction)
  ((dest :initarg :dest :type integer)
   (op1 :initarg :op1 :type integer)
   (op2 :initarg :op2 :type integer)
   )
  (:documentation "This class is for instructions that take two operands and produce one result")
  )

(defclass gate (two-op)
  ((truth-table :initarg :truth-table)
   )
  )

(defun make-gate (tab d x y)
  "Shorthand for creating a gate"
  (declare (type (simple-bit-vector 4) tab)
           (type (integer 0) d x y))
  (make-instance 'gate :dest d :op1 x :op2 y :truth-table tab)
  )

(defun make-or (d x y)
  (make-gate #*0111 d x y)
  )

(defun make-xnor (d x y)
  (make-gate #*1001 d x y)
  )

(defun make-xor (d x y)
  "Shorthand for creating an XOR gate"
  (make-gate #*0110 d x y)
  )

(defun make-and (d x y)
  "Shorthand for an AND gate"
  (make-gate #*0001 d x y)
  )

(defun make-not (d x)
  "Shorthand for a NOT gate"
  (make-gate #*1100 d x x)
  )

(defclass branch (instruction)
  ((cnd :initarg :cnd)
   (targ :initarg :targ :type string)
   )
  (:documentation "Branch instructions should only be used for loops")
  )

(defclass label (instruction)
  ((str :type string :initarg :str)
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

(defclass bits (one-op)
  ()
  (:documentation "Split an integer value into its two's complement representation")
  )

(defclass join (one-op)
  ()
  (:documentation "Coelesce a vector of wires into a single unsigned integer value")
  )

(defclass copy-base (instruction)
  ((dest :initarg :dest)
   (op1 :initarg :op1)
   (op2 :initarg :op2)
   )
  (:documentation "Base class for instructions that copy data")
  )

(defclass copy (copy-base)
  ()
  (:documentation "Copies a value from one position to another")
  )

(defclass const (one-op)
  ()
  (:documentation "Creates a constant value")
  )

(defclass copy-indir (copy-base)
  ()
  (:documentation "Dereference a pointer and copy the derefenced value to a location (x = *y)")
  )

(defclass indir-copy (copy-base)
  ()
  (:documentation "Copy a value to the location pointed to by a pointer (*x = y)")
  )

