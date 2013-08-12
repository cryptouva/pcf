;; Translate LCC opcodes to PCF2 opcodes.  Also removes forward jumps
;; and creates the appropriate muxes.

(defpackage :lcc-translator (:use 
                             :skew-list
                             :pcf2-bc 
                             :priority-queue
                             :string-tokenizer
                             :common-lisp
                             #+sbcl :sb-mop #+cmu :mop)
            (:export
             adder-chain
             read-instructions))
(in-package :lcc-translator)
(use-package :pcf2-bc)

(let ((cur-addr 0))
  (defun more-memory ()
    (incf cur-addr)
    )
  )

(defun adder-chain (xs ys zs &optional (c-in (more-memory)) (tmp1 (more-memory)) (tmp2 (more-memory)) (tmp3 (more-memory)))
  "Create a ripple carry adder chain.  By default, the carry-in, tmp1,
tmp2, and tmp3 locations are simply allocated, making the stack frame
larger.  These can (and should) be explicitly specified, as these are
only temporary and can be safely overwritten by future instructions."
  (assert (= (length xs) (length ys) (length zs)))
  (labels ((full-adder (x y z)
             (let* ((sum-ops (list (make-xor tmp1 x y)
                                   (make-xor z tmp1 c-in)
                                   )
                      )
                    (carry-ops (list (make-xor tmp1 x y)
                                     (make-xor tmp2 c-in x)
                                     (make-and tmp3 tmp1 tmp2)
                                     (make-xor c-in x tmp3)
                                     )
                      )
                    )
               (append sum-ops carry-ops)
               )
             )
           )
    (mapcan #'full-adder xs ys zs)
    )
  )

(defun and-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-and zs xs ys)
  )

(defun xor-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-xor zs xs ys)
  )

(defun equl (xs ys z &optional (tmp1 (more-memory)) (tmp2 (more-memory)))
  (assert (= (length xs) (length ys)))
  (mapcan (lambda (x y)
            (list (make-xor tmp1 x y)
                  (make-not tmp2 tmp1)
                  (make-and z tmp2 z)
                  )
            ) 
          xs ys)
  )

(defun alloc-wires (wires n)
  (cons (+ n (first wires)) wires)
  )

(defclass lcc-instruction ()
    ()
    (:documentation "Base class for all LCC instructions.")
    )

(defclass code (lcc-instruction)
  ()
  )

(defclass static-arg-instruction (lcc-instruction)
  ((s-args :initarg :s-args))
  (:documentation "The base class of instructions that have static arguments specified in the bytecode.  These produce values of the specified width.")
  )

(defclass labelv (static-arg-instruction)
  ()
  )

(defclass export (static-arg-instruction)
  ()
  )

(defclass proc (static-arg-instruction)
  ()
  )

(defclass endproc (static-arg-instruction)
  ()
  )

(defclass import (static-arg-instruction)
  ()
  )

(defclass addrlp (static-arg-instruction)
  ()
  )

(defclass addrgp (static-arg-instruction)
  ()
  )

(defclass cnstu (static-arg-instruction)
  ()
  )

(defclass stack-arg-instruction (lcc-instruction)
  ((width :initarg :width))
  (:documentation "The base class of instructions that take arguments and return values.  The \"width\" member specifies the width of the return value")
  )

(defclass one-arg-instruction (stack-arg-instruction)
  ()
  (:documentation "The base class of instructions that pop one argument off the stack.")
  )

(defclass jumpv (one-lcc-instruction)
  ()
  )

(defclass callv (lcc-instruction)
  ()
  )

(defclass callu (one-arg-instruction)
  ()
  )

(defclass argu (one-arg-instruction)
  ()
  )

(defclass indiru (one-arg-instruction)
  ()
  )

(defclass two-arg-instruction (stack-arg-instruction)
  ()
  (:documentation "The base class of instructions that pop two arguments off the stack.")
  )

(defclass asgnu (two-arg-instruction)
  ()
  )

(defclass bandu (two-arg-instruction)
  ()
  )

(defclass addu (two-arg-instruction)
  ()
  )

(defun parse-instruction (ln)
  "Parse an LCC bytecode instruction from a string"
  (let* ((tokens (tokenize ln))
         (op (intern (map 'string #'char-upcase (first tokens))))
         (cls (find-class op))
         (scls (class-direct-superclasses cls))
         )
    (assert (= (length scls) 1))
    (apply #'make-instance (cons cls 
                                 (ecase (class-name (car scls))
                                   ((one-arg-instruction two-arg-instruction) (list ':width (parse-integer (second tokens))))
                                   (static-arg-instruction (list ':s-args (rest tokens)))
                                   (lcc-instruction nil)
                                   )
                                 )
           )
    )
  )

(defun read-instructions (stream &optional (lst nil))
  "Load LCC bytecode instructions into a skew-binary list.  We use a skew-binary list here because it allows us to conveniently index instructions that we read, which is useful for translating conditional jump instructions."
  (let ((ln (read-line stream nil))
        )
    (if ln
        (read-instructions stream (skew-cons (parse-instruction ln) lst))
        (skew-reverse lst)
        )
    )
  )

(defgeneric add-label (op idx labs)
  )

(defmethod add-label ((op lcc-instruction) idx labs)
  (declare (ignore op idx))
  labs)

(defmethod add-label ((op labelv) idx labs)
  (with-slots (s-args) op
    (setf (gethash (first s-args) labs) idx)
    labs
    )
  )

(defun find-labels (instructions &optional (idx 0) (labs (make-hash-table)))
  "Find the index of all labels in the list of instructions, returning a hash table"
  (declare (type hash-table labs))
  (if (null instructions)
      labs
      (let ((op (skew-first instructions))
            )
        (find-labels (skew-rest instructions) (1+ idx) (add-label op idx labs))
        )
    )
  )

(defgeneric exec-instruction (op stack wires instrs labels)
  (:documentation "This generic method translates an LCC opcode into PCF2 opcodes.  The \"stack\" argument represents the state of the LCC stack at this program point.  The \"wires\" argument represents the location of free wires in the program, which roughly corresponds to the state of the LCC stack.

This method should return the updated stack, wires, and instructions.")
  )

(defun exec-instructions (ops)
  "Translate a list of LCC opcodes into PCF2 opcodes"
  (declare (optimize (debug 3) (speed 0)))
  (let ((lbls (find-labels ops))
        )
    (let ((rvl 
           (skew-reduce (lambda (st op) (let ((stack (first st))
                                              (wires (second st))
                                              (instrs (third st))
                                              )
                                          (exec-instruction op stack wires instrs lbls)
                                          )
                                )
                        ops
                        (list nil 0 nil)
                        )
            )
          )
      (cons (reverse (first rvl)) (rest rvl))
      )
    )
  )

(defun base2 (x n)
  (if (zerop n)
      nil
      (cons (mod x 2) (base2 (ash x -1) (1- n)))
      )
  )

(defmethod exec-instruction ((op cnstu) stack wires instrs labels)
  (with-slots (s-args) op
    (let ((width (* 8 (parse-integer (first s-args))))
          (value (parse-integer (second s-args)))
          )
      (let ((dwires (loop for i from wires to (+ wires width -1) collect i))
            )
        (let ((instrs (append (list 
                               (make-instance 'bits :dest dwires :op1 wires)
                               (make-instance 'const :dest wires :op1 value)
                               )
                              instrs)
                )
              (wires (+ wires width))
              (stack (append dwires stack))
              )
          (list stack wires instrs)
          )
        )
      )
    )
  )

(defmethod exec-instruction ((op addu) stack wires instrs labels)
  (with-slots (width) op
    (let ((width (* 8 width)))
      (let ((arg1 (subseq stack 0 width))
            (stack (nthcdr width stack))
            (rwires (loop for i from wires to (+ wires width -1) collect i))
            )
        (let ((arg2 (subseq stack 0 width))
              (stack (nthcdr width stack))
              )
          (let ((retins (append (reverse (adder-chain arg1 arg2 rwires (+ wires width 1) (+ wires width 2) (+ wires width 3) (+ wires width 4))) instrs))
                (retstack (append rwires stack))
                (retwires (+ wires width))
                )
            (list retins retstack retwires)
            )
          )
        )
      )
    )
  )