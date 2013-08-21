;; Translate LCC opcodes to PCF2 opcodes.  Also removes forward jumps
;; and creates the appropriate muxes.
;;
;; Each function should take an extra parameter, the pointer to the
;; mux condition wire.  This ensures that a function can be called
;; conditionally, as its assignments will all use multiplexers.  We
;; will eventually use an interprocedural dataflow analysis framework
;; to handle the removal of these muxes from functions that are never
;; called conditionally.
;;
;; Since the use of a pointer to the condition wire will carry some
;; cost, we should also have a points-to analysis framework that can
;; be used to remove extraneous copy and multiplexer operations.

(defpackage :lcc-translator (:use 
                             :skew-list
                             :pcf2-bc 
                             :priority-queue
                             :string-tokenizer
                             :common-lisp
                             #+sbcl :sb-mop #+cmu :mop)
            (:export
             parse-instruction
             read-instructions
             exec-instructions)
            (:shadow
             export
             import))
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

(defun subtractor-chain (xs ys zs &optional c-in tmp1 tmp2 tmp3)
  "Create a ripple-borrow subtractor chain."
  (assert (= (length xs) (length ys) (length zs)))
  (labels ((full-subtractor (x y z)
             (let* ((diff-ops (list (make-xor tmp1 x y)
                                    (make-xor z tmp1 c-in)
                                    )
                      )
                    (borrow-ops (list (make-xnor tmp1 x y)
                                      (make-and tmp2 c-in tmp1)
                                      (make-not tmp1 x)
                                      (make-and tmp3 tmp1 y)
                                      (make-or c-in tmp3 tmp2))
                      )
                    )
               (append diff-ops borrow-ops)
               )
             )
           )
    (mapcan #'full-subtractor xs ys zs)
    )
  )

(defun and-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-and zs xs ys)
  )


(defun shift-and-add-multiplier (xs ys zs &optional c-in tmp1 tmp2 tmp3 tmpr)
  "Create a shift-and-add multiplier using ripple-carry adders"
  (declare (optimize (speed 0) (debug 3)))
  (assert (= (length xs) (length ys) (length zs)))
  (labels ((shift-add (st x)
             (let ((n (first st))
                   (ops (second st))
                   )
               (list (1+ n) (append ops (list (and-chain (loop for i in ys collect x) ys tmpr) 
                                              (adder-chain (append (loop for i from 1 to n collect 0) (butlast tmpr n)) zs zs c-in tmp1 tmp2 tmp3))))
               )
             )
           )
    (reduce #'shift-add xs :initial-value (list 0 nil))
    )
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

(defclass cnd-jump-instruction (static-arg-instruction)
  ()
  (:documentation "The base class of instructions that cause conditional branches.")
  )

(defclass jumpv (cnd-jump-instruction one-arg-instruction)
  ()
  (:documentation "This is a conditional branch instruction because it might occur when the targets queue is not empty.  In such a case, this is a conditional branch whose condition is the current mux condition wire.  This is a template for emitting code for this instruction, although it would be better to not emit muxes all over the place like that.")
  )

(defclass ltu (cnd-jump-instruction two-arg-instruction)
  ()
  )

(defclass leu (cnd-jump-instruction two-arg-instruction)
  ()
  )

(defclass neu (cnd-jump-instruction two-arg-instruction)
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
         (op (intern (map 'string #'char-upcase (first tokens)) :lcc-translator))
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
    (setf (gethash (second s-args) labs) idx)
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

(defgeneric exec-instruction (op labels stack wires instrs targets arglist argsize)
  (:documentation "This generic method translates an LCC opcode into PCF2 opcodes.  The \"stack\" argument represents the state of the LCC stack at this program point.  The \"wires\" argument represents the location of free wires in the program, which roughly corresponds to the state of the LCC stack.

This method should return the updated stack, wires, and instructions.  Instructions should be given in reverse order, as this allows instructions to be appended to the instructions list efficiently.

The \"argbase\" parameter represents the list of arguments for the next function call.  When a call instruction is emitted, these arguments will be copied to the base of the next stack frame.  \"argsize\" is the number of arguments for the current function, which we use for addrfp instructions and as an offset for addrlp instructions.")
  )

(defun exec-instructions (ops)
  "Translate a list of LCC opcodes into PCF2 opcodes.  The opcodes should be formatted as a skew-binary list."
  (declare (optimize (debug 3) (speed 0)))
  (let ((lbls (find-labels ops))
        )
    (let ((rvl 
           (skew-reduce (lambda (st op) 
                          (apply #'exec-instruction (append (list op lbls) st));stack wires instrs lbls targs argbase)
                          )
                        ops
                        (list nil 0 nil nil nil 0)
                        )
            )
          )
      (cons (reverse (third rvl)) (rest rvl))
      )
    )
  )

(defun base2 (x n)
  (if (zerop n)
      nil
      (cons (mod x 2) (base2 (ash x -1) (1- n)))
      )
  )

(defmacro close-instr ()
  "This macro will finalize an instruction translator method.  It is a
convenience macro that ensures the returned list has the right length."
  `(progn
     (list stack wires instrs targets arglist argsize)
     )
  )

(defmacro definstr (type &body body)
  "Instruction translator methods are defined with this macro.  It is
a convenience macro that ensures that the method takes the right
number of arguments."
  `(defmethod exec-instruction ((op ,type) labels stack wires instrs targets arglist argsize)
     ,@body
     )
  )

(defmacro push-stack (stack width val &body body)
  `(let ((,stack (append ,val ,stack))
         )
     (let ((,stack (cons ,width ,stack))
           )
       ,@body
       )
     )
  )

(defmacro pop-arg (stack argname &body body)
  (let ((wsym (gensym))
        )
    `(let* ((,wsym (first ,stack))
            (,stack (rest ,stack))
            (,argname (subseq ,stack 0 ,wsym))
            (,stack (nthcdr ,wsym ,stack))
            )
       ,@body
       )
    )
  )

(defmacro add-instrs (inslst &body body)
  (let ((inslstsym (gensym))
        )
    `(let* ((,inslstsym ,inslst)
            (instrs (append (reverse ,inslstsym) instrs))
            )
       (mapc #'(lambda (x) (assert (typep x 'pcf2-bc:instruction))) ,inslstsym)
       ,@body
       )
    )
  )

(defmacro add-target (target-label cnd-value &body body)
  `(let ((t-idx (gethash ,target-label labels nil))
         )
     (assert t-idx)
     (let ((targets (enqueue (cons t-idx ,cnd-value) targets))
           )
       ,@body
       )
     )
  )

(defun eql-gates (arg1 arg2 dest tmp1)
  (assert (= (length arg1) (length arg2)))
  (append (list (make-xnor tmp1 
                           (first arg1)
                           (first arg2))
                (make-and dest tmp1 dest)
                )
          (eql-gates (rest arg1) (rest arg2) dest tmp1)
          )
  )

(definstr neu
  (with-slots (s-args width) op
    (let ((width (* 8 width))
          (targ (second s-args))
          )
      (declare (type string targ))
      (pop-arg stack arg1
        (pop-arg stack arg2
          (assert (and (= width (length arg1) (length arg2))))
          (add-instrs (append 
                       (list (make-instance 'copy-indir :dest wires :op1 0 :op2 1))
                       ;; This will update the value of the global condition wire appropriately,
                       ;; as it ANDs on the value of comparing all these gates.  This is a hack
                       ;; and should not be used in other comparison methods.
                       (eql-gates arg1 arg2 wires (1+ wires))
                       (list (make-not (+ 2 wires) (+ 1 wires))
                             (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                             (make-instance 'branch :cnd (+ 2 wires) :targ targ)
                             )
                       )
            (add-target targ (+ 2 wires)
              (let ((wires (+ 3 wires))
                    )
                (close-instr)
                )
              )
            )
          )
        )
      )
    )
  )

(definstr cnstu
  (with-slots (s-args) op
    (let ((width (* 8 (parse-integer (first s-args))))
          (value (parse-integer (second s-args)))
          )
      (let ((dwires (loop for i from wires to (+ wires width -1) collect i))
            )
        (add-instrs (list (make-instance 'const :dest wires :op1 value)
                          (make-instance 'bits :dest dwires :op1 wires))
          (let ((wires (+ wires width))
                )
            (push-stack stack width dwires
              (close-instr)
              )
            )
          )
        )
      )
    )
  )

(definstr addu
  (with-slots (width) op
    (let* ((width (* 8 width))
           (rwires (loop for i from wires to (+ wires width -1) collect i))
           )
      (pop-arg stack arg1
        (pop-arg stack arg2
          (push-stack stack width rwires
            (add-instrs (adder-chain 
                         arg1 
                         arg2 
                         rwires 
                         (+ wires width 1) 
                         (+ wires width 2) 
                         (+ wires width 3) 
                         (+ wires width 4))
              (let ((wires (+ wires width))
                    )
                (close-instr)
                )
              )
            )
          )
        )
      )
    )
  )

(definstr bandu
  (with-slots (width) op
    (let* ((width (* 8 width))
           (rwires (loop for i from wires to (+ wires width -1) collect i))
           )
      (pop-arg stack arg1
        (pop-arg stack arg2
          (push-stack stack width rwires
            (add-instrs (and-chain arg1 arg2 rwires)
              (let ((wires (+ wires width))
                    )
                (close-instr)
                )
              )
            )
          )
        )
      )
    )
  )

(defstruct arg 
  (len)
  (loc)
  )

(definstr callv
  (pop-arg stack fname
    (assert (and (= 1 (length fname))
                 (typep (first fname) 'string)
                 )
            )
    (add-instrs (append (let ((i 0))
                            (loop for arg in arglist collect
                                 (prog1
;                                     (loop for j in (arg-loc arg) for k from 0 collect
                                     (make-instance 'copy 
                                                    :dest (+ wires i)
                                                    :op1 (first (arg-loc arg))
                                                    :op2 (arg-len arg))
;                                   )
                                   (incf i (arg-len arg))
                                   )
                                 )
                            )
                        (list (make-instance 'call :newbase wires :fname (first fname)))
                        )
      (close-instr)
      )    
    )
  )

(definstr callu
  (with-slots (width) op
    (let ((width (* 8 width))
          )
      (pop-arg stack fname
        (add-instrs (append (let ((i 0))
                                (loop for arg in arglist collect
                                     (prog1
                                         (make-instance 'copy 
                                                        :dest (+ wires i)
                                                        :op1 (first (arg-loc arg))
                                                        :op2 (arg-len arg))
                                       (incf i (arg-len arg))
                                       )
                                     )
                                )
                            (list (make-instance 'call :newbase wires :fname (first fname)))
                            )
          (push-stack stack width (loop for i from wires to (+ wires width -1) collect i)
            (close-instr)
            )
          )
        )
      )
    )
  )

(definstr argu
  (with-slots (width) op
    (pop-arg stack arg
      (let ((arglist (append arglist (list (make-arg :len (* 8 width) :loc arg))))
            )
        (close-instr)
        )
      )
    )
  )

(definstr addrgp
  (declare (optimize (debug 3) (speed 0)))
  (with-slots (s-args) op
    (let ((addr (if (gethash (second s-args) labels nil)
                    (gethash (second s-args) labels nil)
                    (second s-args))
            )
          )
      (push-stack stack 1 (list addr)
        (close-instr)
        )
      )
    )
  )

(definstr addrlp
  (with-slots (s-args) op
    (let ((addr (parse-integer (second s-args)))
          )
      (push-stack stack 1 (list wires)
        (add-instrs (list 
                     (make-instance 'const :dest wires :op1 (+ argsize (* 8 addr)))
                     (make-instance 'mkptr :dest wires)
                     )
          (let ((wires (1+ wires))
                )
            (close-instr)
            )
          )
        )
      )
    )
  )

(definstr indiru
  ;; Pop a pointer off the stack, dereference the pointer and push its value back on the stack
  (with-slots (width) op
    (pop-arg stack ptr
      (add-instrs (list (make-instance 'copy-indir :dest wires :op1 (first ptr) :op2 (* 8 width)))
        (push-stack stack (* 8 width) (loop for i from wires to (+ wires (* 8 width) -1) collect i)
          (let ((wires (+ wires (* 8 width)))
                )
            (close-instr)
            )
          )
        )
      )
    )
  )

(definstr asgnu
  (with-slots (width) op
    (let* ((width (* 8 width))
           )
      (pop-arg stack val
        (pop-arg stack ptr
          (assert (= 1 (length ptr)))
          (add-instrs (list (make-instance 'indir-copy :dest (first ptr) :op1 (first val) :op2 width))
            (close-instr)
            )
          )
        )
      )
    )
  )

(definstr proc
  (with-slots (s-args) op
    (let ((local-size (parse-integer (second s-args)))
          (args-size (parse-integer (third s-args)))
          )
      (let ((wires (+ wires (* 8 local-size)))
            (argsize (* 8 args-size))
            )
        (close-instr)
        )
      )
    )
  )

(definstr endproc
  (assert (null stack))
  (let ((wires 0)
        )
    (close-instr)
    )
  )

(definstr import
  (close-instr)
  )

(definstr export
  (close-instr)
  )

(definstr code
  (close-instr)
  )

(definstr labelv
  (with-slots (s-args) op
    (add-instrs (list (make-instance 'pcf2-bc:label :str (first s-args)))
      (close-instr)
      )
    )
  )