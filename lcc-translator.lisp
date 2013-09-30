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
    (cons
     (make-instance 'const :dest c-in :op1 0)
     (mapcan #'full-adder xs ys zs)
     )
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
    (cons
     (make-instance 'const :dest c-in :op1 0)
     (mapcan #'full-subtractor xs ys zs)
     )
    )
  )

(defun and-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-and zs xs ys)
  )

(defun or-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-or zs xs ys)
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

(defclass bss (lcc-instruction)
  ()
  )

(defclass static-arg-instruction (lcc-instruction)
  ((s-args :initarg :s-args))
  (:documentation "The base class of instructions that have static arguments specified in the bytecode.  These produce values of the specified width.")
  )

(defclass align (static-arg-instruction)
  ()
  )

(defclass skip (static-arg-instruction)
  ()
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

(defclass addrfp (static-arg-instruction)
  ()
  )

(defclass addrgp (static-arg-instruction)
  ()
  )

(defclass indirp (static-arg-instruction)
  ()
  )

(defclass asgnp (static-arg-instruction)
  ()
  )

(defclass addp (two-arg-instruction)
  ()
  )

(defclass argp (one-arg-instruction)
  ()
  )

(defclass lshu (two-arg-instruction)
  ()
  )

(defclass rshu (two-arg-instruction)
  ()
  )

(defclass rshi (two-arg-instruction)
  ()
  )

(defclass boru (two-arg-instruction)
  ()
  )

(defclass bxoru (two-arg-instruction)
  ()
  )

(defclass cnsti (static-arg-instruction)
  ()
  )

(defclass cnstu (static-arg-instruction)
  ()
  )

(defclass one-stack-one-static-arg-instruction (lcc-instruction)
  ((s-args :initarg :s-args)
   )
  (:documentation "The base class of instructions that pop one argument from the stack and have one static argument instruction.  The \"CV*\" instructions are in this class.")
  )

(defclass CVUI (one-stack-one-static-arg-instruction)
  ()
  )

(defclass CVIU (one-stack-one-static-arg-instruction)
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

(defclass jump-instruction (cnd-jump-instruction)
  ()
  (:documentation "This class is needed for technical reasons in the routine that reads LCC bytecode")
  )

(defclass jumpv (jump-instruction)
  ()
  (:documentation "This is a conditional branch instruction because it might occur when the targets queue is not empty.  In such a case, this is a conditional branch whose condition is the current mux condition wire.  This is a template for emitting code for this instruction, although it would be better to not emit muxes all over the place like that.")
  )

(defclass cmp-jump-instruction (cnd-jump-instruction)
  ((width :initarg :width))
  )

(defclass ltu (cmp-jump-instruction)
  ()
  )

(defclass leu (cmp-jump-instruction)
  ()
  )

(defclass gtu (cmp-jump-instruction)
  ()
  )

(defclass geu (cmp-jump-instruction)
  ()
  )

(defclass neu (cmp-jump-instruction)
  ()
  )

(defclass equ (cmp-jump-instruction)
  ()
  )

(defclass callv (lcc-instruction)
  ()
  )

(defclass callu (one-arg-instruction)
  ()
  )

(defclass calli (one-arg-instruction)
  ()
  )

(defclass reti (one-arg-instruction)
  ()
  )

(defclass retu (one-arg-instruction)
  ()
  )

(defclass argu (one-arg-instruction)
  ()
  )

(defclass indiru (one-arg-instruction)
  ()
  )

(defclass indiri (one-arg-instruction)
  ()
  )

(defclass two-arg-instruction (stack-arg-instruction)
  ()
  (:documentation "The base class of instructions that pop two arguments off the stack.")
  )

(defclass asgnu (two-arg-instruction)
  ()
  )

(defclass asgni (two-arg-instruction)
  ()
  )

(defclass bandu (two-arg-instruction)
  ()
  )

(defclass bandi (two-arg-instruction)
  ()
  )

(defclass addu (two-arg-instruction)
  ()
  )

(defclass addi (two-arg-instruction)
  ()
  )

(defclass subu (two-arg-instruction)
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
                                   (cmp-jump-instruction (list ':width (parse-integer (second tokens)) ':s-args (cddr tokens)))
                                   (jump-instruction (list ':s-args (rest tokens)))
                                   (one-stack-one-static-arg-instruction (list ':s-args (rest tokens)))
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

(defgeneric add-label (op idx labs bss base)
  )

(defmethod add-label ((op lcc-instruction) idx labs bss base)
  (declare (ignore op idx))
  (values labs bss base)
  )

(defmethod add-label ((op labelv) idx labs bss base)
  (with-slots (s-args) op
    (if bss
        (setf (gethash (first s-args) labs) (cons 'glob base))
        (setf (gethash (first s-args) labs) (cons 'labl idx))
        )
    (values labs bss base)
    )
  )

(defmethod add-label ((op bss) idx labs bss base)
  (values labs t base)
  )

(defmethod add-label ((op code) idx labs bss base)
  (values labs nil base)
  )

(defmethod add-label ((op skip) idx labs bss base)
  (with-slots (s-args) op
    (values labs bss (+ base (* 8 (parse-integer (first s-args)))))
    )
  )

(defun find-labels (instructions &optional (idx 0) (labs (make-hash-table :test 'equalp)) (bss nil) (base 1))
  "Find the index of all labels in the list of instructions, returning a hash table"
  (declare (type hash-table labs))
  (if (null instructions)
      labs
      (let ((op (skew-first instructions))
            )
        (multiple-value-bind (labs bss base) (add-label op idx labs bss base)
          (find-labels (skew-rest instructions) (1+ idx) labs bss base)
          )
        )
    )
  )

(defgeneric exec-instruction (op labels stack wires instrs targets arglist argsize icnt bss baseinit repeat)
  (:documentation "This generic method translates an LCC opcode into PCF2 opcodes.  The \"stack\" argument represents the state of the LCC stack at this program point.  The \"wires\" argument represents the location of free wires in the program, which roughly corresponds to the state of the LCC stack.

This method should return the updated stack, wires, and instructions.  Instructions should be given in reverse order, as this allows instructions to be appended to the instructions list efficiently.

The \"argbase\" parameter represents the list of arguments for the next function call.  When a call instruction is emitted, these arguments will be copied to the base of the next stack frame.  \"argsize\" is the number of arguments for the current function, which we use for addrfp instructions and as an offset for addrlp instructions.")
  )

(defun exec-instructions (ops)
  "Translate a list of LCC opcodes into PCF2 opcodes.  The opcodes should be formatted as a skew-binary list."
  (declare (optimize (debug 3) (speed 0)))
  (let ((lbls (find-labels ops))
        )
    (format t "s: ~A~%" (gethash "s" lbls))
    (let ((rvl 
           (skew-reduce (lambda (st op) 
                          (apply #'exec-instruction (append (list op lbls) st));stack wires instrs lbls targs argbase)
                          )
                        ops
                        ;; baseinit starts at 1, so that the global mux condition is not overwritten
                        (list nil 0 nil (make-queue) nil 0 0 nil 1 nil)
                        )
            )
          )
      (assert (>= (ninth rvl) 1))
      (format *error-output* "~&Initial base pointer: ~D~%" (ninth rvl))
      (cons (cons (make-instance 'label :str "pcfentry") (cons (make-instance 'initbase :base (ninth rvl)) (reverse (third rvl)))) (rest rvl))
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
     ;; (add-instrs (list (make-instance 'label :str (with-output-to-string (str) 
     ;;                                                (format str "~A~A" (class-name (class-of op)) icnt)
     ;;                                                str
     ;;                                                )))
       (list stack wires instrs targets arglist argsize (1+ icnt) bss baseinit nil)
;       )
     )
  )

(defmacro repeat-instr ()
  `(exec-instruction op labels stack wires instrs targets arglist argsize icnt bss baseinit t)
  )

(defmacro definstr (type &body body)
  "Instruction translator methods are defined with this macro.  It is
a convenience macro that ensures that the method takes the right
number of arguments."
  `(defmethod exec-instruction ((op ,type) labels stack wires instrs targets arglist argsize icnt bss baseinit repeat)
;     (declare (optimize (debug 3) (speed 0)))
     ;; (add-instrs (list (make-instance 'label :str (with-output-to-string (str)
     ;;                                                (format str "begin~A~A" (class-name (class-of op)) icnt)
     ;;                                                )
     ;;                                  )
     ;;                   )
     ,@body
;       )
     )
  )

(defmacro push-stack (stack width val &body body)
  (let ((valsym (gensym))
        )
    `(let ((,valsym ,val)
           )
       (declare (type list ,valsym))
       (let ((,stack (append ,valsym ,stack))
             )
         (let ((,stack (cons ,width ,stack))
               )
           ,@body
           )
         )
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

(defmacro add-target (target-label cnd-value glob-cnd &body body)
  `(let ((t-idx (cdr (gethash ,target-label labels nil)))
         )
     (assert t-idx)
     (let ((targets (enqueue (cons t-idx (list ,target-label ,cnd-value ,glob-cnd)) targets))
           )
       ,@body
       )
     )
  )

(defmacro get-target (target-label cnd-name &body body)
  (let ((next-targ (gensym))
        )
    `(let ((,next-targ (peek-queue targets))
           )
       (let ((targets (if (equalp ,target-label (car ,next-targ))
                          (dequeue targets)
                          targets)
               )
             (,cnd-name (if (equalp ,target-label (car ,next-targ))
                            (cdr ,next-targ)
                            nil
                            )
               )
             )
         ,@body
         )
       )
    )
  )

(defmacro branch-case (target-label forward-body backward-body)
  (let ((t-idx (gensym))
        )
    `(let ((,t-idx (cdr (gethash ,target-label labels nil)))
           )
       (assert ,t-idx)
       (if (>= ,t-idx icnt)
           ,forward-body
           ,backward-body
           )
       )
    )
  )

(defun eql-gates (arg1 arg2 dest tmp1 tmp2)
  (assert (= (length arg1) (length arg2)))
  (if (and arg1 arg2)
      (append (list (make-xnor tmp1 
                               (first arg1)
                               (first arg2))
                    (make-and tmp2 tmp1 dest)
                    (make-instance 'copy :dest dest :op1 tmp2 :op2 1)
                    )
              (eql-gates (rest arg1) (rest arg2) dest tmp1 tmp2)
              )
      )
  )

(definstr jumpv
  (labels ((list-crossed-conditions (queue targ &optional (ret nil))
             (declare (optimize (debug 3) (speed 0)))
             (if (queue-emptyp queue)
                 ret
                 (let ((ctarg (peek-queue queue))
                       (rest (dequeue queue))
                       )
                   (list-crossed-conditions rest targ (if (>= targ (cdr (gethash (car ctarg) labels)))
                                                          (cons (cadr ctarg) ret)
                                                          ret))
                   )
                 )
             )
           (emit-and-lst (wires res &optional (ret nil))
             (if (null wires)
                 ret
                 (emit-and-lst (rest wires)
                               res
                               (append (list (make-and res (first wires) res))
                                       ret)
                               )
                 )
             )
           )
    (with-slots (s-args) op
      (pop-arg stack targ
        (assert (and (= 1 (length targ))
                     (typep (first targ) 'string)
                                        ;                   (queue-emptyp targets)
                     )
                )
        (let ((targ (first targ))
              )
                                        ;        (print targets)
          (if (queue-emptyp targets)
              ;; If the priority queue is empty, then this jump should be unconditional
              (add-instrs (list (make-instance 'const :dest wires :op1 1)
                                (make-instance 'branch :cnd wires :targ targ)
                                )
                (let ((wires (1+ wires))
                      )
                  (close-instr)
                  )
                )
              (branch-case targ
                           (let ((crossed-conds (list-crossed-conditions targets (cdr (gethash targ labels))))
                                 )
                             (add-instrs
                                 (append
                                  (list
                                   (make-instance 'copy-indir :dest wires :op1 0 :op2 1)
                                   (make-instance 'const :dest (+ 2 wires) :op1 1)
                                   )
                                  (emit-and-lst crossed-conds (+ 2 wires))
                                  (list
                                   (make-not (+ 1 wires) (+ 2 wires))
                                   (make-and (+ 2 wires) wires (+ 1 wires))
                                   (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                   )
                                  )
                               (add-target targ (+ 1 wires) wires
                                 (let ((wires (+ 3 wires))
                                       )
                                   (close-instr)
                                   )
                                 )
                               )
                             )
                           (error 'unconditional-backwards-jump)
                           )
              )
          )
        )
      )
    )
  )

(definstr neu
;  (declare (optimize (speed 0) (debug 3)))
  (with-slots (s-args width) op
    (let ((width (* 8 width))
          (targ (first s-args))
          )
      (declare (type string targ))
      (pop-arg stack arg1
        (pop-arg stack arg2
          (assert (and (= width (length arg1) (length arg2))))
          (add-instrs (append 
                       (list (make-instance 'const :dest wires :op1 1))
                       ;; AND in the fall-through case
                       (eql-gates arg1 arg2 wires (1+ wires) (+ 2 wires))
                                        ;
                       )
            (branch-case targ
                         (progn
                           (add-instrs
                               (list 
                                (make-instance 'copy-indir :dest (1+ wires) :op1 0 :op2 1)
                                (make-and (+ 2 wires) wires (1+ wires))
                                (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                )
                             (add-target targ wires (1+ wires)
                               (let ((wires (+ 3 wires))
                                     )
                                 (close-instr)
                                 )
                               )
                             )
                           )
                         (progn
                           (add-instrs (list 
                                        (make-not (1+ wires) wires)
                                        (make-instance 'branch :cnd (1+ wires) :targ targ)
                                        )
                             (let ((wires (+ 2 wires))
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
    )
  )

(definstr equ
;  (declare (optimize (speed 0) (debug 3)))
  (with-slots (s-args width) op
    (let ((width (* 8 width))
          (targ (first s-args))
          )
      (declare (type string targ))
      (pop-arg stack arg1
        (pop-arg stack arg2
          (assert (and (= width (length arg1) (length arg2))))
          (add-instrs (append 
                       (list (make-instance 'const :dest wires :op1 1))
                       ;; AND in the fall-through case
                       (eql-gates arg1 arg2 wires (1+ wires) (+ 2 wires))
                                        ;
                       )
            (branch-case targ
                         (progn
                           (add-instrs
                               (list 
                                (make-not (1+ wires) wires)
                                (make-instance 'copy-indir :dest (+ 3 wires) :op1 0 :op2 1)
                                (make-and (+ 2 wires) (1+ wires) (+ 3 wires))
                                (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                )
                             (add-target targ (+ 1 wires) (+ 3 wires)
                               (let ((wires (+ 4 wires))
                                     )
                                 (close-instr)
                                 )
                               )
                             )
                           )
                         (progn
                           (add-instrs (list 
                                        (make-instance 'branch :cnd wires :targ targ)
                                        )
                             (let ((wires (+ 1 wires))
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
    )
  )

(defun unsigned-less-than (arg1 arg2 dest tmp2 tmp3 tmp4)
  (assert (= (length arg1) (length arg2)))
  (labels ((less-than (a1 a2)
             (list
              ;(make-xor tmp2 a1 dest)
              ;(make-xor tmp3 a2 dest)
              ;(make-and tmp4 tmp2 tmp3)
              ;(make-xor dest a2 tmp4)
              (make-xnor tmp2 a1 a2)
              (make-and tmp3 dest tmp2)
              (make-not tmp2 a1)
              (make-and tmp4 tmp2 a2)
              (make-or dest tmp3 tmp4)
              )
             )
           )
    (cons
     (make-instance 'const :dest dest :op1 0)
     (mapcan #'less-than arg1 arg2)
     )
    )
  )

(definstr ltu
;  (declare (optimize (speed 0) (debug 3)))
  (with-slots (s-args width) op
    (let ((width (* 8 width))
          (targ (first s-args))
          )
      (declare (type string targ))
      (pop-arg stack arg1
        (pop-arg stack arg2
          (assert (and (= width (length arg1) (length arg2))))
          (add-instrs (append 
                       ;; AND in the fall-through case
                       (unsigned-less-than arg2 arg1 wires (1+ wires) (+ 2 wires) (+ 3 wires))
                                        ;
                       )
            (branch-case targ
                         (progn
                           (add-instrs
                               (list 
                                (make-not (+ 3 wires) wires)
                                (make-instance 'copy-indir :dest (1+ wires) :op1 0 :op2 1)
                                (make-and (+ 2 wires) (+ 3 wires) (1+ wires))
                                (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                )
                             (add-target targ (+ 3 wires) (1+ wires)
                               (let ((wires (+ 4 wires))
                                     )
                                 (close-instr)
                                 )
                               )
                             )
                           )
                         (progn
                           (add-instrs (list 
                                        ;(make-not (1+ wires) wires)
                                        (make-instance 'branch :cnd wires :targ targ)
                                        )
                             (let ((wires (+ 4 wires))
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
    )
  )

(definstr leu
;  (declare (optimize (speed 0) (debug 3)))
  (with-slots (s-args width) op
    (let ((width (* 8 width))
          (targ (first s-args))
          )
      (declare (type string targ))
      (pop-arg stack arg1
        (pop-arg stack arg2
          (assert (and (= width (length arg1) (length arg2))))
          (add-instrs (append 
                       ;; AND in the fall-through case
                       (unsigned-less-than arg1 arg2 wires (1+ wires) (+ 2 wires) (+ 3 wires))
                       ;(list (make-not wires wires))
                                        ;
                       )
            (branch-case targ
                         (progn
                           ;; (print targ)
                           ;; (print (gethash targ labels))
                           ;; (print icnt)
                           (add-instrs
                               (list 
                                (make-instance 'copy-indir :dest (1+ wires) :op1 0 :op2 1)
                                (make-and (+ 2 wires) wires (1+ wires))
                                (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                )
                             (add-target targ wires (1+ wires)
                               (let ((wires (+ 4 wires))
                                     )
                                 (close-instr)
                                 )
                               )
                             )
                           )
                         (progn
                           (add-instrs (list 
                                        (make-not (1+ wires) wires)
                                        (make-instance 'branch :cnd (1+ wires) :targ targ)
                                        )
                             (let ((wires (+ 4 wires))
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
    )
  )

(definstr gtu
;  (declare (optimize (speed 0) (debug 3)))
  (with-slots (s-args width) op
    (let ((width (* 8 width))
          (targ (first s-args))
          )
      (declare (type string targ))
      (pop-arg stack arg1
        (pop-arg stack arg2
          (assert (and (= width (length arg1) (length arg2))))
          (add-instrs (append 
                       ;; AND in the fall-through case
                       (unsigned-less-than arg1 arg2 wires (1+ wires) (+ 2 wires) (+ 3 wires))
                       ;(list (make-not wires wires))
                                        ;
                       )
            (branch-case targ
                         (progn
                           ;; (print targ)
                           ;; (print (gethash targ labels))
                           ;; (print icnt)
                           (add-instrs
                               (list 
                                (make-not (+ 3 wires) wires)
                                (make-instance 'copy-indir :dest (1+ wires) :op1 0 :op2 1)
                                (make-and (+ 2 wires) (+ 3 wires) (1+ wires))
                                (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                )
                             (add-target targ (+ 3 wires) (1+ wires)
                               (let ((wires (+ 4 wires))
                                     )
                                 (close-instr)
                                 )
                               )
                             )
                           )
                         (progn
                           (add-instrs (list 
                                        ;(make-not (1+ wires) wires)
                                        (make-instance 'branch :cnd wires :targ targ)
                                        )
                             (let ((wires (+ 1 wires))
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
    )
  )

(definstr geu
;  (declare (optimize (speed 0) (debug 3)))
  (with-slots (s-args width) op
    (let ((width (* 8 width))
          (targ (first s-args))
          )
      (declare (type string targ))
      (pop-arg stack arg1
        (pop-arg stack arg2
          (assert (and (= width (length arg1) (length arg2))))
          (add-instrs (append 
                       ;; AND in the fall-through case
                       (unsigned-less-than arg2 arg1 wires (1+ wires) (+ 2 wires) (+ 3 wires))
                       ;(list (make-not wires wires))
                                        ;
                       )
            (branch-case targ
                         (progn
                           ;; (print targ)
                           ;; (print (gethash targ labels))
                           ;; (print icnt)
                           (add-instrs
                               (list 
                                ;(make-not wires wires)
                                (make-instance 'copy-indir :dest (1+ wires) :op1 0 :op2 1)
                                (make-and (+ 2 wires) wires (1+ wires))
                                (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                )
                             (add-target targ wires (1+ wires)
                               (let ((wires (+ 3 wires))
                                     )
                                 (close-instr)
                                 )
                               )
                             )
                           )
                         (progn
                           (add-instrs (list 
                                        (make-not (1+ wires) wires)
                                        (make-instance 'branch :cnd (1+ wires) :targ targ)
                                        )
                             (let ((wires (+ 2 wires))
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
    )
  )

(defmacro intcnst ()
  `(with-slots (s-args) op
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

(definstr cnstu
  (intcnst)
  )

(definstr cnsti
  (intcnst)
  )

(definstr addp
  (pop-arg stack arg1
    (pop-arg stack arg2
      (format t "addp: ~A ~A ~%" arg1 arg2)
      (cond
        ((= (length arg1) (length arg2) 1)
         (add-instrs (list
                      (make-instance 'pcf2-bc:add :dest wires :op1 (first arg1) :op2 (first arg2))
                      )
           (push-stack stack 1 (list wires)
             (let ((wires (1+ wires))
                   )
               (close-instr)
               )
             )
           )
         )
        ((= (length arg1) 1)
         (add-instrs (list (make-instance 'join :dest wires :op1 arg2)
                           (make-instance 'const :dest (1+ wires) :op1 8)
                           (make-instance 'pcf2-bc:mul :dest wires :op1 wires :op2 (1+ wires))
                           (make-instance 'pcf2-bc:add :dest (1+ wires) :op1 (first arg1) :op2 wires)
                           )
           (push-stack stack 1 (list (1+ wires))
             (let ((wires (+ 2 wires))
                   )
               (close-instr)
               )
             )
           )
         )
        ((= (length arg2) 1)
         (add-instrs (list (make-instance 'join :dest wires :op1 arg1)
                           (make-instance 'const :dest (1+ wires) :op1 8)
                           (make-instance 'pcf2-bc:mul :dest wires :op1 wires :op2 (1+ wires))
                           (make-instance 'pcf2-bc:add :dest (1+ wires) :op1 (first arg2) :op2 wires)
                           )
           (push-stack stack 1 (list (1+ wires))
             (let ((wires (+ 2 wires))
                   )
               (close-instr)
               )
             )
           )
         )
        (t
         (add-instrs (list (make-instance 'join :dest wires :op1 arg1)
                           (make-instance 'const :dest (1+ wires) :op1 8)
                           (make-instance 'pcf2-bc:mul :dest wires :op1 wires :op2 (1+ wires))
                           (make-instance 'join :dest (1+ wires) :op1 arg2)
                           (make-instance 'const :dest (+ 2 wires) :op1 8)
                           (make-instance 'pcf2-bc:mul :dest (1+ wires) :op1 (1+ wires) :op2 (+ 2 wires))
                           (make-instance 'pcf2-bc:add :dest (+ 2 wires) :op1 wires :op2 (1+ wires))
                           )
           (push-stack stack 1 (list (+ 2 wires))
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

(definstr addi
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

(definstr subu
  (with-slots (width) op
    (let* ((width (* 8 width))
           (rwires (loop for i from wires to (+ wires width -1) collect i))
           )
      (pop-arg stack arg2
        (pop-arg stack arg1
          (push-stack stack width rwires
            (add-instrs (subtractor-chain
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

(defmacro right-or-left-shift (zero-concat &body body)
  `(with-slots (width) op
    (let ((width (* 8 width))
          )
      (pop-arg stack amount
        (pop-arg stack val
          (let ((rwires (loop for i from wires to (+ wires width -1) collect i))
                (rwires* (loop for i from (+ wires width) to (+ wires (* 2 width) -1) collect i))
                )
            (assert (= (length rwires) (length rwires*) width))
            (add-instrs (append
                         (list 
                          (make-instance 'const :dest (+ wires (* 2 width)) :op1 0)
                          (make-instance 'copy :dest wires :op1 (first val) :op2 width))
                         (mapcan (lambda (x y)
                                   (let ((shifted-val ,zero-concat)
                                         )
                                     (assert (= (length shifted-val) width))
                                     (append (mux rwires 
                                                  shifted-val 
                                                  rwires* 
                                                  x 
                                                  (+ wires (* 2 width) 1) 
                                                  (+ wires (* 2 width) 2))
                                             (list (make-instance 'copy 
                                                                  :dest (first rwires) 
                                                                  :op1 (first rwires*) 
                                                                  :op2 width))
                                             )
                                     )
                                   ) 
                                 (subseq amount 0 (1+ (floor (log width 2))) )
                                 (loop for i from 0 to (floor (log width 2)) collect i)
                                 )
                         )
              (let ((wires (+ wires (* 2 width) 3))
                    )
                (push-stack stack width rwires*
                  ,@body
                  )
                )
              )
            )
          )
        )
      )
    )
  )


(definstr lshu
  (right-or-left-shift 
      (append
       (loop for i from 0 to (1- (expt 2 y)) collect (+ wires (* 2 width)))
       (subseq rwires 0 (- width (expt 2 y)))
       )
    (close-instr)
    )
  )

(definstr rshi
  (right-or-left-shift
      (append
       (subseq rwires (expt 2 y) width)
       (loop for i from 0 to (1- (expt 2 y)) collect (+ wires (* 2 width)))
       )
    (close-instr)
    )
  )

(definstr rshu
  (right-or-left-shift
      (append
       (subseq rwires (expt 2 y) width)
       (loop for i from 0 to (1- (expt 2 y)) collect (+ wires (* 2 width)))
       )
    (close-instr)
    )
  )

(definstr boru
  (with-slots (width) op
    (let ((width (* 8 width))
          )
      (let ((rwires (loop for i from wires to (+ wires width -1) collect i))
            )
        (assert (= width (length rwires)))
        (pop-arg stack arg1
          (pop-arg stack arg2
            (push-stack stack width rwires
              (add-instrs (or-chain arg1 arg2 rwires)
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
  )

(definstr bxoru
  (with-slots (width) op
    (let ((width (* 8 width))
          )
      (let ((rwires (loop for i from wires to (+ wires width -1) collect i))
            )
        (assert (= width (length rwires)))
        (pop-arg stack arg1
          (pop-arg stack arg2
            (push-stack stack width rwires
              (add-instrs (xor-chain arg1 arg2 rwires)
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

(definstr bandi
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
                 (queue-emptyp targets)
                 )
            )
    (format t "callv ~A with newbase ~A with args: ~A~%" fname wires arglist)
    (let ((i 0)) (add-instrs 
                  (append (print
                           (loop for arg in (reverse arglist) collect
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
                          (progn (print (+ wires i)) nil)
                          (list (make-instance 'call :newbase (+ i wires) :fname (first fname)))
                          )
                   (let ((arglist nil)
                         (wires (+ i wires))
                         )
                     (close-instr)
                     )
                   )
         )    
    )
  )

(definstr callu
  (with-slots (width) op
    (assert (queue-emptyp targets))
    (let ((width (* 8 width))
          )
      (pop-arg stack fname
        (let ((i 0)) 
          (add-instrs  
              (append (loop for arg in (reverse arglist) collect
                           (prog1
                               (make-instance 'copy 
                                              :dest (+ wires i)
                                              :op1 (first (arg-loc arg))
                                              :op2 (arg-len arg))
                             (incf i (arg-len arg))
                             )
                           )
                      (list (make-instance 'call :newbase (+ i wires) :fname (first fname)))
                      )
            (push-stack stack width (loop for j from (+ i wires) to (+ wires width i -1) collect j)
              (let ((wires (+ wires width i 1))
                    (arglist nil)
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

(definstr calli
  (with-slots (width) op
    (assert (queue-emptyp targets))
    (let ((width (* 8 width))
          )
      (pop-arg stack fname
        (let ((i 0)) 
          (add-instrs  
              (append (loop for arg in (reverse arglist) collect
                           (prog1
                               (make-instance 'copy 
                                              :dest (+ wires i)
                                              :op1 (first (arg-loc arg))
                                              :op2 (arg-len arg))
                             (incf i (arg-len arg))
                             )
                           )
                      (list (make-instance 'call :newbase (+ i wires) :fname (first fname)))
                      )
            (push-stack stack width (loop for j from (+ i wires) to (+ wires width i -1) collect j)
              (let ((wires (+ wires width i 1))
                    (arglist nil)
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

(definstr reti
  ;; We must copy the value from the stack to the very beginning of
  ;; the stack frame, which is where return values are stored (see
  ;; above).
  (with-slots (width) op
    (pop-arg stack arg
      (add-instrs (loop for j in arg for i from 0 collect
                       (make-xnor i j j)
                       )
        (close-instr)
        )
      )
    )
  )

(definstr retu
  ;; We must copy the value from the stack to the very beginning of
  ;; the stack frame, which is where return values are stored (see
  ;; above).
  (with-slots (width) op
    (pop-arg stack arg
      (add-instrs (loop for j in arg for i from 0 collect
                       (make-xnor i j j)
                       )
        (close-instr)
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

(definstr argp
  (with-slots (width) op
    (pop-arg stack arg
      (assert (= 1 (length arg)))
      (let ((arglist (append arglist (list (make-arg :len (* 8 width) :loc arg))))
            )
        (close-instr)
        )
      )
    )
  )

(definstr addrgp
;  (declare (optimize (debug 3) (speed 0)))
  (with-slots (s-args) op
    (let ((addr* (string-tokenizer:tokenize (second s-args) #\+))
          )
      (let ((a (gethash (first addr*) labels nil))
            )
        (declare (type (or null (cons symbol (integer 1))) a))
        (format t "~&Address for ~A is ~A (at ~A)~%" (second s-args) a wires)
        (let ((addr (if a
                        (if (equalp (car a) 'glob)
                            (+ (cdr a) (if (second addr*) (parse-integer (second addr*)) 0))
                            (second s-args)
                            );(gethash (second s-args) labels nil)
                        (second s-args)
                        )
                )
              )
          (add-instrs (if (equalp (car a) 'glob)
                          (list (make-instance 'const :dest wires :op1 (+ (if (second addr*) (parse-integer (second addr*)) 0) (cdr a))))
                          )

            (push-stack stack 1 (if (equalp (car a) 'glob) 
                                    (list wires) 
                                    (list addr))
              (let ((wires (if (equalp (car a) 'glob) (1+ wires) wires))
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


;; Below we offset the addresses by 1 because the base of the stack
;; frame will always have a pointer to the global condition wire in
;; position 0
(definstr addrlp
  (with-slots (s-args) op
    (let ((addr (let ((nums (string-tokenizer:tokenize (second s-args) #\+))
                      )
                  ;(parse-integer (second s-args)))
                  (if (< 2 (length nums))
                      (parse-integer (first nums))
                      (reduce #'(lambda (x y) (+ (parse-integer y) x)) nums :initial-value 0)
                      )
                  )
            )
          )
      (push-stack stack 1 (list wires)
        (add-instrs (list 
                     (make-instance 'const :dest wires :op1 (+ 1 (* 8 addr)))
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

(definstr addrfp
  (with-slots (s-args) op
    (let ((width (* 8 (parse-integer (first s-args))))
          (addr (parse-integer (second s-args)))
          )
      (format t "addrfp: ~A (to ~A)~%" (- (* -8 addr) width) wires)
      (add-instrs (list (make-instance 'const :dest wires :op1 (- (* -8 addr) width))
                        (make-instance 'mkptr :dest wires)
                        )

        (push-stack stack 1 (list wires)
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

(definstr indiri
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

(definstr indirp
  (pop-arg stack ptr
    (format t "indir: ~A (to ~A)~%" ptr wires)
    (add-instrs (list (make-instance 'copy-indir :dest wires :op1 (first ptr) :op2 1))
      (push-stack stack 1 (list wires)
        (let ((wires (1+ wires))
              )
          (close-instr)
          )
        )
      )
    )
  )

(defun mux (olds news res cnd tmp1 tmp2)
  "Emit a chain of muxes for condition \"cnd\".  It should be safe for \"res\" to be equal to either \"olds\" or \"news\"."
  (assert (= (length olds) (length news) (length res)))
  (mapcan (lambda (x y r)
            (list
             (make-xor tmp1 x y)
             (make-and tmp2 tmp1 cnd)
             (make-xor r tmp2 x)
             )
            )
          olds news res)
  )

(defmacro asgn-mux (&body body)
  `(add-instrs 
       (if (not (queue-emptyp targets))
           (append 
            (list 
             (make-instance 'copy-indir :dest wires :op1 (first ptr) :op2 width)
             (make-instance 'copy-indir :dest (+ wires (* 2 width) 2) :op1 0 :op2 1))
            (mux (loop for i from 0 to (1- width) collect (+ i wires))
                 val
                 (loop for i from (+ width wires) to (+ wires (* 2 width) -1) collect i)
                 (+ wires (* 2 width) 2)
                 (+ wires (* 2 width))
                 (+ wires (* 2 width) 1)
                 )
            (list (make-instance 'indir-copy :dest (first ptr) :op1 (+ width wires) :op2 width))
            )
           (list (make-instance 'indir-copy :dest (first ptr) :op1 (first val) :op2 width))
           )
     (let ((wires (+ wires (* 2 width) 3))
           )
       ,@body
       )
     )
  )

(definstr asgnu
  (with-slots (width) op
    (let* ((width (* 8 width))
           )
      (pop-arg stack val
        (pop-arg stack ptr
          (assert (and (> (first ptr) 0) (= 1 (length ptr))))
          ;; TODO:  Should add muxes here
          ;;
          ;; To deal with pointers that might not have values that can
          ;; be determined at compile time, the mux should first copy
          ;; the old value to a temporary location, then mux with the
          ;; new value, then assign to the location.
          (asgn-mux
           (close-instr)
           )
          )
        )
      )
    )
  )

(definstr asgni
  (with-slots (width) op
    (let* ((width (* 8 width))
           )
      (pop-arg stack val
        (pop-arg stack ptr
          (assert (and (> (first ptr) 0) (= 1 (length ptr))))
          ;; TODO:  Should add muxes here
          ;;
          ;; To deal with pointers that might not have values that can
          ;; be determined at compile time, the mux should first copy
          ;; the old value to a temporary location, then mux with the
          ;; new value, then assign to the location.
          (asgn-mux
           (close-instr)
           )
          )
        )
      )
    )
  )


(definstr asgnp
  (pop-arg stack val
    (pop-arg stack ptr
      (assert (and (= 1 (length ptr) (length val)) 
                   ;(queue-emptyp targets) 
                   (> (first val) 0)))
                                        ;(asgn-mux
      (format t "asgnp ~A to ~A~%" val ptr)
      (add-instrs (list (make-instance 'indir-copy :dest (first ptr) :op1 (first val) :op2 1))
        (close-instr)
        )
      )
                                        ;)
    )
  )

(definstr proc
  (with-slots (s-args) op
    (assert (queue-emptyp targets))
    (let ((local-size (parse-integer (second s-args)))
          (args-size (parse-integer (third s-args)))
          )
      (let ((wires (+ 1 wires (* 8 local-size)))
            (argsize (* 8 args-size))
            (arglist nil)
            )
        ;; We add instructions to set up the pointer to the global position wire
        ;;
        ;; We need no 'mkptr here, because we need an absolute pointer to wire 0
        (add-instrs (list 
                     (make-instance 'label :str (first s-args))
                     (make-instance 'clear :localsize (* 8 local-size))
                     )
          (close-instr)
          )
        )
      )
    )
  )

(definstr endproc
  (assert (null stack))
  (assert (queue-emptyp targets))
  (add-instrs (list (make-instance 'ret :value wires))
    (let ((wires 0)
          )
      (close-instr)
      )
    )
  )

(definstr import
  (close-instr)
  )

(definstr export
  (close-instr)
  )

(definstr code
  (let ((bss nil))
    (close-instr)
    )
  )

(definstr bss
  (let ((bss t))
    (close-instr)
    )
  )

(definstr skip
;  (declare (optimize (debug 3) (speed 0)))
  (with-slots (s-args) op
    (let ((baseinit (+ baseinit (* 8 (parse-integer (first s-args)))))
          )
      (close-instr)
      )
    )
  )

(definstr align
  (close-instr)
  )

(definstr cvui
  (close-instr)
  )

(definstr cviu
  (close-instr)
  )

(definstr labelv
  (declare (optimize (debug 3) (speed 0)))
;  (format t "~&labelv Targets: ~A~%" targets) 
;  (let ((not-empty (not (queue-emptyp targets)))
;        )
  (with-slots (s-args) op
    (if (not bss)
        (get-target (first s-args) cnd
                                        ;            (if not-empty (assert cnd))
          (add-instrs (append
                       (if (not repeat) (list (make-instance 'pcf2-bc:label :str (first s-args))))
                       (if cnd ;not-empty
                           (list (make-instance 'pcf2-bc:copy-indir :dest wires :op1 0 :op2 1)
                                 (make-not (1+ wires) (first cnd))
                                 ;; globcnd <- globcnd + (oldglobcnd)*(~cnd)
                                 (make-and (1+ wires) (second cnd) (1+ wires))
                                 (make-or (+ 2 wires) wires (1+ wires))
                                 (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                 )
                           )
                       )
            (let ((wires (+ wires (if cnd ;not-empty 
                                      3 
                                      0)))
                  )
              (if (equalp (first s-args) (car (peek-queue targets)))
                  ;; There are no other conditional assignments for this target
                  (repeat-instr)
                  (close-instr)
                                        ;(assert nil)
                  )
              )
            )
          )
        (close-instr)
        )
    )
  )