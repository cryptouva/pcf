(defpackage :lcc-bc (:use 
                     :string-tokenizer
                     :skew-list
                     :common-lisp
                     #+sbcl :sb-mop #+cmu :mop)
            (:export
             width
             parse-instruction
             read-instructions
             lcc-instruction
             code
             bss
             align
             skip
             labelv
             export
             import
             proc
             endproc
             addrlp
             addrfp
             addrgp
             indirp
             asgnp
             addp
             argp
             lshu
             rshu
             rshi
             bxoru
             boru
             bandu
             cnsti
             cnstu
             one-stack-one-static-arg-instruction
             cvui
             cviu
             stack-arg-instruction
             one-arg-instruction
             cnd-jump-instruction
             jump-instruction
             jumpv
             cmp-jump-instruction
             ltu
             leu
             gtu
             geu
             neu
             equ
             callv
             callu
             calli
             retu
             reti
             indiru
             indiri
             two-arg-instruction
             asgnu
             asgni
             asgnp
             bandi
             addu
             addi
             subu
             mulu
             argi
             argu
             read-instructions
             s-args
             )
            (:shadow
             export
             import)
            )
(in-package :lcc-bc)

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

(defclass mulu (two-arg-instruction)
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
