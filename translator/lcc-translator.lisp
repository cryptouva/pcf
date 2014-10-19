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
                             :setmap
                             :lcc-bc
                             :lcc-const
                             :utils
                             #+sbcl :sb-mop #+cmu :mop)
            (:export
             exec-instructions
	     mux
	     )
            (:shadowing-import-from :lcc-bc export import))
(in-package :lcc-translator)
(use-package :pcf2-bc)

;;;
;;; LABELS
;;;

(defgeneric add-label (op idx labs bss base)
  (:documentation "add-label builds the labels hash table and allocates space for global variables. the \"code\" and \"base\" instructions switch on and off whether we append to the global or label section when encountering labels. if the former is true, the skip opcode will allocate space for global variables. if the latter is true, most lcc instructions will not alter the state, but labels will be interned.
IDX is the index of a defined label - it will increment per instruction in order to provide the instruction's location within a program;LABS is the hash table that maps labels to their locations; in tandem, these two identify where in the list of opcodes each label is, and the index is how we find the label in the random access list.
BSS is t/nil depending on if we're in the data section; BASE is the base pointer to the next spot in memory for allocating global variables"
		  )
  )


(defmethod add-label ((op lcc-instruction) idx labs bss base)
  (declare (ignore op idx))
  (values labs bss base)
  )

;; *****
;; first s-args will be a static variable describing either the code block or the global variable name
(defmethod add-label ((op labelv) idx labs bss base)
  (with-slots (s-args) op
    (if bss
        (setf (gethash (first s-args) labs) (cons 'glob base)) ;; cons 'GLOB with the base to indicate a global variable
        (setf (gethash (first s-args) labs) (cons 'labl idx))  ;; cons 'LABL with the index to indicate a code block
        )
    (values labs bss base)
    )
  )

;; sets "bss" to true so that we know we're now in the data section
(defmethod add-label ((op bss) idx labs bss base)
  (values labs t base)
  )

(defmethod add-label ((op code) idx labs bss base)
  (values labs nil base)
  )

;; allocates space for a variable in the data section
;; (skip always in data section)
(defmethod add-label ((op skip) idx labs bss base)
  (with-slots (s-args) op ; get static args
    (values labs bss (+ base (* *byte-width* (parse-integer (first s-args))))) ; store #bits (not bytes) in base, for we will need a wire per bit
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

;;;
;;; END LABELS
;;;

;;;
;;; EXEC INSTRUCTIONS
;;; AND CONVENIENCE MACROS
;;;

(defun exec-instructions (ops) 
  "Translate a list of LCC opcodes into PCF2 opcodes.  The opcodes should be formatted as a skew-binary list.

As a first step, this function uses the constant propagation analysis
framework.  This is necessary for certain instructions to handle
conditional calls to functions, and can also improve code generation
to some extent."
  (declare (optimize (debug 3) (speed 0)))
  (let ((lbls (find-labels ops))
        (cnsts (const-dataflow-funs (list-skew ops)))
        )
    (setf (gethash "$$$END$$$" lbls) (cons 'labl 1000000)) ; last label arbitrarily set to 1mil
    (let ((rvl 
           (skew-reduce (lambda (st op) 
                          (assert (not (null st)))
                          (aif (apply #'exec-instruction (append (list op lbls) st))
                               it
                               (error "State is null -- this is a bug")
                               )
                                        ;stack wires instrs lbls targs argbase)
                          )
                        ops
                        ;; baseinit starts at 1, so that the global mux condition is not overwritten
                        (list 
                         nil
                         0 
                         nil 
                         (enqueue (cons 1000000
                                        (make-branch-target :label "$$$END$$$"
                                                            :cnd-wire 0
                                                            :glob-cnd 0
                                                            :mux-list (map-empty)
                                                            )) (make-queue))
                         nil 
                         0 
                         0 
                         nil 
                         1 
                         nil
                         0
                         (cons cnsts nil))
                        )
            )
          )
      (assert (>= (ninth rvl) 1))
      (format *error-output* "~&Initial base pointer: ~D~%" (ninth rvl))
      (cons
       (cons (make-instance 'label :str "pcfentry")
             (cons (make-instance 'initbase :base (ninth rvl)) 
                   (reverse (third rvl))))
       (rest rvl))
      )
    )
  )

#|
exec-instruction, as its name implies, executes an instruction. Explanation of the variables:
op: the next op code to execute
labels: a hash table that maps labels to their locations 
stack: holds the state of the program's execution, generally pushing and popping a constant and a group of wires - the constant describing the number of wires - that provide the inputs and outputs of our computations
wires: an integer that contains a count of the number of wires in use. New wires are allocated for new variables, and are deallocated when variables go out of scope (with the with-tmp-wires macro). At this point, they are not completely re-allocated in a meaningful way that permits reuse -- this is a possibility for future optimization -- but some are.
instrs: the list of PCF2 instructions (which are added in reverse)
targets: a queue of branch targets, which consist of labels in the labels table 
arglist: contains list of arguments to a function
argsize: 
icnt: instruction count
bss: true if we are in the data section, false if not
baseinit: 
repeat: true if the instruction should be repeated
iidx: instruction index (reset to 0 at each proc)
cnsts: a map of constants
|#

;; We might want to make that last argument be a map containing all
;; dataflow results, but for now just the one...
(defgeneric exec-instruction (op labels stack wires instrs targets arglist argsize icnt bss baseinit repeat iidx cnsts)
  (:documentation "This generic method translates an LCC opcode into PCF2 opcodes.  The \"stack\" argument represents the state of the LCC stack at this program point.  The \"wires\" argument represents the location of free wires in the program, which roughly corresponds to the state of the LCC stack.

This method should return the updated stack, wires, and instructions.  Instructions should be given in reverse order, as this allows instructions to be appended to the instructions list efficiently.

The \"argbase\" parameter represents the list of arguments for the next function call.  When a call instruction is emitted, these arguments will be copied to the base of the next stack frame.  \"argsize\" is the number of arguments for the current function, which we use for addrfp instructions and as an offset for addrlp instructions.")
  )


(defmacro close-instr ()
  "This macro will finalize an instruction translator method.  It is a
convenience macro that ensures the returned list has the right length."
  `(progn
     ;; (add-instrs (list (make-instance 'label :str (with-output-to-string (str) 
     ;;                                                (format str "~A~A" (class-name (class-of op)) icnt)
     ;;                                                str
     ;;                                                )))
       (list stack wires instrs targets arglist argsize (1+ icnt) bss baseinit nil (1+ iidx) cnsts)
;       )
     )
  )

(defmacro repeat-instr ()
  `(exec-instruction op labels stack wires instrs targets arglist argsize icnt bss baseinit t iidx cnsts)
  )

(defmacro definstr (type &body body)
  "Instruction translator methods are defined with this macro.  It is
a convenience macro that ensures that the method takes the right
number of arguments."
  `(defmethod exec-instruction ((op ,type) labels stack wires instrs targets arglist argsize icnt bss baseinit repeat iidx cnsts)
     (declare (optimize (debug 3) (speed 0)))
     ;; (add-instrs (list (make-instance 'label :str (with-output-to-string (str)
     ;;                                                (format str "begin~A~A" (class-name (class-of op)) icnt)
     ;;                                                )
     ;;                                  )
     ;;                   )
     (aif (locally ,@body)
          it
          (error "State is null -- this is a bug")
          )
;       )
     )
  )


(defmacro push-stack (stack width val &body body)
  "pushes two things onto the stack:
  1) a value (val) which is a list of wires
  2) the length of said list"
  (let ((valsym (gensym)))
    `(let ((,valsym ,val))
       (declare (type list ,valsym))
       (let ((,stack (append ,valsym ,stack))) ; append the list of wires to the front of the stack
         (let ((,stack (cons ,width ,stack))) ; append a parameter describing the number of wires appended to the stack
	   (assert (= (length ,valsym) ,width))
           ,@body
           )))))


(defmacro pop-arg (stack argname &body body)
  "pops off how many wires the argument will be, then pops the wires off the stack, assigning the list to argname"
  (let ((wsym (gensym)))
    `(let* ((,wsym (first ,stack)) ; the first argument is how many wires to pop off next
            (,stack (rest ,stack)) ; discard first value
            (,argname (subseq ,stack 0 ,wsym)) ; the list is the 0-,wsm spots on the stack
            (,stack (nthcdr ,wsym ,stack)) ; nthcdr the stack to pop
            )
       ,@body
       )))

(defmacro add-instrs (inslst &body body)
  (let ((inslstsym (gensym))
        )
    `(let* ((,inslstsym ,inslst)
            (instrs (append (reverse ,inslstsym) instrs))
            )
       (mapc #'(lambda (x) (assert (typep x 'pcf2-bc:instruction))) ,inslstsym) ; verify everything we're adding is a valid PCF2 instruction
       ,@body
       )
    )
  )

;; allocate wires programmatically
(defmacro with-temp-wires (sym n &body body)
  ;;(declare (indent defun))
  `(let ((,sym 
	  (if (eql 1 ,n)
	      wires
	      (loop for i from wires to (+ wires ,n -1) collect i)))
         (wires (+ wires ,n)))
     ,@body))

;;;
;;; END CONVENIENCE MACROS
;;; 

;;; 
;;; START BRANCHING
;;;

(defstruct branch-target
  (label "" :type string)
  (cnd-wire)
  (glob-cnd)
  (mux-list)
  (:documentation "Information needed for emitted muxes when a branch target is reached.")
  )

(defmacro add-target (target-label cnd-value glob-cnd &body body)
  `(let ((t-idx (cdr (gethash ,target-label labels nil)))
         )
     (assert t-idx)
     (let ((targets (enqueue (cons t-idx (make-branch-target
                                          :label ,target-label
                                          :cnd-wire ,cnd-value
                                          :glob-cnd ,glob-cnd
                                          :mux-list (map-empty))) targets))
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
       (declare (type (or null branch-target) ,next-targ))
       (let ((targets (if (and ,next-targ (string= ,target-label (branch-target-label ,next-targ)))
                          (dequeue targets) 
                          targets) ;; is the only case where this happens when targets is empty?
               )
             (,cnd-name (if (and ,next-targ (string= ,target-label (branch-target-label ,next-targ)))
                            ,next-targ
                            nil
                            )
               )
             )
         ,@body
         )
       )
    )
  )

;;; target-label is where we're going
;;; forward-body is ?
;;; backward-body is ?
(defmacro branch-case (target-label forward-body backward-body)
  (let ((t-idx (gensym))
        (cnd-sym (gensym)))
    `(let ((,t-idx (cdr (gethash ,target-label labels nil))) ; default to nil in order to fail assertion on next line if label does not exist
           )
       (assert ,t-idx)
       (if (>= ,t-idx icnt) ; if t-idx is greater than the current instruction count, we're jumping forward and this is easy
           ,forward-body
           (add-instrs (append
                        (reverse
                         (map-reduce
                          (lambda (st k x)
                            (declare (type mux-item x) (ignore k))
                            (let* ((width (mux-item-width x))
                                   (tmp1 (+ wires (* 2 width)))
                                   (tmp2 (+ wires (* 2 width) 1))
                                   (mux-dest (loop for i from (+ width wires) to (+ wires (* 2 width) -1) collect i))
                                   (new-copy-wires (loop for i from wires to (+ width wires -1) collect i)))
                              (append (reverse
                                       (append
                                        (list (make-instance 'copy-indir
                                                             :dest (car new-copy-wires)
                                                             :op1 (mux-item-address x)
                                                             :op2 width))
                                        (mux 
                                         (mux-item-old-copy x)
                                         new-copy-wires;(mux-item-address x)
                                         mux-dest
                                         (mux-item-cnd-wire x)
                                         tmp1
                                         tmp2)
                                        (list (make-instance 'indir-copy 
							     :dest (the integer (mux-item-address x))
							     :op1 (car mux-dest)
							     :op2 width))))
				      st)))
                          (let ((,cnd-sym (peek-queue targets)))
                            (branch-target-mux-list ,cnd-sym))
                          nil)))
             (let ((targets 
                    (let ((,cnd-sym (peek-queue targets)))
                      (update-queue-min targets
                                        (make-branch-target
                                         :label (branch-target-label ,cnd-sym)
                                         :cnd-wire (branch-target-cnd-wire ,cnd-sym)
                                         :glob-cnd (branch-target-glob-cnd ,cnd-sym)
                                         :mux-list (map-empty))))))
               ,backward-body))))))

;;;
;;; END BRANCHING
;;;

;;;
;;; START MUXES
;;; 

(defun mux (olds news res cnd tmp1 tmp2)
  "Emit a chain of muxes for condition \"cnd\".  It should be safe for \"res\" to be equal to either \"olds\" or \"news\"."
  (assert (= (length olds) (length news) (length res)))
  (append
  (mapcan (lambda (x y r)
            (list
             (make-xor tmp1 x y)
             (make-and tmp2 tmp1 cnd)
             (make-xor r tmp2 x)
             )
            )
          olds news res))
  )

(defmacro asgn-mux (&body body)
  ;; We need to *always* emit the muxes, so that conditional calls to
  ;; functions will work.
  (let ((cnstsym (gensym))
        )
    (format *error-output* 
            "~&TODO: Need to implement the proper behavior for
    assignments that occur outside of conditional branches, to
    properly support conditional function calls.  Currently
    this just does the assignment unconditionally, which
    results in side effects that are incorrect.

    We should able to use the constant propagation dataflow framework
    for this, but for some reason that is not working....~%"
          )

    `(let ((,cnstsym (cadr (cdr (map-find (write-to-string iidx) (cadr (cdr cnsts))))))
           )
       (format *error-output* "asgn-mux: ~A, iidx: ~D~%" ,cnstsym iidx)
       (if (and (or (queue-emptyp targets)
                    (string= (branch-target-label (peek-queue targets)) "$$$END$$$"))
                ; TODO: Fix this
                ;(integerp ,cnstsym)
                )
           ,(progn
             `(progn
                (add-instrs (list (make-instance 'indir-copy :dest (the integer (first ptr)) :op1 (car val) :op2 width))
                ,@body)
                )
             )
           (add-instrs
               (list
                (make-instance 'copy-indir :dest wires :op1 (first ptr) :op2 width)
                (make-instance 'indir-copy :dest (the integer (first ptr)) :op1 (car val) :op2 width)
                (make-instance 'copy-indir :dest (+ wires width) :op1 0 :op2 1)
                )
             (let* ((mtarget (peek-queue targets))
                    (targets 
                     (update-queue-min targets
                                       (make-branch-target
                                        :label (branch-target-label mtarget)
                                        :cnd-wire (branch-target-cnd-wire mtarget)
                                        :glob-cnd (branch-target-glob-cnd mtarget)
                                        :mux-list (map-insert (first ptr)
                                                              (make-mux-item :address (first ptr)
                                                                             :width width
                                                                             :old-copy (loop for i from 0 to (1- width) collect (+ i wires))
                                                                             :cnd-wire (+ wires width))
                                                              (branch-target-mux-list mtarget)
                                                              )
                                        )
                                       )
                      )
                    )
               (let ((wires (+ wires width 1))
                     )
                 ,@body
                 )
               )
             )
           )
         )
    )
  ;; (add-instrs 
  ;;          (if (not (queue-emptyp targets))
  ;;          (append 
  ;;           (list 
  ;;            (make-instance 'copy-indir :dest wires :op1 (first ptr) :op2 width)
  ;;            (make-instance 'copy-indir :dest (+ wires (* 2 width) 2) :op1 0 :op2 1))
  ;;           (mux (loop for i from 0 to (1- width) collect (+ i wires))
  ;;                val
  ;;                (loop for i from (+ width wires) to (+ wires (* 2 width) -1) collect i)
  ;;                (+ wires (* 2 width) 2)
  ;;                (+ wires (* 2 width))
  ;;                (+ wires (* 2 width) 1)
  ;;                )
  ;;           (list (make-instance 'indir-copy :dest (the integer (first ptr)) :op1 (+ width wires) :op2 width))
  ;;           )
  ;;          (list (make-instance 'indir-copy :dest (the integer (first ptr)) :op1 (first val) :op2 width))
  ;;          )
  ;;        (let ((wires (+ wires (* 2 width) 3))
  ;;              )
  ;;          ,@body
  ;;          )
  ;;        )
  ;;)
  )

(defstruct mux-item
  (address  0 :type integer)
  (width    0 :type (integer 0))
  (old-copy nil :type list)
  (cnd-wire 0 :type integer)
  (:documentation "\"address\" is the address the muxed value should
  be placed in; \"old-copy\" is the address where the previous value
  in that location was stored.  \"width\" is the number of locations
  being copied.")
  )

;;;
;;; END MUXES
;;;

;;;
;;; BEGIN ALU
;;;

;; basic bitwise operations

(defun and-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-and zs xs ys)
  )

(defun or-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-or zs xs ys)
  )

(defun xor-chain (xs ys zs)
  (assert (= (length xs) (length ys) (length zs)))
  (mapcar #'make-xor zs xs ys)
  )

(defun not-chain (xs zs)
  (assert (not (null xs)))
  (assert (= (length xs) (length zs)))
  (mapcar #'make-not zs xs)
  )

(defun add-one (xs zs c-in tmp)
  (labels ((add-carry (x z)
	     (list (make-and tmp x c-in) ; new carry = old x AND old carry
		   (make-xor z c-in x) ; new bit = old x XOR old cin
		   (make-instance 'copy :dest c-in :op1 tmp :op2 1) ; copy over new carry
		   )))
    (append
     (list (make-instance 'const :dest c-in :op1 1) )
     (mapcan #'add-carry xs zs)
     (list (make-instance 'const :dest c-in :op1 0)
	   (make-instance 'const :dest tmp  :op1 0) ); clear after use
     ))
  )

(defun twos-complement (xs zs c-in tmp)
  ;; input in xs, output in zs, c-in and tmp are one-gate temporaries
  (append 
   (not-chain xs zs)
   (add-one zs zs c-in tmp)
   ))

(defun zero-gate (gate)
  (make-instance 'const :dest gate :op1 0)
  )

(defun zero-group (gates)
  (loop for i from (car gates) to (+ (car gates) (length gates) -1) collect (make-instance 'const :dest i :op1 0) )
  )

;; Arithmetic operations (add, subtract, multiply, divide)
;; It is the responsibility of the function to zero out the accessory wires (generally c-in and tmps) before use if they must be zerod,
;; while it was the responsibility of the instruction macro to allocate them
;; the optimizer's reaching def analysis should eliminate all of the unnecessary zero-ing gates,

(defun adder-chain (xs ys zs c-in tmp1 tmp2 tmp3)
  "Create a ripple carry adder chain."
  (assert (= (length xs) (length ys) (length zs)))
  (labels ((full-adder (x y z)
             (let* ((sum-ops (list (make-xor tmp1 x y)
                                   (make-xor z tmp1 c-in)))
                    (carry-ops (list (make-xor tmp1 x y)
                                     (make-xor tmp2 c-in x)
                                     (make-and tmp3 tmp1 tmp2)
                                     (make-xor c-in x tmp3))))
               (append sum-ops carry-ops))))
    (append 
     (list
      (make-instance 'const :dest c-in :op1 0)
      (make-instance 'const :dest tmp1 :op1 0)
      (make-instance 'const :dest tmp2 :op1 0)
      (make-instance 'const :dest tmp3 :op1 0))
     (mapcan #'full-adder xs ys zs)
     )))

(defun subtractor-chain (xs ys zs c-in tmp1 tmp2 tmp3)
  "Create a ripple-borrow subtractor chain."
  (assert (= (length xs) (length ys) (length zs)))
  (labels ((full-subtractor (x y z)
             (let* ((diff-ops (list (make-xor tmp1 x y)
                                     (make-xor z tmp1 c-in)))
                    (borrow-ops (list (make-xnor tmp1 x y)
                                      (make-and tmp2 c-in tmp1)
                                      (make-not tmp1 x)
                                      (make-and tmp3 tmp1 y)
                                      (make-or c-in tmp3 tmp2))))
               (append diff-ops borrow-ops))))
    (append
     (list 
      (make-instance 'const :dest c-in :op1 0)
      (make-instance 'const :dest tmp1 :op1 0)
      (make-instance 'const :dest tmp2 :op1 0)
      (make-instance 'const :dest tmp3 :op1 0))
     (mapcan #'full-subtractor xs ys zs)
     )))

(defun complement-subtract (xs ys zs tmps c-in tmp1 tmp2 tmp3)
  "Create a subtractor by the method of complements"
  (assert (= (length xs) (length ys) (length zs)))
  (append
   (twos-complement ys tmps c-in tmp1)
   (adder-chain xs tmps zs c-in tmp1 tmp2 tmp3) 
   (list
    (zero-gate c-in)
    (zero-gate tmp1)
    (zero-gate tmp2)
    (zero-gate tmp3))
   )
  )

(defun shift-and-add-multiplier (xs ys zs c-in tmp1 tmp2 tmp3 zro acc tmpr)
  "Create a shift-and-add multiplier using ripple-carry adders"
  (declare (optimize (speed 0) (debug 3)))
  (assert (= (length xs) (length ys) (length zs)(length acc)(length tmpr)))
  (labels ((shift-add (st x)
             (let ((n (first st))
                   (ops (second st))
                   )
               (list (1+ n) (append ops 
                                    (and-chain (loop for i in ys collect x) ys tmpr) ; multiply with AND
				    (adder-chain (append (loop for i from 1 to n collect zro) (butlast tmpr n)) acc zs c-in tmp1 tmp2 tmp3) ;; next row to add
				    (list (make-instance 'copy :dest (first acc) :op1 (first zs) :op2 (length zs))) ;; copy over from the accumulator into our result wires
				    ;; it is important that the previous op2 is (length acc) and not (length zs) because in the future zs may not be required to be the same size as xs and ys
 				    ))
               )
             )
           )
    (append  
     (zero-group acc);     (loop for i from (car acc) to (+ (car acc) (length acc) -1) collect (make-instance 'const :dest i :op1 0) )
     (list (zero-gate c-in)
	   (zero-gate zro)
	   (zero-gate tmp1)
	   (zero-gate tmp2)
	   (zero-gate tmp3))
     (second (reduce #'shift-add xs :initial-value (list 0 nil)))
     )
    )
  )

;; unsigned-less-than (arg1 arg2 dest tmp2 tmp3 tmp4)
;; mux (olds news res cnd tmp1 tmp2)
;; subtractor-chain (xs ys zs c-in tmp1 tmp2 tmp3)
(defun subtract-and-check (divisor dividend difference res condition tmp1 tmp2 tmp3 tmp4)
  (append
   (unsigned-less-than dividend divisor condition tmp2 tmp3 tmp4) ; cond := (divisor <= dividend)
   (list (make-not condition condition))
   (subtractor-chain dividend divisor difference tmp1 tmp2 tmp3 tmp4) ; difference := dividend - divisor
   (mux dividend difference res condition tmp2 tmp3) ; res = ( condition ? dividend : dfference)
   ;; res = ( divisor < dividend ? dividend : dividend - divisor )
   ;; cond ( := (divisor ?< dividend) )  is the next bit of the quotient
   )
)

(defmacro sub-div ()
  `(let ((ops (first st)) 
	 (dividend (second st))
	 (y (first yz))
	 (z (second yz)))
     (let ((newdiv (cons y (butlast dividend)))) ;; next msb of ys is the new lsb of dividend, cut off msb of dividend to make room
       (list 
	(append ops (subtract-and-check xs newdiv extras newdiv z tmp1 tmp2 tmp3 tmp4))
	newdiv)))
  )

;; shift-and subtract division:
;; extend ys with extras, then shift, compare, and subtract (and mux) along the way
;; we have to go backwards through the bits because the lsb is first
(defun shift-and-subtract-diviser (xs ys zs padding extras tmp1 tmp2 tmp3 tmp4)
  (assert (= (length xs)(length ys)(length zs)(length padding)(length extras)))
  (labels ((subtract-divide (st yz)
	     (sub-div)
	     ))
      (first (reduce #'subtract-divide
		     (mapcar (lambda(a b) (list a b)) (reverse ys) (reverse zs))
		     :initial-value (list nil padding)
		     ))))

(defun shift-subtract-divide-mod (xs ys zs padding extras tmp1 tmp2 tmp3 tmp4)
  (assert (= (length xs)(length ys)(length zs)(length padding)(length extras)))
  (labels ((subtract-divide (st yz)
	     (sub-div)
	     ))
    (append
     (first (reduce #'subtract-divide
		   (mapcar (lambda(a b) (list a b)) (reverse ys) (reverse zs))
		     :initial-value (list nil padding)
		     ))
     (list (make-instance 'copy :dest (first zs) :op1 (first ys) :op2 (length zs)) ;; use the fact that the final remainder is simply stuck in ys
     ))))


(defun signed-shift-and-subtract-diviser (xs ys zs padding extras tmpxs tmpys tmp1 tmp2 tmp3 tmp4 xsign ysign)
  (append
   (list
    (make-instance 'copy :dest xsign :op1 (car (last xs)) :op2 1)
    (make-instance 'copy :dest ysign :op1 (car (last ys)) :op2 1))
   (twos-complement xs tmpxs tmp1 tmp2)
   (twos-complement ys tmpys tmp1 tmp2)
   (mux xs tmpxs tmpxs xsign tmp1 tmp2)
   (mux ys tmpys tmpys ysign tmp1 tmp2)
   (shift-and-subtract-diviser tmpxs tmpys zs padding extras tmp1 tmp2 tmp3 tmp4)
   (list (make-xor tmp4 xsign ysign)) ;; tmp4 is our new cond bit
   (twos-complement zs extras tmp1 tmp2)
   (mux zs extras zs tmp4 tmp1 tmp2)
   )
)


;;;
;;; END ALU
;;; 

;;;
;;; BEGIN INSTRUCTIONS
;;;

;;; COMPARISONS

(defun eql-gates (arg1 arg2 dest tmp1 tmp2)
  ; implicitly, should dest start out as 0 or 1?
  ; if it starts as 0, tmp2 (copied back to dest) will never be 0 
  (assert (= (length arg1) (length arg2)))
  (if (and arg1 arg2)
    (append (list (make-xnor tmp1 
              (first arg1)
              (first arg2))
      (make-and tmp2 tmp1 dest)
      (make-instance 'copy :dest dest :op1 tmp2 :op2 1))
      (eql-gates (rest arg1) (rest arg2) dest tmp1 tmp2)
    )))

(definstr jumpv ; jump void (unconditional jump)
    (labels ((list-crossed-conditions (queue targ &optional (ret nil))
	       (declare (optimize (debug 3) (speed 0)))
	       (if (or (queue-emptyp queue)
		       (string= (branch-target-label (peek-queue queue)) "$$$END$$$")
		       )
		   ret ; if we're at the end of the queue or of the program, return
		   (let ((ctarg (peek-queue queue)) ;ctarg = current target
			 (rest (dequeue queue))
			 )
		     (list-crossed-conditions rest targ (if (>= targ (cdr (gethash (branch-target-label ctarg) labels))) ;if targ is past the current target,
							    (cons (branch-target-cnd-wire ctarg) ret) ; cons the next condition wire to ret
							    ret)) ; otherwise just ret
		     )
		   )
	       )
	     (emit-and-lst (wires res &optional (ret nil)) ; what is "res" supposed to mean?
	       (if (null wires)
		   ret
		   (emit-and-lst (rest wires)
				 res
				 (append (list (make-and res (first wires) res)) ; append an AND of res and first wire to the return
					 ret)
				 )
		   )
	       )
	     )
      (with-slots (s-args) op
	(pop-arg stack targ
		 (assert (and (= 1 (length targ))
			      (typep (first targ) 'string) ; target is a string
                                        ;                   (queue-emptyp targets)
			      )
			 )
		 (let ((targ (first targ)) ; get that string
		       )
                                        ;        (print targets)
		   (if (or (queue-emptyp targets)
			   (string= (branch-target-label (peek-queue targets)) "$$$END$$$")
			   )
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

(defmacro neu-nei ()
  ;  (declare (optimize (speed 0) (debug 3)))
  `(with-slots (s-args width) op
     (let ((width (* *byte-width* width))
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
			)
	     (let ((wires (1+ wires))
		   (cnd-wires wires)
		   )
	       (branch-case targ 
			    (let ((wires cnd-wires))
			      (add-instrs
				  (list 
				   (make-instance 'copy-indir :dest (1+ wires) :op1 0 :op2 1) ; global condition wire?
				   (make-and (+ 2 wires) wires (1+ wires))
				   (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
				   )
				(add-target targ wires (1+ wires)
				  (let ((wires (+ 3 wires)))
				    (close-instr)))))
			    (let ((wires cnd-wires))
			      (add-instrs (list 
					   (make-not (1+ wires) wires)
					   (make-instance 'branch :cnd (1+ wires) :targ targ)
					   )
				(let ((wires (+ 2 wires)))
 				  (close-instr))))))))))))

(definstr neu ; not equal unsigned
    (neu-nei)
  )

(definstr nei ; not equal signed
    (neu-nei)
)

(defmacro equ-eqi ()
  ;  (declare (optimize (speed 0) (debug 3)))
  `(with-slots (s-args width) op
     (let ((width (* *byte-width* width))
	   (targ (first s-args))
	   )
       (declare (type string targ))
       (pop-arg stack arg1
	 (pop-arg stack arg2
	   (assert (and (= width (length arg1) (length arg2))))
	   (add-instrs (append 
			(list (make-instance 'const :dest wires :op1 1))
			;; AND in the fall-through case
			(eql-gates arg1 arg2 wires (1+ wires) (+ 2 wires)))
	     (let ((wires (1+ wires))
		   (cnd-wires wires)
		   )
	       (branch-case targ 
			    (let ((wires cnd-wires))
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
				    ))))
			    (let ((wires cnd-wires))
			      (add-instrs (list 
					   (make-instance 'branch :cnd wires :targ targ)
					   )
				(let ((wires (+ 1 wires))
				      )
				  (close-instr)
				  )))))))))))

(definstr equ ; equal unsigned
    (equ-eqi)
  )

(definstr eqi ; equal signed
    (equ-eqi)
)

(defun unsigned-less-than (arg1 arg2 dest tmp2 tmp3 tmp4)
  ; dest passed as wires (simply to allocate a single wire). it acts as a temporary accumulator of results
  ; tmp2, tmp3, tmp4 are all one wire in length
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
    (append
     (list (make-instance 'const :dest dest :op1 0))
     (mapcan #'less-than arg1 arg2)
     (list
      (make-instance 'const :dest tmp2 :op1 0)
      (make-instance 'const :dest tmp3 :op1 0)
      (make-instance 'const :dest tmp4 :op1 0)
     ))))

(defun signed-less-than (arg1 arg2 tmpzs dest t1 t2 t3)
  (append 
   ;;(complement-subtract arg1 arg2 tmpzs t1 t2 t3 t4)
   (subtractor-chain arg1 arg2 tmpzs dest t1 t2 t3)
   (greater-than-zero tmpzs dest t1)
   ))

(defun greater-than-zero (arg1 dest tmp)
  (labels ((gt0 (a)
	     (list (make-or tmp a tmp) )))
    (append
     (list (make-instance 'const :dest tmp :op1 0) )
     (mapcan #'gt0 (butlast arg1)) ; the last bit of arg1 is the MSB, i.e. the sign bit
     (list (make-not dest (car (last arg1))) ; NOT that sign bit to AND with the chained OR
	   (make-and dest tmp dest) ; sign bit is 0 and something else is 1
	   ))))

(defmacro lti-gti (a1 a2 fn)
  (let ((arg1 (gensym))
	(arg2 (gensym))
	(fun (gensym))
	)
    `(let ((,arg1 ,a1)
	   (,arg2 ,a2)
	   (,fun ,fn)
	   )
       (with-slots (s-args width) op
	 (let ((width (* *byte-width* width))
	       (targ (first s-args)))
	   (with-temp-wires tmpzs width 
	     (with-temp-wires dst 1 
	       (with-temp-wires t1 1 
		 (with-temp-wires t2 1 
		   (with-temp-wires t3 1 
		     (declare (type string targ))
		     (assert (and (= width (length ,arg1) (length ,arg2))))
		     (add-instrs
			 (append ;; AND in the fall-through case
			  (funcall ,fun ,arg1 ,arg2 tmpzs dst t1 t2 t3))
		       (branch-case targ
				    (add-instrs
					(list 
					 (make-not t3 dst)
					 (make-instance 'copy-indir :dest t1 :op1 0 :op2 1)
					 (make-and t2 t3 t1)
					 (make-instance 'indir-copy :dest 0 :op1 t2 :op2 1)
					 )
				      (add-target targ t3 t1
					(close-instr)))
				    (add-instrs (list 
						 (make-instance 'branch :cnd dst :targ targ)
						 )
				      (close-instr))))))))))))))


(defmacro ltu-gtu (a1 a2 fn)
  (let ((arg1 (gensym))
	(arg2 (gensym))
	(fun (gensym))
	)
    `(let ((,arg1 ,a1)
	   (,arg2 ,a2)
	   (,fun ,fn)
	   )
;  (declare (optimize (speed 0) (debug 3)))
      (with-slots (s-args width) op
	     (let ((width (* *byte-width* width))
		    (targ (first s-args))
		    )
	       (declare (type string targ))
	       (assert (and (= width (length arg1) (length arg2))))
	       (with-temp-wires dst 1 
		 (with-temp-wires t1 1 
		   (with-temp-wires t2 1 
		     (with-temp-wires t3 1 
		       (add-instrs (append 
			 ;; AND in the fall-through case
				    (funcall ,fun ,arg2 ,arg1 dst t1 t2 t3))
			 (branch-case targ
				      (add-instrs
					  (list 
					   (make-not t3 dst)
					   (make-instance 'copy-indir :dest t1 :op1 0 :op2 1)
					   (make-and t2 t3 t1)
					   (make-instance 'indir-copy :dest 0 :op1 t2 :op2 1)
					   )
					(add-target targ t3 t1
					  (close-instr)))
				      (add-instrs (list (make-instance 'branch :cnd dst :targ targ))
					(close-instr)
					))))))))))))


(defmacro leu-geu (a1 a2 fn)
  ;  (declare (optimize (speed 0) (debug 3)))
  (let ((arg1 (gensym))
	(arg2 (gensym))
	(fun (gensym))
	)
    `(let ((,arg1 ,a1)
	   (,arg2 ,a2)
	   (,fun ,fn)
	   )
       (with-slots (s-args width) op
	  (let ((width (* *byte-width* width))
		(targ (first s-args)))
	    (declare (type string targ))
	    (assert (and (= width (length arg1) (length arg2))))
	    (with-temp-wires dst 1 
	      (with-temp-wires t1 1 
		(with-temp-wires t2 1 
		  (with-temp-wires t3 1 
		    (add-instrs (append 
				 ;; AND in the fall-through case
				 (funcall ,fun ,arg1 ,arg2 dst t1 t2 t3)
				 )
		      (branch-case targ
				   (add-instrs
				       (list 
					(make-instance 'copy-indir :dest t1 :op1 0 :op2 1)
					(make-and t2 dst t1)
					(make-instance 'indir-copy :dest 0 :op1 t2 :op2 1)
					)
				     (add-target targ dst t1
				       (close-instr)))
				   (add-instrs (list 
						(make-not t1 dst)
						(make-instance 'branch :cnd t1 :targ targ))
				     (close-instr)))))))))))))
	  
(defmacro lei-gei (a1 a2 fn)
  ;  (declare (optimize (speed 0) (debug 3)))
  (let ((arg1 (gensym))
	(arg2 (gensym))
	(fun (gensym))
	)
    `(let ((,arg1 ,a1)
	   (,arg2 ,a2)
	   (,fun ,fn)
	   )
       (with-slots (s-args width) op
	 (let ((width (* *byte-width* width))
	       (targ (first s-args)))
	   (declare (type string targ))
	   (assert (and (= width (length arg1) (length arg2))))
	   (with-temp-wires tmpzs width 
	     (with-temp-wires dst 1 
	       (with-temp-wires t1 1 
		 (with-temp-wires t2 1 
		   (with-temp-wires t3 1
		     (add-instrs (append 
				  ;; AND in the fall-through case
				  (funcall ,fun ,arg2 ,arg1 tmpzs dst t1 t2 t3)
				  )
		       (branch-case targ
				    (add-instrs
					(list 
					 (make-instance 'copy-indir :dest t1 :op1 0 :op2 1)
					 (make-and t2 dst t1)
					 (make-instance 'indir-copy :dest 0 :op1 t2 :op2 1)
					 )
				      (add-target targ dst t1
					(close-instr)))
				    (add-instrs (list
						 (make-not t1 dst)
						 (make-instance 'branch :cnd t1 :targ targ))
				      (close-instr))))))))))))))

(definstr leu ; less than or equal unsigned
  (pop-arg stack arg1
    (pop-arg stack arg2
      (leu-geu arg1 arg2 #'unsigned-less-than))))

(definstr geu ; greater than or equal unsigned
  (pop-arg stack arg1
    (pop-arg stack arg2
      (leu-geu arg2 arg1 #'unsigned-less-than))))

(definstr lei ; less than or equal signed
  (pop-arg stack arg1
    (pop-arg stack arg2
      (lei-gei arg1 arg2 #'signed-less-than))))

(definstr gei ; greater than or equal signed
  (pop-arg stack arg1
    (pop-arg stack arg2
      (lei-gei arg2 arg1 #'signed-less-than))))

(definstr ltu ; less than unsigned
  (pop-arg stack arg1
    (pop-arg stack arg2
      (ltu-gtu arg1 arg2 #'unsigned-less-than))))

(definstr gtu ; greater than unsigned
  (pop-arg stack arg1
    (pop-arg stack arg2
      (ltu-gtu arg2 arg1 #'unsigned-less-than))))

(definstr lti ; less than signed
  (pop-arg stack arg1
    (pop-arg stack arg2
      (lti-gti arg1 arg2 #'signed-less-than))))

(definstr gti ; greater than signed
  (pop-arg stack arg1
    (pop-arg stack arg2
      (lti-gti arg2 arg1 #'signed-less-than))))
#|
(defmacro lt-gt-eq-neq-compare (a1 a2 fncall fwd bwd)
  (let ((arg1 (gensym))
	(arg2 (gensym))
	(fun (gensym))
	(fwd (gensym))
	(bwd (gensym))
	)
    `(let ((,arg1 ,a1)
	   (,arg2 ,a2)
	   (,fun ,fncall)
	   (,forward ,fwd)
	   (,backward ,bwd)
	   )
       (with-slots (s-args width) op
	 (let ((width (* *byte-width* width))
	       (targ (first s-args)))
	   (with-temp-wires tmpzs width 
	     (with-temp-wires dst 1 
	       (with-temp-wires t1 1 
		 (with-temp-wires t2 1 
		   (with-temp-wires t3 1
		     (with-temp-wires t4 1
		       (declare (type string targ))
		       (assert (and (= width (length ,arg1) (length ,arg2))))
		       (add-instrs
			   (append ;; AND in the fall-through case
			    ,fun)
			 (branch-case targ
				      ,forward
				      ,backward
				      )))))))))))))
|#


;;; END COMPARISONS

;;; CONSTS, POINTERS

(defmacro intcnst ()
  `(with-slots (s-args) op
     (let ((width (* *byte-width* (parse-integer (first s-args))))
           (value (parse-integer (second s-args)))
           )
       (let ((dwires (loop for i from wires to (+ wires width -1) collect i))
             )
         (add-instrs (list (make-instance 'const :dest wires :op1 value)
                           (make-instance 'bits :dest dwires :op1 wires))
           (let ((wires (+ wires width)))
             (push-stack stack width dwires
               (close-instr)
               )))))))

(definstr cnstu ; unsigned constant
  (intcnst)
  )

(definstr cnsti ; signed constant
  (intcnst)
  )

(definstr addp ; add pointer
  (pop-arg stack arg1
    (pop-arg stack arg2
      (format t "addp: ~A ~A ~%" arg1 arg2)
      (cond
        ((= (length arg1) (length arg2) 1)
         (add-instrs (list
                      (make-instance 'pcf2-bc:add :dest wires :op1 (first arg1) :op2 (first arg2)))
           (push-stack stack 1 (list wires)
             (let ((wires (1+ wires)))
               (close-instr)))))
        ((= (length arg1) 1)
         (add-instrs (list (make-instance 'join :dest wires :op1 arg2)
                           (make-instance 'const :dest (1+ wires) :op1 *byte-width*) ;; magic 8?
                           (make-instance 'pcf2-bc:mul :dest wires :op1 wires :op2 (1+ wires))
                           (make-instance 'pcf2-bc:add :dest (1+ wires) :op1 (first arg1) :op2 wires))
           (push-stack stack 1 (list (1+ wires))
             (let ((wires (+ 2 wires)))
               (close-instr)))))
        ((= (length arg2) 1)
         (add-instrs (list (make-instance 'join :dest wires :op1 arg1)
                           (make-instance 'const :dest (1+ wires) :op1 *byte-width*) ;; magic 8?
                           (make-instance 'pcf2-bc:mul :dest wires :op1 wires :op2 (1+ wires))
                           (make-instance 'pcf2-bc:add :dest (1+ wires) :op1 (first arg2) :op2 wires))
           (push-stack stack 1 (list (1+ wires))
             (let ((wires (+ 2 wires)))
               (close-instr)))))
        (t
         (add-instrs (list (make-instance 'join :dest wires :op1 arg1)
                           (make-instance 'const :dest (1+ wires) :op1 *byte-width*) ;; magic 8?
                           (make-instance 'pcf2-bc:mul :dest wires :op1 wires :op2 (1+ wires))
                           (make-instance 'join :dest (1+ wires) :op1 arg2)
                           (make-instance 'const :dest (+ 2 wires) :op1 *byte-width*) ;; magic 8?
                           (make-instance 'pcf2-bc:mul :dest (1+ wires) :op1 (1+ wires) :op2 (+ 2 wires))
                           (make-instance 'pcf2-bc:add :dest (+ 2 wires) :op1 wires :op2 (1+ wires)))
           (push-stack stack 1 (list (+ 2 wires))
             (let ((wires (+ 3 wires)))
               (close-instr)))))))))

;;; arithmetic operations
;;; add, subtract, multiply, divide
;;; these instructions are required to supply the input and output wires
;;; the functions called by these macros will allocated their own temporary wires as necessary
;;; the wires variable does have to keep global state

(defmacro ripple-carry-adder ()
  `(with-slots (width) op
     (let ((width (* *byte-width* width)))
       (with-temp-wires rwires width
	 (with-temp-wires tmp1 1
	   (with-temp-wires tmp2 1
	     (with-temp-wires tmp3 1
	       (with-temp-wires tmp4 1
		 (pop-arg stack arg1
		   (pop-arg stack arg2
		     (push-stack stack width rwires
		       (add-instrs (adder-chain 
				    arg1 
				    arg2
				    rwires 
				    tmp1 
				    tmp2 
				    tmp3 
				    tmp4
					;  wires
				    )
		 (close-instr)))))))))))))

(definstr addu ; add unsigned
  (ripple-carry-adder)
  )

(definstr addi ; add signed
  (ripple-carry-adder)
)

(defmacro shift-add-multiply ()
  `(with-slots (width) op
    (let ((width (* *byte-width* width)))
      (with-temp-wires rwires width 
	(with-temp-wires cin 1
	  (with-temp-wires t2 1
	    (with-temp-wires t3 1
	      (with-temp-wires t4 1
		(with-temp-wires t5 1
		  (with-temp-wires accum width
		    (with-temp-wires tr width
		      (pop-arg stack arg1
			(pop-arg stack arg2
			  (push-stack stack width rwires
			    (add-instrs 
				(shift-and-add-multiplier ;(karatsuba-multiplication
				 arg1 arg2 rwires
					;				 wires
				 cin t2 t3 t4 t5 accum tr
				 )
			      (close-instr))))))))))))))))

(definstr muli ; multiply signed
  (shift-add-multiply)
)

(definstr mulu ; multiply unsigned
  (shift-add-multiply)
  )

(definstr divu
  (with-slots (width) op
    (let ((width (* *byte-width* width)))
      (with-temp-wires rwires width
	(with-temp-wires pad width
	  (with-temp-wires extras width
	    (with-temp-wires cin 1
	      (with-temp-wires tmp2 1
		(with-temp-wires tmp3 1
		  (with-temp-wires tmp4 1
		    (pop-arg stack arg1
		      (pop-arg stack arg2
			(push-stack stack width rwires
			    (add-instrs 
				(shift-and-subtract-diviser arg1 arg2 rwires pad extras cin tmp2 tmp3 tmp4)
			      (close-instr)))))))))))))))

(definstr divi
  (with-slots (width) op
    (let ((width (* *byte-width* width)))
      (with-temp-wires rwires width
	(with-temp-wires pad width
	  (with-temp-wires extras width
	    (with-temp-wires tmpxs width
	      (with-temp-wires tmpys width
		(with-temp-wires cin 1
		  (with-temp-wires tmp2 1
		    (with-temp-wires tmp3 1
		      (with-temp-wires tmp4 1
			(with-temp-wires xsign 1
			  (with-temp-wires ysign 1
			    (pop-arg stack arg1
			      (pop-arg stack arg2
				(push-stack stack width rwires
				  (add-instrs 
				      (signed-shift-and-subtract-diviser arg1 arg2 rwires pad extras tmpxs tmpys cin tmp2 tmp3 tmp4 xsign ysign)
				    (close-instr)))))))))))))))))))

;;; modulus operator is undefined (or not consistently defined) when at least one operand is negative, so we leave it undefined here. modi works the same as modu
(defmacro modi-modu ()
  `(with-slots (width) op
    (let ((width (* *byte-width* width)))
      (with-temp-wires rwires width
	(with-temp-wires pad width
	  (with-temp-wires extras width
	    (with-temp-wires cin 1
	      (with-temp-wires tmp2 1
		(with-temp-wires tmp3 1
		  (with-temp-wires tmp4 1
		    (pop-arg stack arg1
		      (pop-arg stack arg2
			(push-stack stack width rwires
			  (add-instrs 
			      (shift-subtract-divide-mod arg1 arg2 rwires pad extras cin tmp2 tmp3 tmp4)
			    (close-instr))))))))))))))
  )

(definstr modu
  (modi-modu)
  )

(definstr modi
  (modi-modu)
)

(defmacro subtract-by-complement ()
  "subtraction by the method of complements."
  `(with-slots (width) op
    (let ((width (* *byte-width* width)))
      (with-temp-wires rwires width
	(with-temp-wires tmps width
	  (with-temp-wires cin 1
	    (with-temp-wires t1 1
	      (with-temp-wires t2 1
		(with-temp-wires t3 1
		  (pop-arg stack arg2
		    (pop-arg stack arg1
		      (push-stack stack width rwires
			(add-instrs 
			    ;;(subtractor-chain 
			    (complement-subtract  
			     arg1 arg2 rwires
			     tmps
			     cin t1 t2 t3
			     )
			  (close-instr))))))))))))))

(definstr subu ; subtract unsigned
  ;; note that this works just the same as signed, underflow will not generate a warning or error
  (subtract-by-complement)
  )

(definstr subi ; subtract signed
  (subtract-by-complement)
  )
	
;;
;; bitwise shifts
;;

(defmacro right-or-left-shift (zero-concat cnst-shift &body body)
  `(with-slots (width) op
    (let ((width (* *byte-width* width)))
      (pop-arg stack amount
        (pop-arg stack val
	  (with-temp-wires rwires width
	    (with-temp-wires rwires* width
	      (with-temp-wires zro 1
		(with-temp-wires tmp1 1
		  (with-temp-wires tmp2 1
		    ;; Check to see if there is a constant on top of the stack (this is the shift amount)
		    (let ((cnst (car (cdr (map-find (write-to-string iidx) (cadr (cdr cnsts)))))))
		      (print cnst)
		      (assert (or (equalp cnst 'not-const) (typep cnst 'number)))
		      (format *error-output* "right-or-left-shift cnst: ~D, iidx: ~D~%" cnst iidx)
		      (if (equalp cnst 'not-const)
			  (progn
			    (assert (= (length rwires) (length rwires*) width))
			    (add-instrs (append
					 (list 
					  (make-instance 'const :dest zro :op1 0)
					  (make-instance 'copy :dest (first rwires) :op1 (first val) :op2 width))
					 (mapcan (lambda (x y)
						   (let ((shifted-val ,zero-concat))
						     (assert (= (length shifted-val) width))
						     (append (mux rwires ; mux rwires with shifted val and put result in rwires*
								  shifted-val 
								  rwires* 
								  x 
								  tmp1 
								  tmp2)
							     (list (make-instance 'copy ; then copy rwires* back into rwires
										  :dest (first rwires) 
										  :op1 (first rwires*) 
										  :op2 width))))) 
						 (subseq amount 0 (1+ (floor (log width 2))) ) ; x: a series of cascading muxes on each bit
						 (loop for i from 0 to (floor (log width 2)) collect i) ; y will be used in shifted-val
						 ))
			      (push-stack stack width rwires*
			      ,@body )))
			  ;; TODO: shift only by a constant amount
			  (let ((y cnst))
			    (add-instrs (append
					 (list 
					  (make-instance 'const :dest zro :op1 0)
					  (make-instance 'copy :dest (first rwires) :op1 (first val) :op2 width)
					  )
					 (let ((shifted-value ,cnst-shift))
					   (assert (= (length shifted-value) width))
					   (loop for i in (reverse shifted-value) for j in (reverse rwires*) collect ; why reverse both here?
						(make-instance 'copy :dest j :op1 i :op2 1))))
			      (push-stack stack width rwires*
				,@body)))))))))))))))

(definstr lshu ; left shift unsigned
  (right-or-left-shift 
      (append
       (loop for i from 0 to (1- (expt 2 y)) collect zro)
       (subseq rwires 0 (- width (expt 2 y)))
       )
      (append
       (loop for i from 0 to (1- y) collect zro)
       (subseq rwires 0 (- width y))
       )
    (close-instr)
    )
  )

(definstr lshi ; left shift unsigned
  (right-or-left-shift 
      (append
       (loop for i from 0 to (1- (expt 2 y)) collect zro)
       (subseq rwires 0 (- width (expt 2 y)))
       )
      (append
       (loop for i from 0 to (1- y) collect zro)
       (subseq rwires y width)
       )
    (close-instr)
    )
  )

(definstr rshu ; right shift unsigned
  (right-or-left-shift
      (append
       (subseq rwires (expt 2 y) width)
       (loop for i from 0 to (1- (expt 2 y)) collect zro)
       )
      (append
       (subseq rwires 0 (- width y))
       (loop for i from 0 to (1- y) collect zro)
       )
    (close-instr)
    )
  )

(definstr rshi ; right shift signed
  (right-or-left-shift
      (append
       (subseq rwires (expt 2 y) width)
       (loop for i from 0 to (1- (expt 2 y)) collect zro)
       )
      (append
       (subseq rwires y width)
       (loop for i from 0 to (1- y) collect zro)
       )
    (close-instr)
    )
  )

;;
;; bitwise arithmetic operations
;;

(defmacro bitwise-1-arg (fn)
;  "Perform bitwise operation \"fn\" on one operator"
  (let ((fun (gensym)))
    `(let ((,fun ,fn))
       (with-slots (width) op
	 (let ((width (* *byte-width* width)))
	   (with-temp-wires rwires width
	     (pop-arg stack arg1
	       (push-stack stack width rwires
		 (add-instrs (funcall ,fun arg1 rwires)
		   (close-instr))))))))))

(defmacro bitwise-2-arg (fn)
;  "Perform bitwise operation \"fn\" on two operators"
  (let ((fun (gensym)))
    `(let ((,fun ,fn))
       (with-slots (width) op
	  (let ((width (* *byte-width* width)))
	    (with-temp-wires rwires width
	      (pop-arg stack arg1
		(pop-arg stack arg2
		  (push-stack stack width rwires
		    (add-instrs (funcall ,fun arg1 arg2 rwires)
		      (close-instr)))))))))))

(definstr boru
  (bitwise-2-arg #'or-chain)
)

(definstr bori
  (bitwise-2-arg #'or-chain)
)

(definstr bxoru
  (bitwise-2-arg #'xor-chain)
)

(definstr bxori
  (bitwise-2-arg #'xor-chain)
)

(definstr bandu
  (bitwise-2-arg #'and-chain)
  )

(definstr bandi
  (bitwise-2-arg #'and-chain)
  )

(definstr bcomu
  (bitwise-1-arg #'not-chain)
)

(definstr bcomi
  (bitwise-1-arg #'not-chain)
)

(definstr negi
  (with-slots (width) op
    (let ((width (* *byte-width* width)))
      (with-temp-wires rwires width
	(with-temp-wires tmp1 1
	  (with-temp-wires tmp2 1
	    (pop-arg stack arg
	      (push-stack stack width rwires
		(add-instrs
		    (twos-complement arg rwires tmp1 tmp2)
		  (close-instr))))))))))

;;; CALLING FUNCTIONS

(defstruct arg 
  (len)
  (loc)
  )

(definstr callv ; call void function
  (pop-arg stack fname
    (assert (and (= 1 (length fname))
                 (typep (first fname) 'string)
                 )
            ) 
    ;; (assert (or (queue-emptyp targets)
    ;;             (string= (branch-target-label (peek-queue targets)) "$$$END$$$")))
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
                          (list (make-instance 'call :newbase (+ i wires) :fname (first fname))
                                )
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

(defmacro calli-callu ()
  `(with-slots (width) op
     ;; (assert (or (queue-emptyp targets)
     ;;             (string= (branch-target-label (peek-queue targets)) "$$$END$$$")))
     (let ((width (* *byte-width* width))
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
			      (incf i (arg-len arg))))
		       (list (make-instance 'call :newbase (+ i wires) :fname (first fname)))
		       )
	     (push-stack stack width (loop for j from (+ i wires) to (+ wires width i -1) collect j)
	       (let ((wires (+ wires width i 1))
		     (arglist nil)
		     )
		 (close-instr)))))))))

(definstr callu ; call unsigned int function
  (calli-callu)
  )

(definstr calli ; call signed int function
  (calli-callu)
  )

(defmacro reti-retu ()
  ;; We must copy the value from the stack to the very beginning of
  ;; the stack frame, which is where return values are stored (see
  ;; above).
  `(assert (or (queue-emptyp targets)
              (string= (branch-target-label (peek-queue targets)) "$$$END$$$")
              )
          )
  `(with-slots (width) op
    (pop-arg stack arg
      (add-instrs (loop for j in arg for i from 0 collect
                       (make-xnor i j j)
                       )
        (close-instr)))))

(definstr reti ; return signed int 
  (reti-retu)
  )

(definstr retu ; return unsigned int
  (reti-retu)
  )

(defmacro argu-argi ()
  `(with-slots (width) op
    (pop-arg stack arg
      (let ((arglist (append arglist (list (make-arg :len (* *byte-width* width) :loc arg))))
            )
        (close-instr)
        ))))

(definstr argu ; unsigned int argument
  (argu-argi)
  )

(definstr argi ; signed int argument
  (argu-argi)
  )

(definstr argp ; pointer argument
  (with-slots (width) op
    (pop-arg stack arg
      (assert (= 1 (length arg)))
      (let ((arglist (append arglist (list (make-arg :len (* *byte-width* width) :loc arg))))
            )
        (close-instr)
        )
      )
    )
  )

(definstr addrgp ; address of a global pointer
;  (declare (optimize (debug 3) (speed 0)))
  (with-slots (s-args) op
    (let ((addr* (string-tokenizer:tokenize (second s-args) #\+))
          ;(width (parse-integer (first s-args)))
          )
      (let ((a (gethash (first addr*) labels nil)))
        (declare (type (or null (cons symbol (integer 1))) a))
        (let ((addr (if a
                        (if (equalp (car a) 'glob)
                            ;; Do not multiply by width here.
                            (+ (cdr a) (if (second addr*) (* *byte-width* (parse-integer (second addr*))) 0))
                            (second s-args));(gethash (second s-args) labels nil)
                        (second s-args))))
          (format t "~&Address for ~A is ~A (at ~A)~%" (second s-args) addr wires)
          (add-instrs (if (equalp (car a) 'glob)
                          (list (make-instance 'const :dest wires :op1 (+ (if (second addr*) (* *byte-width* (parse-integer (second addr*))) 0) (cdr a)))))
            (push-stack stack 1 (if (equalp (car a) 'glob) 
                                    (list wires) 
                                    (list addr))
              (let ((wires (if (equalp (car a) 'glob) (1+ wires) wires)))
                (close-instr)))))))))


;; Below we offset the addresses by 1 because the base of the stack
;; frame will always have a pointer to the global condition wire in
;; local position 0. ( we also offset by baseinit)
(definstr addrlp ; address of a local (pointer)
  (with-slots (s-args) op
    (let ((addr (let ((nums (string-tokenizer:tokenize (second s-args) #\+))
                      )
                  ;;(parse-integer (second s-args)))
                  (if (< 2 (length nums))
                      (parse-integer (first nums))
                      (reduce #'(lambda (x y) (+ (parse-integer y) x)) nums :initial-value 0)
                      ))
            ))
      (push-stack stack 1 (list wires)
        (add-instrs (list 
                     (make-instance 'const :dest wires :op1 (+ 1 baseinit (* *byte-width* addr)))
                     (make-instance 'mkptr :dest wires)
                     )
          (let ((wires (1+ wires)))
            (close-instr)))))))

(definstr addrfp ;address of a parameter (pointer)
  (with-slots (s-args) op
    (let ((width (* *byte-width* (parse-integer (first s-args))))
          (addr (parse-integer (second s-args)))
          )
      (format t "addrfp: ~A (to ~A)~%" (- (* (* -1 *byte-width*) addr) width) wires)
      (add-instrs (list (make-instance 'const :dest wires :op1 (- (* (* -1 *byte-width*) addr) width))
                        (make-instance 'mkptr :dest wires)
                        )
        (push-stack stack 1 (list wires)
          (let ((wires (1+ wires)))
            (close-instr)
            )
          )
        )
      )
    )
  )

(defmacro indiri-indiru ()
  ;; Pop a pointer off the stack, dereference the pointer and push its value back on the stack
  `(with-slots (width) op
     (let ((width (* *byte-width* width)))
       (with-temp-wires rwires width
	   (pop-arg stack ptr
	     (add-instrs (list (make-instance 'copy-indir :dest (first rwires) :op1 (first ptr) :op2 width))
	       (push-stack stack width rwires
		 (close-instr)
		 )))))))

(definstr indiru ; fetch unsigned int
    (indiri-indiru)
  )

(definstr indiri ; fetch signed int
    (indiri-indiru)
)

(definstr indirp ; fetch pointer
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

(definstr asgnu ; assigned unsigned int
  (with-slots (width) op
    (let* ((width (* *byte-width* width))
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
          ;;
          ;; Note: this is problematic, as assignments to an array may
          ;; run into problems when the array index is computed
          ;; according to a complex formula.  The solution should be
          ;; to delay the creation of muxes until the label is reached
          ;; by e.g. adding an additional field to the priority queue
          ;; data elements, a queue of assignments that should be
          ;; multiplexed (we need a queue to ensure that we do this in
          ;; the right order when multiple assignments to the same
          ;; location occur in a conditional block).  The old value
          ;; will need to be move to some temporary location that we
          ;; allocate here when the assignment is made, and the new
          ;; value will be unconditionally copied to the location of
          ;; the old value.
          ;;
          ;; Potential optimization: store things that should be muxed
          ;; as a map, and do not even add to a queue.  Whatever the
          ;; last assignment was should get muxed when the target is
          ;; reached.  The map should include the location the old
          ;; value was copied to and the condition wire at the time of
          ;; the assignment.
          (asgn-mux
           (close-instr)
           )
          )
        )
      )
    )
  )

(definstr asgni ; assign signed int
  (with-slots (width) op
    (let* ((width (* *byte-width* width))
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


(definstr asgnp ; assign pointer
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
    (assert (or (queue-emptyp targets)
                (string= (branch-target-label (peek-queue targets)) "$$$END$$$")))
    (let ((local-size (parse-integer (second s-args)))
          (args-size (parse-integer (third s-args)))
          )
      (let ((wires (+ 1 wires (* *byte-width* local-size)))
            (argsize (* *byte-width* args-size))
            (arglist nil)
            (iidx 0)
            (cnsts (cons (car cnsts) (cdr (map-find (first s-args) (car cnsts)))))
            )
        ;; We add instructions to set up the pointer to the global position wire
        ;;
        ;; We need no 'mkptr here, because we need an absolute pointer to wire 0
        (add-instrs (list 
                     (make-instance 'label :str (first s-args))
                     (make-instance 'clear :localsize (* *byte-width* local-size))
                     )
          (close-instr)
          )
        )
      )
    )
  )

(definstr endproc
  (assert (null stack))
  (assert (or (queue-emptyp targets)
              (string= (branch-target-label (peek-queue targets)) "$$$END$$$")
              )
          )
  (let ((cnd (peek-queue targets))
        )
    (add-instrs
        (reverse
         (map-reduce
          (lambda (st k x)
            (declare (type mux-item x) (ignore k))
            (let* ((width (mux-item-width x))
                   (tmp1 (+ wires (* 2 width)))
                   (tmp2 (+ wires (* 2 width) 1))
                   (mux-dest (loop for i from (+ width wires) to (+ wires (* 2 width) -1) collect i))
                   (new-copy-wires (loop for i from wires to (+ width wires -1) collect i)))
              (append (reverse
                       (append
                        (list (make-instance 'copy-indir
                                             :dest (car new-copy-wires)
                                             :op1 (mux-item-address x)
                                             :op2 width))
                        (mux 
                         (mux-item-old-copy x) ; olds 
                         new-copy-wires;(mux-item-address x) ;news
                         mux-dest ; res
                         (mux-item-cnd-wire x) ;cnd
                         tmp1 ;t1
                         tmp2) ;t2
                        (list
                         (make-instance 'indir-copy 
                                        :dest (the integer (mux-item-address x))
                                        :op1 (car mux-dest)
                                        :op2 width))))
                      st)))
          (branch-target-mux-list cnd)
          nil))
      (add-instrs (list (make-instance 'ret :value wires))
        (let* ((wires 0)
               (mtarget (peek-queue targets))
               ;; Clear the muxes emitted for this function, we no longer require them
               (targets (enqueue (cons 1000000
                                  (make-branch-target
                                   :label (branch-target-label mtarget)
                                   :cnd-wire (branch-target-cnd-wire mtarget)
                                   :glob-cnd (branch-target-glob-cnd mtarget)
                                   :mux-list (map-empty)
                                   )
                                  )
                                 (make-queue)
                                 )
                 )
               )
          (close-instr)
          )
        )
      )
    )
  )

(definstr import
  (close-instr)
  )

(definstr export
  (close-instr)
  )

(definstr code ; code section
  (let ((bss nil))
    (close-instr)
    )
  )

(definstr bss ; data section
  (let ((bss t))
    (close-instr)
    )
  )

(definstr skip ; skip to allocate space on the stack (useful in the data section)
;  (declare (optimize (debug 3) (speed 0)))
  (with-slots (s-args) op
    (let ((baseinit (+ baseinit (* *byte-width* (parse-integer (first s-args)))))
          )
      (close-instr)
      )
    )
  )

(definstr align ; skip some space to align with cache (not useful for our purposes)
  (close-instr)
  )

(defmacro convert-type-instr (&body body)
  ;; if first arg = second arg (same lengths), do nothing
  ;; if first arg < second arg, cut off appropriate amount of second arg
  ;; if first arg > second arg, extend
  `(with-slots (s-args) op
     (let ((targwidth  (* *byte-width* (parse-integer (first s-args))))
	   (curwidth (* *byte-width* (parse-integer (second s-args)))))
       (pop-arg stack arg
           (cond
             ((= targwidth curwidth)
              (push-stack stack targwidth arg
                (close-instr)))
             ((< targwidth curwidth)
              (push-stack stack targwidth (subseq arg 0 targwidth)
;                (add-instrs (list (make-instance 'copy :dest (first rwires) :op1 (first arg) :op2 targwidth))
                  (close-instr)));)
             ((> targwidth curwidth)
	      (with-temp-wires rwires targwidth
	       (push-stack stack targwidth rwires
		(add-instrs 
		 (append 
		  (list (make-instance 'copy :dest (first rwires) :op1 (first arg) :op2 curwidth))
		  ,@body)
		  (close-instr))))))))))

(definstr cvui ; convert from unsigned integer (to signed integer)
  (convert-type-instr))

(definstr cviu ; convert from signed integer (to unsigned integer)
  (convert-type-instr))

(definstr cvii ; convert from signed integer (to signed integer)
  (convert-type-instr
   (add-instrs
    (loop for i in (subseq rwires curwidth) collect
	  (make-instance 'copy :dest i :op1 (car (last arg)) :op2 1));sign extend
    )))

(definstr cvuu ; convert from unsigned integer (to unsigned integer)
  (convert-type-instr))

(definstr labelv ; label definition (void)
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
                           (append (list (make-instance 'pcf2-bc:copy-indir :dest wires :op1 0 :op2 1)
                                         (make-not (1+ wires) (branch-target-cnd-wire cnd))
                                         ;; globcnd <- globcnd + (oldglobcnd)*(~cnd)
                                         (make-and (1+ wires) (branch-target-glob-cnd cnd) (1+ wires))
                                         (make-or (+ 2 wires) wires (1+ wires))
                                         (make-instance 'indir-copy :dest 0 :op1 (+ 2 wires) :op2 1)
                                         )
                                   (reverse
                                    (map-reduce
                                     (lambda (st k x)
                                       (declare (type mux-item x) (ignore k))
                                       (let* ((width (mux-item-width x))
                                              (tmp1 (+ wires (* 2 width)))
                                              (tmp2 (+ wires (* 2 width) 1))
                                              (mux-dest (loop for i from (+ width wires) to (+ wires (* 2 width) -1) collect i))
                                              (new-copy-wires (loop for i from wires to (+ width wires -1) collect i))
                                              )
                                         (append (reverse
                                                  (append
                                                   (list (make-instance 'copy-indir
                                                                        :dest (car new-copy-wires)
                                                                        :op1 (mux-item-address x)
                                                                        :op2 width))
                                                   (mux 
                                                    (mux-item-old-copy x)
                                                    new-copy-wires;(mux-item-address x)
                                                    mux-dest
                                                    (mux-item-cnd-wire x)
                                                    tmp1
                                                    tmp2
                                                    )
                                                   (list
                                                    (make-instance 'indir-copy 
                                                                   :dest (the integer (mux-item-address x))
                                                                   :op1 (car mux-dest)
                                                                   :op2 width) )))
                                                 st)))
                                     (branch-target-mux-list cnd)
                                     nil)))))
		      (let ((wires (+ wires (if cnd ;not-empty 
						3
						0))))
			(if (and (not (queue-emptyp targets)) (equalp (first s-args) (branch-target-label (peek-queue targets))))
			    ;; There are no other conditional assignments for this target
			    (repeat-instr)
			    (close-instr)
                                        ;(assert nil)
			    ))))
        (close-instr) ;; bss do nothing
        )))
