;; Interpreter for the PCF2 bytecode

(defpackage :pcf2-interpreter 
  (:use :pcf2-bc :common-lisp)
  (:export run-opcodes
           make-pcf2-state))
(in-package :pcf2-interpreter)

(defstruct pcf2-state
  (memory)
  (baseptr)
  (iptr)
  (lbls)
  (call-stack)
  (:documentation "The memory array is the wire table + pointers + constant values.  The baseptr is the pointer in the memory array to the beginning of the current stack frame.  The iptr is the pointer to the next opcode.  The lbls slot is the hash table of labels to the beginning of the list of opcodes they correspond to.")
  )

(defun run-opcodes (opcodes state)
  (declare (type pcf2-state state)
           )
  (if (null opcodes)
    state
    (let ((op (first opcodes))
          (rst (rest opcodes))
          )
      (run-opcodes rst (run-opcode state op))
      )
    )
  )

(defmacro get-state-val (state idx)
  `(aref (pcf2-state-memory ,state) ,idx)
  )

(defmacro set-state-val (state idx val)
  `(progn
     (setf (aref (pcf2-state-memory ,state) ,idx) ,val)
     ,state)
  )

(defmacro find-label (state label)
  `(gethash ,label (pcf2-state-lbls ,state))
  )

(defmacro update-state (state memory baseptr iptr call-stack)
  `(progn
     ,(if memory
          `(setf (pcf2-state-memory ,state) ,memory)
          )
     ,(if baseptr
          `(setf (pcf2-state-baseptr ,state) ,baseptr)
          )
     ,(if iptr
          `(setf (pcf2-state-iptr ,state) ,iptr)
          )
     ,(if call-stack
          `(setf (pcf2-state-call-stack ,state) ,call-stack)
          )
     state
     )
  )

(defgeneric run-opcode (state opcode)
  (:documentation "Each PCF2 opcode will take in a program state and update it in some way")
  )

(defmethod run-opcode ((state pcf2-state) (opcode call))
  (with-slots (newbase fname) opcode
    (let ((new-iptr (find-label state fname))
          (call-stack (cons (cons (pcf2-state-iptr state)
                                  (pcf2-state-baseptr state))
                            (pcf2-state-call-stack state)))
          )
      (update-state state nil (+ (pcf2-state-baseptr state) newbase) new-iptr call-stack)
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode ret))
  (with-slots (value) opcode
    (let ((iptrbaseptr (first (pcf2-state-call-stack state)))
          (call-stack (rest (pcf2-state-call-stack state)))
          )
      (update-state state nil (cdr iptrbaseptr) (car iptrbaseptr) call-stack)
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode branch))
  (with-slots (cnd targ) opcode
    (let ((cnd-v (get-state-val state (+ (pcf2-state-baseptr state) cnd)))
          )
      (declare (type bit cnd-v))
      (if (zerop cnd-v)
          state
          (update-state state nil nil targ nil)
          )
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode add))
  (with-slots (dest op1 op2) opcode
    (let* ((true-op1 (+ op1 (pcf2-state-baseptr state)))
           (true-op2 (+ op2 (pcf2-state-baseptr state)))
           (true-dest (+ dest (pcf2-state-baseptr state)))
           (x (get-state-val state true-op1))
           (y (get-state-val state true-op2))
           )
      (declare (type integer x y))
      (set-state-val state true-dest (+ x y))
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode sub))
  (with-slots (dest op1 op2) opcode
    (let* ((true-op1 (+ op1 (pcf2-state-baseptr state)))
           (true-op2 (+ op2 (pcf2-state-baseptr state)))
           (true-dest (+ dest (pcf2-state-baseptr state)))
           (x (get-state-val state true-op1))
           (y (get-state-val state true-op2))
           )
      (declare (type integer x y))
      (set-state-val state true-dest (- x y))
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode mul))
  (with-slots (dest op1 op2) opcode
    (let* ((true-op1 (+ op1 (pcf2-state-baseptr state)))
           (true-op2 (+ op2 (pcf2-state-baseptr state)))
           (true-dest (+ dest (pcf2-state-baseptr state)))
           (x (get-state-val state true-op1))
           (y (get-state-val state true-op2))
           )
      (declare (type integer x y))
      (set-state-val state true-dest (* x y))
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode const))
  (with-slots (dest op1) opcode
    (let* ((true-dest (+ dest (pcf2-state-baseptr state)))
           )
      (set-state-val state true-dest op1)
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode copy))
  (with-slots (dest op1) opcode
    (let ((true-op1 (+ op1 (pcf2-state-baseptr state)))
          (true-dest (+ dest (pcf2-state-baseptr state)))
          )
      (set-state-val state true-dest
                     (get-state-val state true-op1)
                     )
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode gate))
  (with-slots (dest op1 op2 truth-table) opcode
    (let* ((true-op1 (+ op1 (pcf2-state-baseptr state)))
           (true-op2 (+ op2 (pcf2-state-baseptr state)))
           (true-dest (+ dest (pcf2-state-baseptr state)))
           (op1val (get-state-val state true-op1))
           (op2val (get-state-val state true-op2))
           )
      (set-state-val state true-dest
                     (bit truth-table (+ op1val (* 2 op2val))))
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode label))
  state
  )