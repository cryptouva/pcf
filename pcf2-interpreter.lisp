;; Interpreter for the PCF2 bytecode

(defpackage :pcf2-interpreter 
  (:use :pcf2-bc :skew-list :common-lisp)
  (:export run-opcodes
           init-state
           setup-labels
           pcf2-state))
(in-package :pcf2-interpreter)

(defstruct (pcf2-state
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "PCF2 State: ~%")
                (format stream "Memory: (")
                (skew-map #'(lambda (x) (format stream "~A " x))
                          (pcf2-state-memory struct))
                (format stream ")~%")
                (format stream "Baseptr: ~A~%" (pcf2-state-baseptr struct))
                (format stream "Insptr: ~A~%" (pcf2-state-iptr struct))
                (format stream "Call stack: ~A~%" (pcf2-state-call-stack struct))
                (format stream "Labels: ~A~%" (pcf2-state-lbls struct))
                )
              )
             )
  (memory)
  (baseptr 0 :type (integer 0))
  (iptr)
  (lbls)
  (call-stack)
  (alice-inputs)
  (bob-inputs)
  (:documentation "The memory random access list is the wire table + pointers + constant values.  The baseptr is the pointer in the memory array to the beginning of the current stack frame.  The iptr is the pointer to the next opcode.  The lbls slot is the hash table of labels to the beginning of the list of opcodes they correspond to.

The functions that operate on pcf2-state objects should treat these objects as immutable.  Thus we use a random access list rather than an array, and a linked list to create stack frames.  In doing so we can support debugging much effectively and potentially even allow reversible debugging of PCF2 programs.")
  )

(defun init-state (memsize opcodes inputs-file alice-input-size bob-input-size)
  "Initialize a PCF2 state object with \"memsize\" memory elements.  Alice and Bob inputs are read from the inputs-file parameter, which should be a stream"
  (declare (type stream inputs-file)
           (optimize (debug 3) (speed 0)))
  (let ((alice-input (parse-integer (read-line inputs-file) :radix 16))
        (bob-input (parse-integer (read-line inputs-file) :radix 16))
        )
    (let ((newstate (make-pcf2-state :memory
                                     (let ((m nil))
                                       (loop for i from 2 to memsize do
                                            (setf m (skew-cons 0 m))
                                            )
                                       (setf m (skew-cons 1 m))
                                       m)
                                     :baseptr 1
                                     :iptr opcodes
                                     :lbls (make-hash-table :test 'equalp)
                                     :call-stack nil
                                     :alice-inputs (skew-reverse
                                                    (cdr (reduce (lambda (st x)
                                                                   (declare (ignore x))
                                                                   (let ((val (car st))
                                                                         (lst (cdr st))
                                                                         )
                                                                     (cons (ash val -1) (skew-cons (mod val 2) lst))
                                                                     )
                                                                   )
                                                                 (loop for i from 1 to alice-input-size collect i)
                                                                 :initial-value (cons alice-input nil))))
                                     :bob-inputs (skew-reverse
                                                  (cdr (reduce (lambda (st x)
                                                                 (declare (ignore x))
                                                                 (let ((val (car st))
                                                                       (lst (cdr st))
                                                                       )
                                                                   (cons (ash val -1) (skew-cons (mod val 2) lst))
                                                                   )
                                                                 )
                                                               (loop for i from 1 to bob-input-size collect i)
                                                               :initial-value (cons bob-input nil))))
                                     )
            )
          )
      newstate
      )
    )
  )

(defun get-party-input (state idx party)
  (declare (optimize (debug 0) (speed 3)))
  (cond
    ((equalp party "alice") (skew-ref idx (pcf2-state-alice-inputs state)))
    ((equalp party "bob") (skew-ref idx (pcf2-state-bob-inputs state)))
    )
  )

(defmacro add-label (str opcodes state)
  `(progn
     (setf (gethash ,str (pcf2-state-lbls ,state)) ,opcodes)
     state
     )
  )

(defun setup-labels (opcodes state)
  (if opcodes
      (typecase (first opcodes)
        (pcf2-bc:label (with-slots (str) (first opcodes)
                         (let ((state (add-label str opcodes state)))
                           (setup-labels (rest opcodes) state)
                           )
                         )
                       )
        (t 
         (setup-labels (rest opcodes) state))
        )
      state
      )
  )

(defmacro get-state-val (state idx)
  `(skew-ref ,idx (pcf2-state-memory ,state))
  )

(defmacro set-state-val (state idx val)
  `(let ((st ,state))
     (let ((newstate (make-pcf2-state :baseptr (pcf2-state-baseptr st)
                                      :iptr (pcf2-state-iptr st)
                                      :lbls (pcf2-state-lbls st)
                                      :call-stack (pcf2-state-call-stack st)
                                      :memory (skew-update ,idx ,val (pcf2-state-memory st))
                                      :alice-inputs (pcf2-state-alice-inputs ,state)
                                      :bob-inputs (pcf2-state-bob-inputs ,state)
                                      )
             )
           )
       newstate)
     )
  )
  
(defmacro find-label (state label)
  (let ((sym (gensym))
        )
    `(let ((,sym (gethash ,label (pcf2-state-lbls ,state)))
           )
       (assert ,sym)
       ,sym
       )
    )
  )

(defmacro update-state (state memory baseptr iptr call-stack)
  `(let ((st ,state))
     (let ((newstate (make-pcf2-state :baseptr (pcf2-state-baseptr st)
                                      :iptr (pcf2-state-iptr st)
                                      :lbls (pcf2-state-lbls st)
                                      :call-stack (pcf2-state-call-stack st)
                                      :memory (pcf2-state-memory st)
                                      :alice-inputs (pcf2-state-alice-inputs st)
                                      :bob-inputs (pcf2-state-bob-inputs st)
                                      )
             )
           )
       ,(if memory
            `(setf (pcf2-state-memory newstate) ,memory)
            )
       ,(if baseptr
            `(progn
               ;(format *error-output* "~%Setting base point: ~A~%" ,baseptr)
               (setf (pcf2-state-baseptr newstate) ,baseptr)
               )
            )
       ,(if iptr
            `(setf (pcf2-state-iptr newstate) ,iptr)
            )
       ,(if call-stack
            `(setf (pcf2-state-call-stack newstate) ,call-stack)
            )
       newstate)
     )
  )

(defun run-opcodes (state)
  (declare (type pcf2-state state)
           (optimize (speed 3) (debug 0))
           )
  (labels ((run-opcodes* (state)
             (let ((opcodes (pcf2-state-iptr state))
                   )
               (if (null opcodes)
                   state
                   (let* ((op (first opcodes))
                          (newstate (run-opcode state op))
                          (state (update-state newstate
                                               nil 
                                               nil 
                                               (rest (pcf2-state-iptr newstate)) 
                                               nil))
                          )
                     (run-opcodes* state)
                     )
                   )
               )
             )
           )
    (let ((state (update-state state nil nil (cons (first (pcf2-state-iptr state)) (gethash "main" (pcf2-state-lbls state) 'fail)) nil))
          )
      (run-opcodes* state)
      )
    )
  )


(defgeneric run-opcode (state opcode)
  (:documentation "Each PCF2 opcode will take in a program state and update it in some way")
  )

(defmethod run-opcode ((state pcf2-state) (opcode initbase))
  (with-slots (base) opcode
    (update-state state nil (+ (pcf2-state-baseptr state) base) nil nil)
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode call))
  (declare (optimize (debug 3) (speed 0)))
  (with-slots (newbase fname) opcode
    (let ((newbase (+ newbase (pcf2-state-baseptr state)))
          )
      ;(format *error-output* "~%Calling: ~A~%" fname)
      (cond
        ((or (string-equal fname "alice")
             (string-equal fname "bob"))
                                        ;(update-state state nil nil nil nil)
         (let ((input-pos (reduce (lambda (val b)
                                    (declare (type bit b))
                                    (+ (ash val 1) b)
                                    )
                                  (loop for i from 32 downto 1 collect
                                       (get-state-val state (+ newbase i))
                                       )
                                  )
                 )
               )
           (let ((state (reduce
                         #'(lambda (st x)
                             (set-state-val st (+ newbase x) (get-party-input state (+ input-pos x) fname))
                             )
                         (loop for i from 0 to 31 collect i)
                         :initial-value state)
                   )
                 )
             (format t "~&Input for ~A: ~A~%" fname (loop for i from 0 to 31 collect (get-state-val state (+ newbase i))))
             state
             )
           )
         )
        ((string-equal fname "output_alice")
         (format t "~&Output for Alice: ~A~%" (loop for i from 1 to 32 collect (get-state-val state (+ newbase i))))
         state
         )
        ((string-equal fname "output_bob")
         (format t "~&Output for Bob: ~A~%" (loop for i from 1 to 32 collect (get-state-val state (+ newbase i))))
         state
         )
        (t 
         (let ((new-iptr (find-label state fname))
               (call-stack (cons (cons (rest (pcf2-state-iptr state))
                                       (pcf2-state-baseptr state))
                                 (pcf2-state-call-stack state)))
               )
           ;(format t "~&Calling function ~A new base ptr: ~A~%" fname newbase)
           ;(format t "~&Call stack: ~A~%" (mapcar #'cdr call-stack))
           (update-state state nil newbase new-iptr call-stack)
           )
         )
        )
      )
    )
  )

 (defmethod run-opcode ((state pcf2-state) (opcode ret))
   (with-slots (value) opcode
     (if (pcf2-state-call-stack state)
         (let ((iptrbaseptr (first (pcf2-state-call-stack state)))
               (call-stack (rest (pcf2-state-call-stack state)))
               )
           ;(format t "~&Returning from funcall.  Setting base ptr to: ~A~%" (cdr iptrbaseptr))
           (update-state state nil (cdr iptrbaseptr) (car iptrbaseptr) call-stack) 
           )
         state
         )
     )
   )

 (defmethod run-opcode ((state pcf2-state) (opcode branch))
   (with-slots (cnd targ) opcode
     (let ((cnd-v (get-state-val state (+ (pcf2-state-baseptr state) cnd)))
           (targ (find-label state targ))
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
       (format *error-output* "Add(~A,~A,~A): ~A + ~A = ~A~%" op1 op2 dest x y (+ x y))
       (set-state-val state true-dest (+ x y))
       )
     )
   )

(defmethod run-opcode ((state pcf2-state) (opcode join))
  (with-slots (dest op1) opcode
    (let ((true-op1 (mapcar (lambda (x) (get-state-val state (+ x (pcf2-state-baseptr state)))) op1))
          (true-dest (+ dest (pcf2-state-baseptr state)))
          )
      (set-state-val state true-dest
                     (reduce (lambda (v x)
                               (declare (type bit x))
                               (+ (ash v 1) x)
                               )
                             (reverse true-op1)
                             :initial-value 0
                             )
                     )
      )
    )
  )

 (defmethod run-opcode ((state pcf2-state) (opcode bits))
   (with-slots (dest op1) opcode
     (let* ((true-op1 (+ op1 (pcf2-state-baseptr state)))
            (true-dest (mapcar
                        (lambda (x) (+ x (pcf2-state-baseptr state)))
                        dest))
            (val (get-state-val state true-op1))
            )
       (car 
        (reduce (lambda (stv x)
                  (let ((state (car stv))
                        (val (cdr stv))
                        )
                    (cons 
                     (set-state-val state x (mod val 2))
                     (ash val -1)
                     )
                    )
                  )
                true-dest :initial-value (cons state val))
        )
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
       (format t "mul: ~A * ~A = ~A~&" x y (* x y))
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
   (with-slots (dest op1 op2) opcode
     (let ((true-op1 (+ op1 (pcf2-state-baseptr state)))
           (true-dest (+ dest (pcf2-state-baseptr state)))
           )
       (reduce (lambda (state x)
                 (set-state-val state (+ x true-dest)
                                (get-state-val state (+ x true-op1))
                                )
                 )
               (loop for i from 0 to (1- op2) collect i)
              :initial-value state)
      )
    )
  )


(defmethod run-opcode ((state pcf2-state) (opcode copy-indir)
                       )
  (with-slots (dest op1 op2) opcode
    (let ((true-op1 (+ op1 (pcf2-state-baseptr state)))
          (true-dest (+ dest (pcf2-state-baseptr state)))
          )
      (reduce (lambda (state i) 
                (set-state-val state (+ i true-dest)
                               (get-state-val state
                                              (+ i (get-state-val state
                                                                  true-op1))))
                )
              (loop for i from 0 to (1- op2) collect i) :initial-value state)
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode indir-copy))
  (with-slots (dest op1 op2) opcode
    (let ((true-op1 (+ op1 (pcf2-state-baseptr state)))
          (true-dest (get-state-val state (+ dest (pcf2-state-baseptr state))))
          )
      (reduce (lambda (st i) 
                (set-state-val st
                               (+ i true-dest)
                               (get-state-val st (+ i true-op1))
                               )
                ) (loop for i from 0 to (1- op2) collect i) :initial-value state)
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode mkptr))
  (with-slots (dest) opcode
    (let ((true-dest (+ dest (pcf2-state-baseptr state)))
          )
      (set-state-val state true-dest
                     (+ (pcf2-state-baseptr state) (get-state-val state true-dest))
                     )
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode gate))
  (declare (optimize (debug 3) (speed 0)))
  (with-slots (dest op1 op2 truth-table) opcode
    (let ((true-op1 (+ op1 (pcf2-state-baseptr state)))
          (true-op2 (+ op2 (pcf2-state-baseptr state)))
          (true-dest (+ dest (pcf2-state-baseptr state)))
          )
      (let ((op1val (get-state-val state true-op1))
            (op2val (get-state-val state true-op2))
            )
        (declare (type bit op1val op2val))
        (set-state-val state true-dest
                       (bit truth-table (+ op1val (* 2 op2val))))
        )
      )
    )
  )

(defmethod run-opcode ((state pcf2-state) (opcode label))
  state
  )