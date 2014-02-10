;; Basic pointer analysis framework for LCC.  This will make other
;; dataflow more precise and allow us to handle conditional function
;; calls correctly, among other things.  Basically, we need to be able
;; to determine that we have a local pointer, at the very least, and
;; if that local pointer points to a constant (which is definitely
;; something we want to know).

(defpackage :lcc-ptranl
  (:use :cl
        :utils
        :lcc-dataflow
        :lcc-bc
        :setmap)
  (:export ptranl-dataflow-funs)
  (:shadow export import)
  )
(in-package :lcc-ptranl)

;; We need to represent the pointers on the stack i.e. those created
;; by addr* instructions somehow.  Also, we will need to create
;; well-defined notions of different pointer types e.g. global, args,
;; local, more than just using special symbols.
;;
;; For the stack, we have a special location 'stack that we use to
;; identify the top of the stack.  Operations that push to the top of
;; the stack will kill this value and replace it with one of their
;; own.  Operations that pop from the stack will need us to actually
;; keep track of the stack top for them, as a stack, but we only need
;; to do this for operations that actually deal with pointers
;; i.e. only operations that *expect* a pointer from the top of the
;; stack will ever need to actually pop, and only operations that push
;; a pointer to the top of the stack will ever need to push.

(defun ptranl-dataflow-funs (ops)
  "Compute the points-to relations from ops"
  (declare (optimize (debug 3) (Speed 0)))
  (let ((funs (lcc-dataflow:get-function-body ops))
        )
    (setmap:map-map (lambda (k v)
                      (declare (ignore k))
                      (multiple-value-bind (ptrs stack) (lcc-dataflow:flow-forwards #'ptr-join-fn 
                                                                                    #'ptr-flow-fn
                                                                                    (lcc-dataflow:make-cfg-single-ops v)
                                                                                    (setmap:map-empty :comp string<) 
                                                                                    (setmap:map-empty :comp string<) 
                                                                                    (setmap:map-empty :comp string<) 
                                                                                    (cons 
                                                                                     (empty-set :comp ptrcomp)
                                                                                     (empty-set :comp ptrcomp)
                                                                                     )
                                                                                    )
                        (cons ptrs stack)
                        )
                                                   ;(setmap:set-from-list (loop for i from 0 to 8 collect (* 4 i))))
                      )
                    funs)
    )
  )

(defun ptr-join-fn (x y)
  (cons
   (set-union (car x) (car y))
   (set-inter (cdr x) (cdr y))
   )
  )

(defun ptr-flow-fn (in-set in-stack valmap bb &optional (lsize 8))
;  (print (basic-block-ops bb))
;  (format *error-output* "~&~A" in-stack)
  (if (zerop (length (basic-block-ops bb)))
      (list in-stack (cons (empty-set :comp ptrcomp) (empty-set :comp ptrcomp)))
      (progn
        (assert (= 1 (length (basic-block-ops bb))))
        (let ((may-in (car in-set))
              (must-in (cdr in-set))
              )
          (let ((may-out (set-union (set-diff may-in
                                              (set-from-list (car (kill (first (basic-block-ops bb)) in-stack must-in lsize)) :comp #'ptrcomp)
                                              )
                                    (set-from-list (car (gen (first (basic-block-ops bb)) in-stack may-in lsize)) :comp #'ptrcomp)
                                    )
                  )
                (must-out (set-union (set-diff must-in
                                               (set-from-list (car (kill (first (basic-block-ops bb)) in-stack may-in lsize)) :comp #'ptrcomp)
                                               )
                                     (set-from-list (car (gen (first (basic-block-ops bb)) in-stack must-in lsize)) :comp #'ptrcomp)
                                     )
                  )
                (stck-out (stck (first (basic-block-ops bb)) may-in in-stack))
                )
            ;(print stck-out)
            (list stck-out (cons may-out must-out) valmap)
            )
          )
        )
      )
  )

(defmacro def-gen-kill (type &key stck)
  `(locally
       (declare (optimize (debug 3) (speed 0)))
     (defmethod gen ((op ,type) stack ptrs lsize)
       (the (cons list (cons list null)) 
         (list (list-from-set 
                (set-union 
                 (constgen
                  (constleftl op stack)
                  (constrightl op stack)
                  )
                 (depgen
                  (constleftl op stack)
                  (depleftl op ptrs stack)
                  (constrightl op stack)
                  (deprightl op ptrs stack)
                  )
                 )
                ) 
               (stck op ptrs stack))
         )
       )

     (defmethod kill ((op ,type) stack ptrs lsize)
       (the (cons list (cons list null)) 
         (list 
          (list-from-set
           (set-union 
            (constkill
             (constleftl op stack)
             lsize
             )
            (constkill
             (depleftl op ptrs stack)
             lsize
             )
            )
           )
          (stck op ptrs stack))
         )
       )
     )
  )

(def-gen-kill lcc-instruction
    :stck stack
    )

(def-gen-kill argp
    :stck (cddr stack)
    )

(def-gen-kill addrgp
    :stck (cons glob stack)
    )

(def-gen-kill addrfp
    :stck (progn  (cons args stack))
    )

(def-gen-kill addrlp
    :stck (the (cons integer t)
            (cons
             (parse-integer (second (slot-value op 's-args))) stack))
    )

(def-gen-kill indirp
    :stck (locally 
              (declare (optimize (debug 3) (speed 0)))
            (if (eql (first stack) args)
                (cons glob (cdr stack))
                (let ((st (list-from-set (set-filter (lambda (x)
                                                       (equalp (first stack) (car x))
                                                       )
                                                     ptrs
                                                     )
                                         )
                        )
                      )
                  (assert (= 1 (length st)))
                  (cons 
                                        ;(aif (first st)
                                        ;     it
                                        ;     )
                   (first st)
                   (cdr stack)
                   )
                  )
                )
            )
    )

(def-gen-kill asgnp
    :stck (cddr stack)
    )

(def-gen-kill cnd-jump-instruction
    :stck (cdr stack)
    )

(def-gen-kill asgnu
    :stck (cdr stack)
    )

(def-gen-kill asgni
    :stck (cdr stack)
    )

(def-gen-kill indiri
    :stck (cdr stack)
    )

(def-gen-kill indiru
    :stck (cdr stack)
    )

(def-gen-kill callu
    :stck (cdr stack)
    )

(def-gen-kill calli
    :stck (cdr stack)
    )

(def-gen-kill callv
    :stck (cdr stack)
    )

(defun ptrcomp (x y)
  (declare (type (cons (or symbol integer)
                       (or symbol integer))
                 x y))
  (cond
    ((equalp (car x) (car y))
     (cond
       ((equalp (cdr x) (cdr y)) nil)
       ((and (equalp (cdr x) glob)
             (equalp (cdr y) args)) t)
       ((typep (cdr x) 'symbol) t)
       ((typep (cdr y) 'symbol) nil)
       (t (< (cdr x) (cdr y)))
       )
     )
    ((equalp (car x) 'stack) t)
    ((equalp (car y) 'stack) nil)
    ((< (car x) (car y)) t)
    ((= (car x) (car y))
     (< (cdr x) (cdr y)))
    )
  )

(defun constgen (constL constR)
  (reduce
   (lambda (st x)
     (set-union st
                (set-from-list (mapcar (lambda (y)
                                         (cons x y)
                                         )
                                       constR)
                               :comp #'ptrcomp)
                )
     )
   constL
   :initial-value (empty-set :comp ptrcomp)
   )
  )

(defun constkill (constL &optional (maxaddr 32))
  (declare (optimize (debug 3) (speed 0)))
  (reduce (lambda (st x)
                (set-union st
                           (set-from-list 
                            (cons
                             (cons x glob)
                             (loop for i from 0 to maxaddr collect 
                                  (cons x
                                        i
                                        )
                                  )
                             )
                            :comp #'ptrcomp
                            )
                           )
                )
              constL
              :initial-value (empty-set :comp ptrcomp)
              )
  )

(defun depgen (constL depL constR depR)
  (let ((st1 (reduce (lambda (st x)
                           (set-union st
                                      (set-from-list (mapcar (lambda (y)
                                                               (cons x y)
                                                               )
                                                             depL)
                                                     :comp #'ptrcomp)
                                      )
                           )
                         constL
                         :initial-value (empty-set :comp ptrcomp)
                         )
          )
        )
    (let ((st2 (reduce (lambda (st x)
                         (set-union st
                                    (set-from-list (mapcar (lambda (y)
                                                             (cons x y)
                                                             )
                                                           constR)
                                                   :comp #'ptrcomp)
                                    )
                         )
                       depL
                       :initial-value st1)
            )
          )
      (reduce (lambda (st x)
                (set-union st
                           (set-from-list (mapcar (lambda (y)
                                                    (cons x y)
                                                    )
                                                  depR)
                                          :comp #'ptrcomp)
                           )
                )
              depL
              :initial-value st2)
      )
    )
  )

(defstruct ptr
  (type)
  (loc)
  )

(defgeneric constleftl (op stack)
  )

(defgeneric depleftl (op ptrs stack)
  )

(defgeneric constrightl (op stack)
  )

(defgeneric deprightl (op ptrs stack)
  )

(defgeneric stck (op ptrs stack)
  )

(defmacro def-left-right (type &key constleftl depleftl constrightl deprightl stck)
  `(locally
       (declare (optimize (debug 3) (speed 0)))
     (defmethod constleftl ((op ,type) stack)
       (the list ,constleftl)
       )
     (defmethod depleftl ((op ,type) ptrs stack)
       (the list ,depleftl)
       )
     (defmethod constrightl ((op ,type) stack)
       (the list ,constrightl)
       )
     (defmethod deprightl ((op ,type) ptrs stack)
       (the list ,deprightl)
       )
     (defmethod stck ((op ,type) ptrs stack)
       (the list ,(aif stck
                       it
                       'stack))
       )
     )
  )

(def-left-right lcc-instruction
    )

(def-left-right callu
    :stck (cdr stack)
    )

(def-left-right calli
    :stck (cdr stack)
    )

(def-left-right callv
    :stck (cdr stack)
    )

(def-left-right argp
    :stck (cddr stack)
    )

;; Indirp
;;
;; First stack arg = ptr
(def-left-right indirp
    ;; :constleftl (list 'stack)
    ;; :deprightl (let ((ys (set-reduce (lambda (st x)
    ;;                                    (if (equalp (first stack) (car x))
    ;;                                        (set-insert st (cdr x))
    ;;                                        st
    ;;                                        )
    ;;                                    )
    ;;                                  ptrs 
    ;;                                  (empty-set :comp ptrcomp)))
    ;;                  )
    ;;              (set-reduce (lambda (st x)
    ;;                            (if (set-member (car x) ys)
    ;;                                (cons 
    ;;                                 (the (or symbol (integer 0)) (cdr x))
    ;;                                 st)
    ;;                                st
    ;;                                )
    ;;                            )
    ;;                          ptrs 
    ;;                          nil
    ;;                          )
    ;;              )
    :stck (if (or (set-member args (first stack))
                  (set-member glob (first stack)))
              stack
              (let ((ys (set-reduce (lambda (st x)
                                      (if (set-member (car x) (first stack))
                                          (set-insert st (cdr x))
                                          st
                                          )
                                      )
                                    ptrs
                                    (empty-set)
                                    )
                      )
                    )
                (cons (set-reduce (lambda (st x)
                                    (if (set-member (car x) ys)
                                        (set-insert st (cdr x))
                                        st
                                        )
                                    )
                                  ptrs
                                  (empty-set)
                                  )
                      (cdr stack)
                      )
                )
              )
    )


(def-left-right indiri
    :stck (cdr stack)
    )

(def-left-right indiru
    :stck (cdr stack)
    )

(def-left-right asgnp
    :constleftl (list-from-set (second stack))
    :constrightl (list-from-set (first stack))
    :stck (cddr stack)
    )

(def-left-right asgni
    :constleftl (list-from-set (first stack))
    :stck (cdr stack)
    )

(def-left-right asgnu
    :constleftl (list-from-set (first stack))
    :stck (cdr stack)
    )

(def-left-right addrlp
    :stck (cons (with-slots (s-args) op
                   (set-insert (empty-set) (parse-integer (second s-args)))
                   ) 
                stack)
    )

(defconstant args -1000)
(defconstant glob -2000)

(def-left-right addrfp
    :stck (cons (set-insert (empty-set) args) stack)
    )

(def-left-right addrgp
    :stck (cons (set-insert (empty-set) glob) stack)
    )

(def-left-right cnd-jump-instruction
    :stck (cdr stack)
    )