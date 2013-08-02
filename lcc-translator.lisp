;; Translate LCC opcodes to PCF2 opcodes.  Also removes forward jumps
;; and creates the appropriate muxes.

(defpackage :lcc-translator (:use 
                             :pcf2-bc 
                             :common-lisp))
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