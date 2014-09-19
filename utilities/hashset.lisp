;;; A minimal set implementation for speed on is-included, remove and merge operations

(defpackage :hashset
  (:use :common-lisp :utils)
  (:export new-hash-set
           hash-set
           )
  )
(in-package :hashset)

(defstruct (hashset
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "{")
                (maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) (get-hashset-table struct))
                (format stream "}")
                )
              )
             )
  (set)
  (comp)
  (:documentation "A key/value map based on an AVL tree.")
  )


(defun get-hashset-table (hashset)
  (hashset-set hashset))

(defun make-hashset (&keys (size nil) (comp #'eql))
  (make-hash-table
   :set (make-hash-table :test comp :size size)
   :comp comp)
)
