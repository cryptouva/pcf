;; Skew binary random access list implementation, based on Okasaki's
;; thesis.

(defpackage :skew-list (:use :common-lisp)
            (:export skew-cons
                     skew-first
                     skew-rest
                     skew-ref
                     skew-update
                     skew-map
                     skew-reduce
                     skew-reverse
                     skew-append
                     skew-list
                     list-skew)
            )
(in-package :skew-list)

(defmacro lf (x)
  `(cons ,x (cons nil nil))
  )

(defmacro br (x t1 t2)
  `(cons ,x (cons ,t1 ,t2))
  )

(defmacro left (tr)
  `(cadr ,tr)
  )

(defmacro right (tr)
  `(cddr ,tr)
  )

(defmacro root (tr)
  `(cadr ,tr)
  )

(defun skew-cons (x lst)
  "Similar to skew-inc, but also adds x to a binary tree.  The size of
the tree is recorded as the digit of the skew binary number."
  (if (>= (length lst) 2)
      (if (= (car (first lst)) (car (second lst)))
          (cons (cons (+ 1 (car (first lst)) (car (second lst)))
                      (br x (cdr (first lst)) (cdr (second lst)))
                      )
                (nthcdr 2 lst))
          (cons (cons 1 (lf x)) lst)
          )
      (cons (cons 1 (lf x)) lst)
      )
  )

(defun skew-first (lst)
  "Straightforward:  the root of the first tree."
  (if (null lst)
      nil
      (root (first lst))
      )
  )

(defun skew-rest (lst)
  "Similar to skew-dec, but must also take the left and right halves
of the trees if the first tree has more than one element."
  (if (null lst)
      nil
      (if (= 1 (car (first lst)))
          (rest lst)
          (cons
           (cons (floor (/ (car (first lst)) 2))
                 (left (cdr (first lst))))
           (cons (cons (floor (/ (car (first lst)) 2))
                       (right (cdr (first lst))))
                 (rest lst))
           )
          )
      )
  )

(defun skew-ref (idx lst)
  "Look up the idxth element of a skew-binary random access list."
  (labels ((tree-lookup (w i tr)
             (cond
               ((zerop i) (car tr))
               ((= 1 w) (error 'index-out-of-bounds))
               (t (if (<= i (floor (/ w 2)))
                      (tree-lookup (floor (/ w 2)) (1- i) (left tr))
                      (tree-lookup (floor (/ w 2)) (- i 1 (floor (/ w 2))) (right tr))
                      )
                  )
               )
             )
           )
    (cond
      ((null lst) (error 'index-out-of-bounds))
      ((< idx (car (first lst)))
       (tree-lookup (car (first lst)) idx (cdr (first lst))))
      (t (skew-ref (- idx (car (first lst))) (rest lst)))
      )
    )
  )


(defun skew-update (idx y lst)
  "Replace the element at position idx in lst with y"
  (declare (optimize (debug 0) (speed 3)))
  (labels ((tree-update (w i y tr)
             (cond
               ((and (= 1 w) (zerop i)) (lf y))
               ((= 1 w) (error 'index-out-of-bounds))
               ((zerop i) (cons y (cdr tr)))
               (t
                (if (<= i (floor (/ w 2)))
                    (br (car tr) 
                        (tree-update (floor (/ w 2)) (1- i) y (left tr)) 
                        (right tr))
                    (br (car tr) 
                        (left tr) 
                        (tree-update (floor (/ w 2)) (- i 1 (floor (/ w 2))) y (right tr)))
                    )
                )
               )
             )
           )
    (cond
      ((null lst) (error 'index-out-of-bounds))
      ((< idx (car (first lst)))
       (cons (cons (caar lst) (tree-update (caar lst) idx y (cdar lst)))
             (cdr lst)))
      (t (cons (car lst) (skew-update (- idx (caar lst)) y (rest lst)))
         )
      )
    )
  )

(defun skew-list (lst)
  "Convert a list into a skew-binary random access list."
  (if (null lst)
      nil
      (skew-cons (first lst) (skew-list (rest lst)))
      )
  )

(defun list-skew (lst)
  (if (null lst)
      nil
      (cons (skew-first lst) (list-skew (skew-rest lst)))
      )
  )

(defun skew-map (fn lst)
  "Perform a map operation on a skew-binary random access list."
  (declare (optimize (debug 0) (speed 3)) (type function fn))
  (labels ((skew-map* (lst res)
             (if (null lst)
                 (skew-reverse res)
                 (skew-map* (skew-rest lst) (skew-cons (funcall fn (skew-first lst)) res))
                 )
             )
           )
    (skew-map* lst nil)
    )
  )

(defun skew-reduce* (fn lst &optional (init 0))
  (declare (optimize (debug 3) (speed 0)))
  (if (null lst)
      init
      (funcall fn (skew-reduce* fn (skew-rest lst) init) (skew-first lst))
      )
  )   

(defun skew-reverse (lst &optional (ret nil))
  "Simple implementation of reverse for skew-binary random access lists."
  (if (null lst)
      ret
      (skew-reverse (skew-rest lst)
                    (skew-cons (skew-first lst) ret))
      )
  )

(defun skew-reduce (fn lst &optional (init 0))
  "Reduce operation for skew-binary random access lists."
;  (skew-reduce* fn (skew-reverse lst) init)
  (declare 
   (type function fn)
   (optimize (debug 3) (speed 0)))
  (if (null lst)
      init
      (let ((init (funcall fn init (skew-first lst)))
            )
        (skew-reduce fn (skew-rest lst) init)
        )
      )
  )

(defun skew-append (lst1 lst2)
  "Append one skew-binary random access list to another."
  (if (null lst1)
      lst2
      (skew-cons (skew-first lst1)
                 (skew-append (skew-rest lst1) lst2)
                 )
      )
  )