;; Author: Benjamin Kreuter
;;
;; An implementation of an AVL tree, because Common Lisp does not come
;; with a balanced binary tree.

(defpackage :tree (:use :common-lisp :unit)
            (:export avl-tree-insert
                     avl-tree-insert-unique
                     avl-tree-remove
                     avl-tree-search
                     avl-tree-map
                     avl-tree-reduce)
            )
(in-package :tree)

(defmacro avl-height (tr)
  `(the fixnum
     (if (null ,tr)
         0
         (car ,tr)
         )
     )
  )

(defmacro avl-data (tr)
  `(cadr ,tr)
  )

(defmacro avl-left (tr)
  `(caddr ,tr)
  )

(defmacro avl-right (tr)
  `(cdddr ,tr)
  )

(defmacro avl-cons (data left right)
  (let ((_right (gensym))
        (_left (gensym))
        (_data (gensym))
        )
    `(let ((,_right ,right)
           (,_left ,left)
           (,_data ,data))
       (cons (locally (declare (optimize (safety 0))) (the fixnum (1+ (max (avl-height ,_left) (avl-height ,_right)))))
             (cons ,_data
                   (cons ,_left ,_right)))
       )
    )
  )

(defun left-rotate (tr)
  "Perform a left rotation"
  (assert tr)
  (if (or (= 1 (car tr)) (null (avl-right tr)))
      tr
      (avl-cons (avl-data (avl-right tr))
                (avl-cons (avl-data tr)
                          (avl-left tr)
                          (avl-left (avl-right tr))
                          )
                (avl-right (avl-right tr))
                )
      )
  )

(defun right-rotate (tr)
  "Perform a right rotation"
  (assert tr)
  (if (or (= 1 (car tr)) (null (avl-left tr)))
      tr
      (avl-cons (avl-data (avl-left tr))
                (avl-left (avl-left tr))
                (avl-cons (avl-data tr)
                          (avl-right (avl-left tr))
                          (avl-right tr)
                          )
                )
      )
  )

(defun avl-balance (tr)
  (cond
    ((> -1 (- (avl-height (avl-left tr)) (avl-height (avl-right tr))))
                                        ; Need a left rotation
     (left-rotate (if (< (avl-height (avl-right (avl-right tr)))
                         (avl-height (avl-left (avl-right tr))))
                                        ; Need a right rotation
                      (avl-cons (avl-data tr)
                                (avl-left tr)
                                (right-rotate (avl-right tr)))
                      tr)
                  )
     )
    ((< 1 (- (avl-height (avl-left tr)) (avl-height (avl-right tr))))
     (right-rotate (if (< (avl-height (avl-left (avl-left tr)))  
                          (avl-height (avl-right (avl-left tr))))
                        ; Need a left rotation
                       (avl-cons (avl-data tr)
                                 (left-rotate (avl-left tr))
                                 (avl-right tr))
                       tr))
     )
    (t tr)
    )
  )

(defun avl-list-cons (x lst)
  "Using an AVL tree representation of a list, perform cons"
  (if (null lst)
      (avl-cons x
                nil
                nil)
      (avl-balance (avl-cons (avl-data lst) (avl-list-cons x (avl-left lst)) (avl-right lst)))
      )
  )

(defun avl-tree-insert (x lst &key (comp #'<))
  "Insert a new value into an AVL tree."
  (if (null lst)
      (avl-cons x nil nil)
      (if (funcall comp x (avl-data lst))
          (avl-balance (avl-cons
                        (avl-data lst)
                        (avl-tree-insert x (avl-left lst) :comp comp)
                        (avl-right lst)))
          (avl-balance (avl-cons
                        (avl-data lst)
                        (avl-left lst)
                        (avl-tree-insert x (avl-right lst) :comp comp)))
          )
      )
  )

(defun avl-tree-insert-unique (x lst &key (comp #'<))
  "Insert a new value into an AVL if the value is not already present"
  (declare (optimize (debug 0) (speed 3))
           (type function comp))
  (if (null lst)
      (avl-cons x nil nil)
      (cond
        ((funcall comp x (avl-data lst))
         (avl-balance (avl-cons
                        (avl-data lst)
                        (avl-tree-insert-unique x (avl-left lst) :comp comp)
                        (avl-right lst)))
         )
        ((funcall comp (avl-data lst) x)
          (avl-balance (avl-cons
                        (avl-data lst)
                        (avl-left lst)
                        (avl-tree-insert-unique x (avl-right lst) :comp comp)))
         )
        ;; To allow maps to be updated, we create a node with this input value
        (t (avl-cons x
                     (avl-left lst)
                     (avl-right lst)))
        )
      )
  )

(defun avl-tree-remove-min (lst)
  "Find the leftmost node of the tree and remove it"
  (if (null (avl-left lst))
      (values (avl-data lst) nil)
      (multiple-value-bind (rval rlst) (avl-tree-remove-min (avl-left lst))
        (values rval (avl-cons
                      (avl-data lst)
                      rlst
                      (avl-right lst)
                      )
                )
        )
      )
  )

(defun avl-tree-remove (x lst &key (comp #'<))
  "Remove a value from an AVL tree"
  (if (null lst)
      (error 'value-not-in-tree)
      (cond
        ((funcall comp x (avl-data lst))
         (avl-balance (avl-tree-remove x (avl-left lst) :comp comp)))
        ((funcall comp (avl-data lst) x)
         (avl-balance (avl-tree-remove x (avl-right lst) :comp comp)))
        (t 
         (if (avl-right lst)
             (multiple-value-bind (rgsm rglst) (avl-tree-remove-min (avl-right lst))
               (avl-balance (avl-cons rgsm
                                      (avl-left lst)
                                      rglst)
                            )
               )
             (avl-left lst)
             )
         )
        )
      )
  )

(defun avl-tree-search (x lst &key (comp #'<))
  "Search an AVL tree for x, return true if x is in the tree"
  (if (null lst)
      (values nil nil)
      (cond
        ((funcall comp x (avl-data lst))
         (avl-tree-search x (avl-left lst) :comp comp)
         )
        ((funcall comp (avl-data lst) x)
         (avl-tree-search x (avl-right lst) :comp comp)
         )
        ;; We use (avl-data lst) rather than x to support implementations of maps
        (t (values t (avl-data lst)))
        )
      )
  )

(defun-ut avl-tree-map (fn tr &key (comp #'<))
  (if t
      (avl-tree-reduce (lambda (tr x)
                         (avl-tree-insert (funcall fn x) tr :comp comp))
                       tr nil)
      )
  :documentation "Create a new AVL tree by applying \"fn\" to every node in \"tr\""
  :tests 
  ((map-rebalances . (lambda ()
                       (let ((tree (avl-tree-insert 11
                                                    (avl-tree-insert 10
                                                                     (avl-tree-insert 2
                                                                                      (avl-tree-insert 3
                                                                                                       (avl-tree-insert
                                                                                                        4
                                                                                                        nil
                                                                                                        )
                                                                                                       )
                                                                                      )
                                                                     )
                                                    )
                               )
                             )
                         (let ((mapped (avl-tree-map #'(lambda (x)
                                                         (if (evenp x)
                                                             (* x 2)
                                                             x
                                                             )
                                                         )
                                                     tree))
                               )
                           (print mapped)
                           (< (avl-data (avl-left mapped))
                              (avl-data mapped))
                           )
                         )
                       )
                   )
   )
  )

(defun avl-tree-reduce (fn tr st)
  "Fold the values in the tree \"tr\" using the function \"fn\".  Note:  DO NOT ASSUME ANYTHING ABOUT ORDER!!!

\"fn\" should be of the form (lambda (state x) ...)"
  (declare (optimize (debug 0) (speed 3))
           (type function fn))
  (if (null tr)
      st
      (let* ((st-left (avl-tree-reduce fn (avl-left tr) st))
             (st-cur (apply fn (list st-left (avl-data tr))))
             )
        (avl-tree-reduce fn (avl-right tr) st-cur)
        )
      )
  )