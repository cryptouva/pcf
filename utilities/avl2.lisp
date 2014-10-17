;; Authors: Benjamin Kreuter and Benjamin Terner
;;
;; An implementation of an AVL tree, because Common Lisp does not come
;; with a balanced binary tree.

(defpackage :tree (:use :common-lisp :unit)
            (:export avl-tree-insert
                     avl-tree-insert-unique
                     avl-tree-remove
                     avl-tree-search
                     avl-tree-map
                     avl-tree-reduce
                     empty-avl-tree)
            )
(in-package :tree)


(defstruct (avl-tree
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth));; (ignore struct))
                (format stream "{")
                (avl-tree-map (lambda (x) (format stream "~A  " x)) struct :comp (avl-tree-comp struct))
                (format stream "}")
                )
              )
             )
  (left)
  (right)
  (height)
  (comp)
  (data)
)

(defparameter *default-comp* #'<)

(defun empty-avl-tree (&key (comp *default-comp*))
  (make-avl-tree
   :left nil
   :right nil
   :height 1
   :comp comp
   :data nil)
  )

(defmacro tree-is-empty (tr)
  `(and (null (avl-data ,tr))
        (null (avl-left ,tr))
        (null (avl-right ,tr))))

(defmacro avl-height (tr)
  `(if ,tr
       (avl-tree-height ,tr)
       0)
  )

(defmacro avl-data (tr)
  `(if ,tr
       (avl-tree-data ,tr)
       nil)
)

(defmacro avl-left (tr)
  `(if ,tr
      (avl-tree-left ,tr)
      nil)
  )

(defmacro avl-right (tr)
  `(if ,tr
      (avl-tree-right ,tr)
      nil)
  )

(defmacro avl-comp (tr)
  `(if ,tr
      (avl-tree-comp ,tr)
      nil)
)

(defun avl-cons (data left right &key (comp *default-comp*))
  (assert (or
           (or (null left) (null right))
           (equalp (avl-comp left) (avl-comp right))))
  (make-avl-tree
   :left left
   :right right
   :data data
   :height (max (avl-height left) (avl-height right))
   :comp comp))

(defun left-rotate (tr)
  "Perform a left rotation"
  (assert tr)
  (if (or (= 1 (avl-height tr)) (null (avl-right tr)))
      tr
      (avl-cons (avl-data (avl-right tr))
                (avl-cons (avl-data tr)
                          (avl-left tr)
                          (avl-left (avl-right tr))
                          :comp (avl-comp tr)
                          )
                (avl-right (avl-right tr))
                )
      )
  )

(defun right-rotate (tr)
  "Perform a right rotation"
  (assert tr)
  (if (or (= 1 (avl-height tr)) (null (avl-left tr)))
      tr
      (avl-cons (avl-data (avl-left tr))
                (avl-left (avl-left tr))
                (avl-cons (avl-data tr)
                          (avl-right (avl-left tr))
                          (avl-right tr)
                          :comp (avl-comp tr)
                          )
                )
      )
  )

(defun avl-balance (tr)
  (declare (optimize (debug 3)(speed 0)))
  (cond
    ((> 1 (- (avl-height (avl-left tr)) (avl-height (avl-right tr))))
                                        ; Need a left rotation
     (left-rotate (if (< (avl-height (avl-right (avl-right tr)))
                         (avl-height (avl-left (avl-right tr))))
                                        ; Need a right rotation
                      (avl-cons (avl-data tr)
                                (avl-left tr)
                                (right-rotate (avl-right tr))
                                :comp (avl-comp tr))
                      tr)))
    ((< -1 (- (avl-height (avl-left tr)) (avl-height (avl-right tr))))
     (right-rotate (if (< (avl-height (avl-left (avl-left tr)))  
                          (avl-height (avl-right (avl-left tr))))
                                        ; Need a left rotation
                       (avl-cons (avl-data tr)
                                 (left-rotate (avl-left tr))
                                 (avl-right tr)
                                 :comp (avl-comp tr))
                       tr)))
    (t tr)))

#|
(defun avl-list-cons (x lst)
  "Using an AVL tree representation of a list, perform cons"
  (if (null lst)
      (avl-cons x
                nil
                nil)
      (avl-balance (avl-cons (avl-data lst) (avl-list-cons x (avl-left lst)) (avl-right lst)))
      )
  )
|#

(defun avl-tree-insert (x tr &key (comp nil))
  (declare (optimize (debug 3)(speed 0)))
  (if (null comp)
      (avl-insert x tr :comp (avl-comp tr))
      (avl-insert x tr :comp comp)))

(defun avl-insert (x tr &key comp)
  "Insert a new value into an AVL tree."
  (declare (optimize (debug 3)(speed 0)))
  (if (or
       (null tr)
       (tree-is-empty tr))
      (avl-cons x nil nil :comp comp)
      (cond
        ((null x) tr)
        ((funcall comp x (avl-data tr))
         (avl-balance (avl-cons
                       (avl-data tr)
                       (avl-insert x (avl-left tr) :comp comp)
                       (avl-right tr)
                       :comp comp)))
        (t (avl-balance (avl-cons
                         (avl-data tr)
                         (avl-left tr)
                         (avl-insert x (avl-right tr) :comp comp)
                         :comp comp))))))

(defun avl-tree-insert-unique (x lst &key (comp nil))
  (if (null comp)
      (avl-insert-unique x lst :comp (avl-comp lst))
      (avl-insert-unique x lst :comp comp)))
                         
(defun avl-insert-unique (x lst &key comp)
  "Insert a new value into an AVL if the value is not already present, otherwise update the value"
  (declare (optimize (debug 3)(speed 0)))
  (if (or
       (null lst)
       (tree-is-empty lst))
      (avl-cons x nil nil :comp comp)
      (cond
        ((null x) lst)
        ((funcall comp x (avl-data lst))
         (avl-balance (avl-cons
                       (avl-data lst)
                       (avl-insert-unique x (avl-left lst) :comp comp)
                       (avl-right lst)
                       :comp comp))
         )
        ((funcall comp (avl-data lst) x)
         (avl-balance (avl-cons
                       (avl-data lst)
                       (avl-left lst)
                       (avl-insert-unique x (avl-right lst) :comp comp)
                       :comp comp))
         )
        ;; To allow maps to be updated, we create a node with this input value
        (t 
         (avl-cons x
                   (avl-left lst)
                   (avl-right lst)
                   :comp comp))
        )
      )
  )

(defun avl-tree-remove-min (lst)
  "Find the leftmost node of the tree and remove it"
  (declare (optimize (debug 3)(speed 0)))
  (if (null (avl-left lst))
      (values (avl-data lst) (if (avl-right lst)
                                 (avl-right lst)
                                 nil))
      (multiple-value-bind (rval rlst) (avl-tree-remove-min (avl-left lst))
        (values rval (avl-cons
                      (avl-data lst)
                      rlst
                      (avl-right lst)
                      :comp (avl-comp lst))))))

(defun avl-tree-remove (x lst &key (comp nil) (allow-no-result nil))
  (if (null comp)
      (avl-remove x lst 
                  :comp (avl-comp lst)
                  :allow-no-result allow-no-result)
      (avl-remove x lst 
                  :comp comp
                  :allow-no-result allow-no-result)))

(defun avl-remove (x lst &key comp allow-no-result)
  "Remove a value from an AVL tree"
  (declare (optimize (debug 3)(speed 0)))
  (if (tree-is-empty lst)
      (if allow-no-result
          nil
          (error 'value-not-in-tree))
      (cond
        ((funcall comp x (avl-data lst))
         (avl-balance (avl-cons
                       (avl-data lst)
                       (avl-remove x (avl-left lst) :comp comp :allow-no-result allow-no-result)
                       (avl-right lst)
                       :comp comp)))
        ((funcall comp (avl-data lst) x)
         (avl-balance (avl-cons
                       (avl-data lst)
                       (avl-left lst)
                       (avl-remove x (avl-right lst) :comp comp :allow-no-result allow-no-result)
                       :comp comp)))
        (t 
         (if (avl-right lst)
             (multiple-value-bind (rgsm rglst) (avl-tree-remove-min (avl-right lst))
               (avl-balance (avl-cons rgsm
                                        (avl-left lst)
                                        rglst
                                        :comp comp)))
             (avl-left lst))))))

(defun avl-tree-search (x lst &key (comp nil))
  (if (null comp)
      (avl-search x lst :comp (avl-comp lst))
      (avl-search x lst :comp comp)))

(defun avl-search (x lst &key comp)
  "Search an AVL tree for x, return true if x is in the tree"
  (if (tree-is-empty lst)
      (values nil nil)
      (cond
        ((funcall comp x (avl-data lst))
         (avl-search x (avl-left lst) :comp comp)
         )
        ((funcall comp (avl-data lst) x)
         (avl-search x (avl-right lst) :comp comp)
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
                                                                                                        (empty-avl-tree)
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
  (declare (type function fn))
  (if (tree-is-empty tr)
      st
      (let* ((st-left (avl-tree-reduce fn (avl-left tr) st))
             (st-cur (apply fn (list st-left (avl-data tr))))
             )
        (avl-tree-reduce fn (avl-right tr) st-cur)
        )
      )
  )
