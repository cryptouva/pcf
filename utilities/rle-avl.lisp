;; Authors: Benjamin Kreuter and Benjamin Terner
;;
;; An run-length-encoded implementation of an AVL tree,
;; because Common Lisp does not come with a balanced binary tree,
;; and run-length encoding greatly improves the space efficiency
;; for one of our applications.
;;
;; for purposes that have to do with implementing this quickly,
;; nodes in this tree are only permitted to have integral idx values
;; although in a future generalization indexes may be permitted to 
;; have other "closeness" relationships for which this works.

(defpackage :rle-tree (:use :common-lisp :unit :utils)
            (:export rle-avl-insert
                     rle-avl-insert-unique
                     rle-avl-remove
                     rle-avl-search
                     rle-avl-map
                     rle-avl-reduce
                     empty-rle-avl)
            )
(in-package :rle-tree)

(defstruct (rle-avl
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "{")
                (rle-avl-map (lambda (x) (format stream "~A " x)) struct )
                (format stream "}")
                )
              )
             )
  (left)
  (right)
  (height)
  (length)
  (idx)
  (data)
)

(defparameter *default-comp* #'<)

(defun empty-rle-avl ()
  ;; this is really an empty symbol.
  ;; will need checks to ensure we don't try to insert empty trees into other trees.
  (make-rle-avl
   :left nil
   :right nil
   :idx nil
   :height 1
   :length 0
   :data nil)
  )

(defun tree-is-empty (tr)
  (and (null (avl-idx tr))
       (null (avl-left tr))
       (null (avl-right tr))))

(defun avl-height (tr)
  (aif tr
       (rle-avl-height it)
       0)
  )

(defun avl-idx (tr)
  (aif tr
       (rle-avl-idx tr)
       nil))

(defun avl-data (tr)
  (aif tr
       (rle-avl-data it)
       nil)
)

(defun avl-length (tr)
  (aif tr
       (rle-avl-length it)
       nil))

(defun avl-left (tr)
  (aif tr
      (rle-avl-left it)
      nil)
  )

(defun avl-right (tr)
  (aif tr
      (rle-avl-right it)
      nil)
  )

(defun avl-cons (idx left right &optional (data t))
  (declare (optimize (debug 3)(speed 0)))
  (make-rle-avl
   :left left
   :right right
   :height (1+ (max (aif left (avl-height it) 0) (aif right (avl-height right) 0)))
   :length 1
   :idx idx
   :data data
   )
  )

(defun node-expand-up (tr)
  (make-rle-avl
   :left (avl-left tr)
   :right (avl-right tr)
   :data (avl-data tr)
   :idx (avl-idx tr)
   :height (avl-height tr)
   :length (1+ (avl-length tr))))

(defun node-expand-down (tr)
  (make-rle-avl
   :left (avl-left tr)
   :right (avl-right tr)
   :data (avl-data tr)
   :idx (1- (avl-idx tr))
   :height (avl-height tr)
   :length (1+ (avl-length tr))))

(defun left-rotate (tr)
  "Perform a left rotation"
  (assert tr)
  (if (or (= 1 (avl-height tr)) (null (avl-right tr)))
      tr
      (avl-cons (avl-idx (avl-right tr))
                (avl-cons (avl-idx tr)
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
  (if (or (= 1 (avl-height tr)) (null (avl-left tr)))
      tr
      (avl-cons (avl-idx (avl-left tr))
                (avl-left (avl-left tr))
                (avl-cons (avl-idx tr)
                          (avl-right (avl-left tr))
                          (avl-right tr)
                          )
                )
      )
  )

(defun avl-balance (tr)
  (cond
    ((> 1 (- (avl-height (avl-left tr)) (avl-height (avl-right tr))))
                                        ; Need a left rotation
     (left-rotate (if (< (avl-height (avl-right (avl-right tr)))
                         (avl-height (avl-left (avl-right tr))))
                                        ; Need a right rotation
                      (avl-cons (avl-idx tr)
                                (avl-left tr)
                                (right-rotate (avl-right tr)))
                      tr)))
    ((< -1 (- (avl-height (avl-left tr)) (avl-height (avl-right tr))))
     (right-rotate (if (< (avl-height (avl-left (avl-left tr)))  
                          (avl-height (avl-right (avl-left tr))))
                                        ; Need a left rotation
                       (avl-cons (avl-idx tr)
                                 (left-rotate (avl-left tr))
                                 (avl-right tr))
                       tr)))
    (t tr)))
#|
(defun rle-avl-find-previous (x tr &key (comp *default-comp*))
  (if (or
       (null tr)
       (tree-is-empty tr))
      nil
      (cond
        (funcall comp x (avl-idx tr))
        
)
|#

(defun rle-avl-insert (x tr &key (comp *default-comp*) (length 1))
  "Insert a new value into an AVL tree."
  (declare (ignore length)
           ;;(optimize (debug 3)(speed 0))
           )
  (if (or
       (null tr)
       (tree-is-empty tr))
      (avl-cons x nil nil)
      (cond
        ((funcall comp x (avl-idx tr)) ;;(funcall comp x (avl-idx tr))
         (avl-balance (avl-cons
                       (avl-idx tr)
                       (rle-avl-insert x (avl-left tr) :comp comp)
                       (avl-right tr)
                      )))
        (t (avl-balance (avl-cons
                         (avl-idx tr)
                         (avl-left tr)
                         (rle-avl-insert x (avl-right tr) :comp comp)
                        ))
           ))))

(defmacro insert-unique-right-branch ()
 `(rle-avl-insert-unique x (avl-right lst) :comp comp :length length :data data :data-equiv data-equiv)
 )

(defmacro insert-unique-left-branch ()
  `(rle-avl-insert-unique x (avl-left lst) :comp comp :length length :data data :data-equiv data-equiv)
)

(defmacro insert-unique-right ()
  `(avl-balance
    (avl-cons
     (avl-idx lst)
     (avl-left lst)
     (insert-unique-right-branch)
     (avl-data lst))))

(defmacro insert-unique-left ()
  `(avl-balance (avl-cons
                (avl-idx lst)
                (insert-unique-left-branch)
                (avl-right lst)
                (avl-data lst))))


(defun rle-avl-insert-unique (x lst &key
                                      (comp *default-comp*)
                                      (length 1)
                                      (data t)
                                      (data-equiv #'equalp))
  "Insert a new value into an AVL if the value is not already present, otherwise update the value"
  (declare (optimize (debug 3)(speed 0))
           ;;(ignore length)
           )
  ;;(break)
  (if (or
       (null lst)
       (tree-is-empty lst))
      (avl-cons x nil nil);:comp comp)
      (cond
        ;;((null x) lst)
        ;; first, if x is not reachable from the previous on the low end
        ((funcall comp x (- (avl-idx lst) length))
         (insert-unique-left))
        ;; second, if x is not reachable from the previous on the high end
        ((funcall comp (+ (avl-idx lst) (avl-length lst) 1) x)
         (insert-unique-right))
        ((funcall comp (+ (avl-idx lst) (avl-length lst)) x)
         ;; x is the next on the high end after previous
         (if (funcall data-equiv lst data)
             (node-expand-down lst)
             (insert-unique-right))
         )
        ((funcall comp x (- (avl-idx lst) length))
         ;; x is before avl-idx but can reach it from its length
         (if (funcall data-equiv lst data)
             (node-expand-up lst)
             (insert-unique-left)))
         ;; To allow maps to be updated, we create a node with this input value
        (t 
         (avl-cons x
                   (avl-left lst)
                   (avl-right lst)
                   )
         )
        )
      )
  )

(defun rle-avl-remove-min (lst)
  "Find the leftmost node of the tree and remove it"
  ;;(declare (optimize (debug 3)(speed 0)))
  (if (null (avl-left lst))
      (values (avl-idx lst) (if (avl-right lst)
                                 (avl-right lst)
                                 nil))
      (multiple-value-bind (rval rlst) (rle-avl-remove-min (avl-left lst))
        (values rval (avl-cons
                      (avl-idx lst)
                      rlst
                      (avl-right lst)
                      ;;:comp (avl-comp lst)
                      )))))

(defun rle-avl-remove (x lst &key comp allow-no-result)
  "Remove a value from an AVL tree"
  ;;(declare (optimize (debug 3)(speed 0)))
  (if (tree-is-empty lst)
      (if allow-no-result
          nil
          (error 'value-not-in-tree))
      (cond
        ((funcall comp x (avl-idx lst))
         (avl-balance (avl-cons
                       (avl-idx lst)
                       (rle-avl-remove x (avl-left lst) :comp comp :allow-no-result allow-no-result)
                       (avl-right lst)
                      )))
        ((funcall comp (avl-idx lst) x)
         (avl-balance (avl-cons
                       (avl-idx lst)
                       (avl-left lst)
                       (rle-avl-remove x (avl-right lst) :comp comp :allow-no-result allow-no-result)
                       )))
        (t 
         (if (avl-right lst)
             (multiple-value-bind (rgsm rglst) (rle-avl-remove-min (avl-right lst))
               (avl-balance (avl-cons rgsm
                                        (avl-left lst)
                                        rglst
                                        )))
             (avl-left lst))))))

(defun rle-avl-search (x lst &key (comp *default-comp*))
  "Search an AVL tree for x, return true if x is in the tree"
  (if (tree-is-empty lst)
      (values nil nil)
      (cond
        ((funcall comp x (avl-idx lst))
         (rle-avl-search x (avl-left lst) :comp comp)
         )
        ((funcall comp (+ (avl-idx lst) (avl-length lst) -1) x)
         ;; subtract 1 because single nodes have length 1
         (rle-avl-search x (avl-right lst) :comp comp)
         )
        ;; We use (avl-data lst) rather than x to support implementations of maps
        (t (values t (avl-data lst)))
        )
      )
  )

(defun-ut rle-avl-map (fn tr &key (comp #'<))
  (if t
      (rle-avl-reduce (lambda (tr x)
                         (rle-avl-insert (funcall fn x) tr :comp comp))
                       tr nil)
      )
  :documentation "Create a new AVL tree by applying \"fn\" to every node in \"tr\""
  :tests 
  ((map-rebalances . (lambda ()
                       (let ((tree (rle-avl-insert 11
                                                    (rle-avl-insert 10
                                                                     (rle-avl-insert 2
                                                                                      (rle-avl-insert 3
                                                                                                       (rle-avl-insert
                                                                                                        4
                                                                                                        (empty-rle-avl)
                                                                                                        )
                                                                                                       )
                                                                                      )
                                                                     )
                                                    )
                               )
                             )
                         (let ((mapped (rle-avl-map #'(lambda (x)
                                                         (if (evenp x)
                                                             (* x 2)
                                                             x
                                                             )
                                                         )
                                                     tree))
                               )
                           (print mapped)
                           (< (avl-idx (avl-left mapped))
                              (avl-idx mapped))
                           )
                         )
                       )
                   )
   )
  )

(defun rle-avl-reduce (fn tr st)
  "Fold the values in the tree \"tr\" using the function \"fn\".  Note:  DO NOT ASSUME ANYTHING ABOUT ORDER!!!

\"fn\" should be of the form (lambda (state x) ...)"
  (declare (type function fn))
  (if (tree-is-empty tr)
      st
      (let* ((st-left (rle-avl-reduce fn (avl-left tr) st))
             (st-cur (apply fn (list st-left (avl-idx tr))))
             )
        (rle-avl-reduce fn (avl-right tr) st-cur)
        )
      )
  )

