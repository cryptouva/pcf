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

(defun avl-cons (idx left right &key (data t) (length 1))
  (declare (optimize (debug 3)(speed 0)))
  (make-rle-avl
   :left left
   :right right
   :height (1+ (max (aif left (avl-height it) 0) (aif right (avl-height right) 0)))
   :length length
   :idx idx
   :data data
   )
  )

(defun node-expand-up (tr)
  ;;(break)
  (make-rle-avl
   :left (avl-left tr)
   :right (avl-right tr)
   :data (avl-data tr)
   :idx (avl-idx tr)
   :height (avl-height tr)
   :length (1+ (avl-length tr))))

(defun node-expand-down (tr)
  ;;(break)
  (make-rle-avl
   :left (avl-left tr)
   :right (avl-right tr)
   :data (avl-data tr)
   :idx (1- (avl-idx tr))
   :height (avl-height tr)
   :length (1+ (avl-length tr))))

;; remember that i put the avl-balances into the following two functions while writing avl-remove
;; and i might want to take them out.
(defun left-rotate (tr)
  "Perform a left rotation"
  (assert tr)
  (if (or (= 1 (avl-height tr)) (null (avl-right tr)))
      tr
      (avl-cons (avl-idx (avl-right tr))
                (avl-balance
                 (avl-cons (avl-idx tr)
                           (avl-left tr)
                           (avl-left (avl-right tr))
                           ))
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
                (avl-balance
                 (avl-cons (avl-idx tr)
                           (avl-right (avl-left tr))
                           (avl-right tr)
                           ))
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
                                (right-rotate (avl-right tr))
                                :data (avl-data tr)
                                :length (avl-length tr))
                      tr)))
    ((< -1 (- (avl-height (avl-left tr)) (avl-height (avl-right tr))))
     (right-rotate (if (< (avl-height (avl-left (avl-left tr)))  
                          (avl-height (avl-right (avl-left tr))))
                                        ; Need a left rotation
                       (avl-cons (avl-idx tr)
                                 (left-rotate (avl-left tr))
                                 (avl-right tr)
                                 :data (avl-data tr)
                                 :length (avl-length tr))
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
        
)))
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
     :data (avl-data lst)
     :length (avl-length lst))))

(defmacro insert-unique-left ()
  `(avl-balance
    (avl-cons
     (avl-idx lst)
     (insert-unique-left-branch)
     (avl-right lst)
     :data (avl-data lst)
     :length (avl-length lst))))

(defun rle-avl-insert-unique (x lst &key (comp *default-comp*)(length 1)
                                      (data t) (data-equiv #'equalp))
  "Insert a new value into an AVL if the value is not already present, otherwise update the value"
  (declare (optimize (debug 3)(speed 0)))
  (if (or
       (null lst)
       (tree-is-empty lst))
      (avl-cons x nil nil);:comp comp)
      (cond
        ;; if x is less than data and cannot reach it with length
        ((funcall comp (+ x length) (avl-idx lst))
         (insert-unique-left))
        ((funcall comp (+ x length -1) (avl-idx lst))
         ;; x is before avl-idx but can reach it from its length
         (if (funcall data-equiv (avl-data lst) data)
             (node-expand-down lst)
             (progn
               (break)
               (insert-unique-left))))
        ;; if x is not reachable from the previous + its length
        ((funcall comp (+ (avl-idx lst) (avl-length lst)) x)
         (insert-unique-right))
        ((funcall comp (+ (avl-idx lst) (avl-length lst) -1) x)
         ;; x is the next on the high end after previous
         (if (funcall data-equiv (avl-data lst) data)
             (node-expand-up lst)
             (progn 
               (break)
               (insert-unique-right))))
         ;; To allow maps to be updated, we create a node with this input value
        (t 
         (if (funcall data-equiv data (avl-data lst))
             lst ;; don't need to do anything to the list.
             (rle-avl-insert-unique x (rle-avl-remove x lst
                                                  :comp comp
                                                  :allow-no-result nil
                                                  :length length)
                                :comp comp :length length
                                :data data :data-equiv data-equiv))))))

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

(defun rle-avl-remove (x lst &key (comp *default-comp*) (allow-no-result nil) (length 1))
  "Remove a value from a RLE-AVL tree"
  ;;(declare (optimize (debug 3)(speed 0)))
  (if (tree-is-empty lst)
      (if allow-no-result
          nil
          (error 'value-not-in-tree))
      (cond
        ((funcall comp (+ x length -1) (avl-idx lst))
         (avl-balance (avl-cons
                       (avl-idx lst)
                       (rle-avl-remove x (avl-left lst) :comp comp
                                       :allow-no-result allow-no-result :length length)
                       (avl-right lst))))
        ((funcall comp (+ (avl-idx lst) (avl-length lst) -1) x)
         (avl-balance (avl-cons
                       (avl-idx lst)
                       (avl-left lst)
                       (rle-avl-remove x (avl-right lst) :comp comp
                                       :allow-no-result allow-no-result :length length))))
        (t 
         (cond 
           ((equal length (avl-length lst)) ;; then we can do remove-min as per a usual avl-tree    
            (if (avl-right lst)
                (multiple-value-bind (rgsm rglst) (rle-avl-remove-min (avl-right lst))
                  (avl-balance (avl-cons rgsm
                                         (avl-left lst)
                                         rglst)))
                (avl-left lst)))
           ;; one of the lengths is not 1
           ((> length (avl-length lst))
            (error "cannot remove more than the length of the target node"))
           (t ;; (avl-length lst) is not 1
            ;; build a new tree with the stuff that came before, followed by the stuff that came after
            (let ((prev-remain (- x (avl-idx lst))) ;; the length of items remaining after cutting at x
                  (succ-remain (- (+ (avl-idx lst) (avl-length lst)) (+ x length))) ;; length of items remaining after resuming where x left off
                  (left (avl-left lst))
                  (right (avl-right lst)))
              (cond
                ((and (zerop succ-remain) (zerop prev-remain))
                 nil)
                ((zerop prev-remain)
                 (avl-cons (+ x length)
                           left
                           right
                           :data (avl-data lst)
                           :length succ-remain))
                ((zerop succ-remain)
                 (avl-cons (avl-idx lst)
                           left
                           right
                           :data (avl-data lst)
                           :length prev-remain))
                (t
                 (rle-avl-insert-unique (avl-idx lst)
                                        (avl-cons (+ x length)
                                                  left
                                                  right
                                                  :data (avl-data lst)
                                                  :length succ-remain)
                                        :data (avl-data lst)
                                        :length prev-remain))))
            ))))))

(defun rle-avl-search (x lst &key (comp *default-comp*))
  "Search an AVL tree for x, return true if x is in the tree"
  (declare (optimize (debug 3)(speed 0)))
  (break)
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
        (t (values t (avl-idx lst)))
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
             (st-cur (reduce (lambda (state x)
                               (apply fn (list state x)))
                             (loop for i from (avl-idx tr) to (+ (avl-idx tr) (avl-length tr) -1) collect i)
                             :initial-value st-left))
             )
        (rle-avl-reduce fn (avl-right tr) st-cur)
        )
      )
  )

