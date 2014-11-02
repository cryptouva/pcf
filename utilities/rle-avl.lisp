;; Authors: Benjamin Kreuter and Benjamin Terner
;;
;; An run-length-encoded implementation of an AVL tree,
;; because Common Lisp does not come with a balanced binary tree,
;; and run-length encoding greatly improves the space efficiency
;; for one of our applications. It also seems to have some performance benefits
;; because tree traversals are generally shorter (even with the extra overhead
;; with removing nodes at insertion)
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
                     rle-avl-node-search
                     rle-avl-search-val
                     rle-avl-map
                     rle-avl-reduce
                     rle-avl-node-reduce ;; ONLY for special-purpose efficient functions
                     avl-data ;; ONLY for special-purpose efficient functions
                     avl-length ;; ONLY for special-purpose efficient functions
                     avl-idx ;; ONLY for special-purpose efficient functions
                     empty-rle-avl)
            )
(in-package :rle-tree)

(defstruct (rle-avl
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "{")
                ;;(rle-avl-map (lambda (idx data) (format stream "(~A.~A)" idx data)) struct :key-val t)
                (rle-avl-node-reduce (lambda (st node)
                                       (format stream "(~A.~A.~A)" (avl-idx node) (avl-data node) (avl-length node))) struct t)
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
;;  (declare (optimize (debug 3)(speed 0)))
  (make-rle-avl
   :left left
   :right right
   :height (1+ (max (aif left (avl-height it) 0) (aif right (avl-height right) 0)))
   :length length
   :idx idx
   :data data
   )
  )

(defun node-expand-up (tr x xlen)
  ;; this function can go at most 2 deep (base call and then a recursive one)
  ;;  (declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (if (and (avl-right tr)
           (equal (avl-data tr) (avl-data (avl-right tr)))
           (equal (avl-idx (avl-right tr)) (+ (avl-idx tr) (avl-length tr) xlen)))
      (avl-cons
       (avl-idx tr)
       (avl-left tr)
       (avl-balance
        (rle-avl-remove (avl-idx (avl-right tr))
                    (avl-right tr)
                    :length (avl-length (avl-right tr))))
       :data (avl-data tr)
       :length (+ (avl-length tr) xlen (avl-length (avl-right tr))))
      (make-rle-avl
       :left (avl-left tr)
       :right (avl-right tr)
       :data (avl-data tr)
       :idx (avl-idx tr)
       :height (avl-height tr)
       :length (- (+ x xlen)
                  (avl-idx tr))))
)

(defun node-expand-down (tr x xlen)
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (if (and (avl-left tr)
           (equal (avl-data tr) (avl-data (avl-left tr)))
           (equal (avl-idx (avl-left tr))
                  (- (avl-idx tr) xlen (avl-length (avl-left tr)))))
      (avl-cons
       (avl-idx (avl-left tr))
       (avl-balance
        (rle-avl-remove (avl-idx (avl-left tr))
                        (avl-left tr)
                        :length (avl-length (avl-left tr))))
       (avl-right tr)
       :data (avl-data tr)
       :length (+ (avl-length tr) xlen (avl-length (avl-left tr))))
      (make-rle-avl
       :left (avl-left tr)
       :right (avl-right tr)
       :data (avl-data tr)
       :idx x
       :height (avl-height tr)
       :length (- (max (+ (avl-idx tr) (avl-length tr)) (+ x xlen))
                  x)))
)


;; remember that i put the avl-balances into the following two functions while writing avl-remove
;; and i might want to take them out.
(defun left-rotate (tr)
  "Perform a left rotation"
 ;; (assert tr)
  (if (or (null tr) (= 1 (avl-height tr)) (null (avl-right tr)))
      tr
      (avl-cons (avl-idx (avl-right tr))
                ;;(avl-balance
                 (avl-cons (avl-idx tr)
                           (avl-left tr)
                           (avl-left (avl-right tr))
                           :data (avl-data tr)
                           :length (avl-length tr))
                 ;;)
                 (avl-right (avl-right tr))
                 :data (avl-data (avl-right tr))
                 :length (avl-length (avl-right tr)))))

(defun right-rotate (tr)
  "Perform a right rotation"
  (assert tr)
  (if (or (= 1 (avl-height tr)) (null (avl-left tr)))
      tr
      (avl-cons (avl-idx (avl-left tr))
                (avl-left (avl-left tr))
                ;;(avl-balance
                 (avl-cons (avl-idx tr)
                           (avl-right (avl-left tr))
                           (avl-right tr)
                           :data (avl-data tr)
                           :length (avl-length tr)
                           )
                 ;;)
                 :data (avl-data (avl-left tr))
                 :length (avl-length (avl-left tr)))))

(defun avl-balance (tr)
  ;;(declare (optimize (debug 3)(speed 0)))
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


(defmacro insert-right-branch ()
 `(rle-avl-insert x (avl-right lst) :comp comp :length length :data data :data-equiv data-equiv)
 )

(defmacro insert-left-branch ()
  `(rle-avl-insert x (avl-left lst) :comp comp :length length :data data :data-equiv data-equiv)
)

(defmacro insert-right ()
  `(avl-balance
    (avl-cons
     (avl-idx lst)
     (avl-left lst)
     (insert-right-branch)
     :data (avl-data lst)
     :length (avl-length lst))))

(defmacro insert-left ()
  `(avl-balance
    (avl-cons
     (avl-idx lst)
     (insert-left-branch)
     (avl-right lst)
     :data (avl-data lst)
     :length (avl-length lst))))


(defmacro insert-unique-branch (branch)
  `(rle-insert-unique x ,branch :comp comp :length length :data data :data-equiv data-equiv)
)

(defmacro insert-unique-right ()
  `(avl-balance
    (avl-cons
     (avl-idx lst)
     (avl-left lst)
     (insert-unique-branch (avl-right lst))
     :data (avl-data lst)
     :length (avl-length lst))))

(defmacro insert-unique-left ()
  `(avl-balance
    (avl-cons
     (avl-idx lst)
     (insert-unique-branch (avl-left lst))
     (avl-right lst)
     :data (avl-data lst)
     :length (avl-length lst))))

(defun rle-avl-insert-unique (x lst &key (comp *default-comp*)(length 1)
                                      (data t) (data-equiv #'equalp))
  ;;(declare (optimize (debug 3)(speed 0)))
  (multiple-value-bind (found tree) (rle-avl-node-search x lst :comp comp)
    ;;(declare (optimize (debug 3)(speed 0)))
    ;;(break)
    (if (and found (funcall data-equiv data (avl-data tree)))
        (if (and ;; x must be >= than avl-idx and (x+length) must be <= (avl-idx + avl-length)
             (funcall comp (- (avl-idx tree) 1) x)
             (funcall comp (+ x length) (+ (avl-idx tree) (avl-length tree) 1)))
            lst ;; in order to simply return the tree (nothing to insert)
            (rle-insert-unique x 
                               (rle-avl-remove x lst :comp comp :allow-no-result t :length length) 
                               ;lst
                               :comp comp :length length :data data :data-equiv data-equiv))
         (rle-insert-unique x 
                               (rle-avl-remove x lst :comp comp :allow-no-result t :length length) 
                               ;lst
                               :comp comp :length length :data data :data-equiv data-equiv))))

;;(rle-insert-unique x lst :comp comp :length length :data data :data-equiv data-equiv))))

#|   (rle-insert-unique x (if found
(rle-avl-remove x lst :comp comp :allow-no-result nil :length length) 
lst)|#

(defmacro tree-insert ()
  `(if (or
       (null lst)
       (tree-is-empty lst))
      (avl-cons x nil nil :data data :length length);:comp comp)
      (cond
        ;; if x is less than data and cannot reach it with length
        ((funcall comp (+ x length) (avl-idx lst))
         (insert-unique-left))
        ((funcall comp (+ (avl-idx lst) (avl-length lst)) x)
         (insert-unique-right))
        ((funcall comp (+ x length -1) (avl-idx lst))
         ;; x is before avl-idx but can reach it from its length
         (if (funcall data-equiv (avl-data lst) data)
             (node-expand-down lst x length)
             (insert-unique-left)))
        ;; if x is not reachable from the previous + its length
        ((funcall comp (+ (avl-idx lst) (avl-length lst) -1) x)
         ;; x is the next on the high end after previous
         (if (funcall data-equiv (avl-data lst) data)
             (node-expand-up lst x length)
             (insert-unique-right)))
         ;; To allow maps to be updated, we create a node with this input value
        (t 
         (if (funcall data-equiv data (avl-data lst))
             ;; at least part of the insert is redundant
             (cond
               ((funcall comp x (avl-idx lst))
                ;; x < first node, expand down from avl-idx
                (node-expand-down lst x length)
                )
               ((funcall comp (+ (avl-idx lst) (avl-length lst) -1) (+ x length -1))
                ;; x + length > last node, expand up from last
                (node-expand-up lst x length)
                )
               (t
                (error "should have already found data equivalence"))) ;; don't need to do anything to the list.
             (rle-insert-unique x lst
                                ;; (rle-avl-remove x lst
                                ;;                 :comp comp
                                ;;                 :allow-no-result nil
                                ;;                 :length length)
                                :comp comp :length length
                                :data data :data-equiv data-equiv))))))

(defun rle-avl-insert (x lst &key (comp *default-comp*)(length 1)
                                      (data t) (data-equiv #'equalp))
  "Insert a new value into an AVL tree."
  ;;(declare ;;(ignore length)
  ;; (optimize (debug 3)(speed 0))
   ;;        )
  (tree-insert))

(defun rle-insert-unique (x lst &key (comp *default-comp*)(length 1)
                                      (data t) (data-equiv #'equalp))
  "Insert a new value into an AVL if the value is not already present, otherwise update the value"
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (tree-insert))

(defun rle-avl-remove-min (lst)
  "Find the leftmost node of the tree and remove it"
  ;;(declare (optimize (debug 3)(speed 0)))
  (if (null (avl-left lst))
      (values (avl-idx lst)
              (if (avl-right lst)
                  (avl-right lst)
                  nil)
              (avl-data lst)
              (avl-length lst)
              )
      (multiple-value-bind (rval rlst rdata rlength) (rle-avl-remove-min (avl-left lst))
        (values rval (avl-cons
                      (avl-idx lst)
                      rlst
                      (avl-right lst)
                      :data (avl-data lst)
                      :length (avl-length lst)
                      ;;:comp (avl-comp lst)
                      )
                rdata rlength
                ))))

(defun rle-avl-remove (x lst &key (comp *default-comp*) (allow-no-result t) (length 1))
  (if (equal length 0)
      lst
      (multiple-value-bind (found tree) (rle-avl-node-search x lst :comp comp)
        (if found
            (rle-avl-remove
             (+ x (avl-length tree))
             (rle-avl-remove-item x lst :comp comp :allow-no-result allow-no-result :length (min length (avl-length tree)))
             :comp comp
             :length (- length (min length (avl-length tree)))
             )
            (rle-avl-remove (+ x 1) lst :comp comp :length (- length 1))))))

(defun rle-avl-remove-item (x lst &key (comp *default-comp*) (allow-no-result nil) (length 1))
  "Remove a value from a RLE-AVL tree"
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (if (tree-is-empty lst)
      (if allow-no-result
          nil
          (error "value not in tree"))
      (cond
        ((funcall comp (+ x length -1) (avl-idx lst))
         (avl-balance (avl-cons
                       (avl-idx lst)
                       (rle-avl-remove x (avl-left lst) :comp comp
                                       :allow-no-result allow-no-result :length length)
                       (avl-right lst)
                       :length (avl-length lst)
                       :data (avl-data lst))))
        ((funcall comp (+ (avl-idx lst) (avl-length lst) -1) x)
         (avl-balance (avl-cons
                       (avl-idx lst)
                       (avl-left lst)
                       (rle-avl-remove x (avl-right lst) :comp comp
                                       :allow-no-result allow-no-result :length length)
                       :length (avl-length lst)
                       :data (avl-data lst))))
        (t
         (cond 
           ((equal length (avl-length lst)) ;; then we can do remove-min as per a usual avl-tree    
            (if (avl-right lst)
                (multiple-value-bind (rval rlst rdata rlength) (rle-avl-remove-min (avl-right lst))
                  (avl-balance (avl-cons rval
                                         (avl-left lst)
                                         rlst
                                         :data rdata
                                         :length rlength)))
                (avl-left lst)))
           ;; lengths not equal
           ((> length (avl-length lst))
            ;; this could signal a bug if i haven't combined consecutive pieces of the same data
            (error "cannot remove more than the length of the target node"))
           (t ;; (avl-length lst) is not 1
            ;; build a new tree with the stuff that came before, followed by the stuff that came after
            (let ((prev-remain (- x (avl-idx lst))) ;; the length of items remaining after cutting at x
                  (succ-remain (- (+ (avl-idx lst) (avl-length lst)) (+ x length))) ;; length of items remaining after resuming where x left off
                  (left (avl-left lst))
                  (right (avl-right lst)))
              (cond
                ((and (zerop succ-remain) (zerop prev-remain))
                 (error "lengths should not be equal"))
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
                 (avl-balance (avl-cons (avl-idx lst)
                                        (avl-left lst)
                                        (avl-balance
                                         (avl-cons (+ x length)
                                                   nil
                                                   (avl-right lst)
                                                   :data (avl-data lst)
                                                   :length succ-remain))
                                        :data (avl-data lst)
                                        :length prev-remain))))
              )))))))

  
(defun rle-avl-search (x lst &key (comp *default-comp*))
  "Search an AVL tree for x, return true if x is in the tree"
  ;;(declare (optimize (debug 3)(speed 0)))
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
        ;;(t (values t x))
        (t (values t (avl-data lst)))
        )
      )
  )

(defun rle-avl-node-search (x lst &key (comp *default-comp*))
  "Search an AVL tree for the tree rooted with the node containing x"
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (if (tree-is-empty lst)
      (values nil nil)
      (cond
        ((funcall comp x (avl-idx lst))
         (rle-avl-node-search x (avl-left lst) :comp comp)
         )
        ((funcall comp (+ (avl-idx lst) (avl-length lst) -1) x)
         ;; subtract 1 because single nodes have length 1
         (rle-avl-node-search x (avl-right lst) :comp comp)
         )
        (t 
         ;;(break)
         (values t lst))
        )
      )
  )


(defun rle-avl-search-val (x lst &key (comp *default-comp*))
  "Search an AVL tree for x, return true if x is in the tree"
  ;;(declare (optimize (debug 3)(speed 0)))
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
        ;; We use (avl-data lst) to support implementations of maps
        (t (values t (avl-data lst)))
        )
      )
  )

(defun rle-avl-map (fn tr &key (comp #'<) (key-val nil))
 (if key-val
     (rle-avl-reduce (lambda (tr k v)
                       (let ((res (funcall fn k v)))
                         (rle-avl-insert (car res) tr :comp comp :data (cdr res))))
                     tr nil)
     (rle-avl-reduce (lambda (tr k v)
                       (declare (ignore v))
                       (rle-avl-insert (funcall fn k) tr :comp comp))
                     tr nil)))
#|
(defun-ut rle-avl-map (fn tr &key (comp #'<) (key-val nil))
  ;; if key-val is set, fn takes two arguments: key and value, and returns a new value
  ;; if key-val is not set, fn takes one argument and returns a new value
  (if key-val
       (rle-avl-reduce (lambda (tr k v)
                         (let ((res (funcall fn k v)))
                           (rle-avl-insert (car res) tr :comp comp :data (cdr res))))
                       tr nil)
       (rle-avl-reduce (lambda (tr k v)
                         (declare (ignore v))
                         (rle-avl-insert (funcall fn k) tr :comp comp))
                       tr nil))
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
                         (let ((mapped (rle-avl-map #'(lambda (x v)
                                                        (declare (ignore v))
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
|#

(defun rle-avl-reduce (fn tr st)
  "Fold the values in the tree \"tr\" using the function \"fn\".  Note:  DO NOT ASSUME ANYTHING ABOUT ORDER!!!

\"fn\" should be of the form (lambda (state idx data) ...)"
  (declare (type function fn))
  ;;(break)
  (if (tree-is-empty tr)
      st
      (let* ((st-left (rle-avl-reduce fn (avl-left tr) st))
             (st-cur (reduce (lambda (state x)
                               (funcall fn state (car x) (cdr x)))
                             (loop for i from (avl-idx tr) to (+ (avl-idx tr) (avl-length tr) -1) collect (cons i (avl-data tr)))
                             :initial-value st-left))
             )
        (rle-avl-reduce fn (avl-right tr) st-cur)
        )
      )
  )


(defun rle-avl-node-reduce (fn tr st)
    "Fold the nodes in the tree \"tr\" using the function \"fn\".  Note:  DO NOT ASSUME ANYTHING ABOUT ORDER!!!

\"fn\" should be of the form (lambda (state node) ...)"
  (declare (type function fn))
  ;;(break)
  (if (tree-is-empty tr)
      st
      (let* ((st-left (rle-avl-node-reduce fn (avl-left tr) st))
             (st-cur (funcall fn st-left tr))
             )
        (rle-avl-node-reduce fn (avl-right tr) st-cur)
        )
      )
)
