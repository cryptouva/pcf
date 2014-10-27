;; Authors: Benjamin Kreuter and Benjamin Terner
;;
;; This implements sets and maps more efficiently than the standard CL
;; way (which uses linked lists).

(defpackage :setmap-rle (:use :rle-tree :common-lisp)
            (:export set-member
                     empty-set
                     singleton
                     set-insert
                     set-remove
                     set-diff
                     set-diff-efficient
                     set-union
                     set-inter
                     set-map           
                     set-from-list
                     list-from-set
                     set-equalp
                     set-reduce
		     set-subset
                     set-filter
                     map-empty
                     map-singleton
                     map-insert
                     map-keys
                     map-vals
                     map-upsert
                     map-remove
                     map-find
                     map-val
                     map-map
                     map-reduce
                     map-filter
                     map-fold-forward
                     map-fold-backward
                     #|
                     rle-avl-set
                     alist->map
                     alist->map*
                     |#)
            )
(in-package :setmap-rle)

(defstruct (rle-avl-set
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "{")
                ;; (format stream "Tree: ")
                (rle-avl-map (lambda (x y)
                               (format stream "(~A.~A) " x y))
                             (rle-avl-set-tree struct)
                             :key-val t) 
                ;;(format stream "~%")
                ;;(format stream "Comp: ~A" (rle-avl-set-comp struct))
                (format stream "}")
                )
              )
             )
  (tree)
  (comp)
  (:documentation "A key/value map based on an AVL tree.")
  )

(defparameter *default-comp* #'<)

(defmacro empty-set (&key comp)
  (if comp
      ;;`(make-rle-avl-set :tree (empty-avl-tree :comp ,comp) :comp ,comp)
      ;;`(make-rle-avl-set :tree (empty-avl-tree :comp #'<) :comp #'<)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp ,comp)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp *default-comp*)
      )
  )


(defun set-member (x st)
  "Check if \"x\" is contained in \"set\""
  (multiple-value-bind (y v) (rle-avl-search x (rle-avl-set-tree st) :comp (rle-avl-set-comp st))
    (and y (or
            (equalp x v)
            (funcall (rle-avl-set-comp st) v x)))
    )
  )

(defun singleton (x &key (comp #'<))
  "Create a new singleton set"
  (make-rle-avl-set :tree
                ;;(avl-tree-insert x (empty-avl-tree :comp comp) :comp comp)
                    (rle-avl-insert-unique x (empty-rle-avl) :comp comp)
                    :comp comp)
  )

(defun set-diff (set1 set2)
  "Compute the set containing only elements contained in \"set1\" but not \"set2\""
  (declare (type rle-avl-set set1 set2))
  (assert (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2)))
  (let ((comp (rle-avl-set-comp set1))
        )
    (make-rle-avl-set :tree 
                  (rle-avl-reduce (lambda (st k v)
                                     (if (set-member k set2)
                                         st
                                         (rle-avl-insert-unique k st :comp comp :data v)
                                         )
                                     )
                                   (rle-avl-set-tree set1)
                                   nil)
                  :comp comp)
    )
  )


(defun set-diff-efficient (set1 set2)
  "Quickly remove all elements in set2 from set1, where set1 is much larger than set2"
  (declare (type rle-avl-set set1 set2))
  (assert (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2)))
  (let ((comp (rle-avl-set-comp set1)))
    (make-rle-avl-set :tree
                  (rle-avl-reduce (lambda (st k v)
                                    (declare (ignore v))
                                     (if (set-member k set1)
                                         (rle-avl-remove k st :comp comp)
                                         st)
                                     )
                                   (rle-avl-set-tree set2)
                                   (rle-avl-set-tree set1))
                  :comp comp)
    )
    ;; (set-reduce (lambda (st x)
    ;;               (if (set-member x set1)
    ;;                 (set-remove st x)
    ;;                 st))
    ;;             set1
    ;;             set2)
    
)

(defun set-subset (set1 set2)
  (declare (type rle-avl-set set1 set2))
  (cond
    ((not (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2))) nil)
    (t (rle-avl-reduce (lambda (x k v)
                         (declare (ignore v))
                          (and x (set-member k set2))
                          )
                        (rle-avl-set-tree set1)
                        t)
       )
    )
  )

(defun set-equalp (set1 set2)
  (declare (type rle-avl-set set1 set2))
  (and (set-subset set1 set2)
       (set-subset set2 set1)
       )
  )

(defun set-union (set1 set2)
  "Compute the union of \"set1\" and \"set2\""
  (declare (type rle-avl-set set1 set2)
           (optimize (debug 3)(speed 0)))
  (assert (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2)))
  (let ((comp (rle-avl-set-comp set1)))
    (make-rle-avl-set :tree
                  (rle-avl-reduce (lambda (st x v)
                                     (rle-avl-insert-unique x st :comp comp :data v))
                                   (rle-avl-set-tree set1)
                                   (rle-avl-set-tree set2))
                  :comp comp
                  )))

(defun set-insert (set x)
  (declare (optimize (debug 3)(speed 0)))
  (let ((comp (rle-avl-set-comp set)))
    (make-rle-avl-set :tree
                  (rle-avl-insert-unique x (rle-avl-set-tree set) :comp comp)
                  :comp comp))
  )

(defun set-remove (set x &optional (allow-no-result t))
  "Remove key \"x\" from the set"
  (declare (optimize (debug 3)(speed 0)))
   (let ((comp (rle-avl-set-comp set)))
     (make-rle-avl-set :tree
                   (rle-avl-remove x (rle-avl-set-tree set) :comp comp :allow-no-result allow-no-result)
                   :comp comp)))

(defun set-from-list (lst &key (comp #'<))
  "Create a set that contains the elements of \"lst\""
  (declare (type list lst)
           (type (function (t t) boolean) comp))
  (reduce (lambda (st x)
            (set-insert st x))
          lst
          :initial-value (empty-set :comp comp)))

(defun list-from-set (st)
  (set-reduce (lambda (st x v)
                (declare (ignore v))
                (cons x st)
                )
              st
              nil)
  )

(defun set-inter (set1 set2)
  "Compute the intersection of \"set1\" and \"set2\""
  (declare (type rle-avl-set set1 set2))
  (assert (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2)))
  (let ((comp (rle-avl-set-comp set1))
        )
    (make-rle-avl-set :tree 
                  (rle-avl-reduce (lambda (st k v)
                                    (if (set-member k set2)
                                         (rle-avl-insert-unique k st :comp comp :data v)
                                         st
                                         )
                                    )
                                  (rle-avl-set-tree set1)
                                  nil)
                  :comp comp)
    )
  )

(defun set-map (fn st)
  "Compute the image of \"st\" under \"fn\""
  (make-rle-avl-set :tree (rle-avl-map fn (rle-avl-set-tree st)
                                         :comp (rle-avl-set-comp st))
                     :comp (rle-avl-set-comp st))
  )


(defun set-reduce (fn st state)
  "Fold \"st\" over \"fn\""
  ;; fn should be of type (lambda (state key val))
  (rle-avl-reduce fn (rle-avl-set-tree st) state)
  )


(defun set-filter (fn st)
  "Filter \"st\" to produce the subset of all elements for which \"fn\" is true"
  (set-reduce (lambda (s k v)
                (declare (ignore v))
                (if (funcall fn k)
                    (set-insert s k)
                    s)
                )
              st 
              (make-rle-avl-set :tree nil :comp (rle-avl-set-comp st))
              )
  )

;;;
;;; Maps
;;;

(defmacro map-empty (&key (comp nil))
  (if (null comp)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp *default-comp*)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp ,comp)
      )
  )

(defmacro map-singleton (x y &key (comp nil))
  (if (null comp)
      `(map-insert ,x ,y (map-empty))
      `(map-insert ,x ,y (map-empty :comp comp) :comp comp)))

(defun map-keys (mp)
  (declare (type rle-avl-set mp))
  (map-reduce (lambda (state key val)
                (declare (ignore val))
                ;;(set-insert state key))
                (cons key state))
              mp
              nil ;;(empty-set :comp cmp)
              ))


(defun map-vals (mp &key (cmp #'<))
  (declare (type rle-avl-set mp) (type function cmp))
  (map-reduce (lambda (state key val)
                (declare (ignore key))
                (set-insert state val))
              mp
              (empty-set :comp cmp)))

(defun map-insert (x y mp)
  "Insert \"x -> y\" into the map \"mp\", returning the new map containing x->y"
  (declare (type rle-avl-set mp)
           (optimize (debug 3)(speed 0)))
  (break)
  (let ((comp (rle-avl-set-comp mp))
        )
    (make-rle-avl-set :tree
                  (rle-avl-insert-unique  x
                                          (rle-avl-set-tree mp)
                                          :comp comp
                                          :data y)
                  :comp comp)
    )
  )


(defun map-upsert (x y mp)
  (map-insert x y mp)
  )

(defun map-remove (x mp)
  "Remove the element with key \"x\" from the map \"mp\""
  (declare (optimize (debug 3)(speed 0)))
  (let ((comp (rle-avl-set-comp mp))
        )
    (make-rle-avl-set :tree
                  (rle-avl-remove x (rle-avl-set-tree mp) :comp comp)
                  :comp comp)
    )
  )

(defun map-find (x mp &optional (allow-no-result nil))
  "Search the map \"mp\" for the key \"x\".  If found, return the value; else return nil"
  (declare (optimize (debug 3)(speed 0)))
  (let ((comp (rle-avl-set-comp mp)))
    (multiple-value-bind (found value) (rle-avl-search x
                                                       (rle-avl-set-tree mp) 
                                                       :comp comp)
      (if (and (not allow-no-result) (not found))
          (error "Object not found in map")
          )
      (if (not found)
          nil
          (cons x value)))))

(defun map-val (x mp &optional (allow-no-result nil))
  (let ((val (map-find x mp allow-no-result)))
    (if (null val)
        nil
        (cdr val))))

(defun map-map (fn mp)
  "Apply \"fn\" to each element of the map \"mp\" to create a new map.

\"fn\" should have the form (lambda (key value) ...) and return the new value"
  (declare (type (function (t t) t) fn)
           (type rle-avl-set mp))
  (make-rle-avl-set :tree (rle-avl-map (lambda (k v)
                                         (cons k (funcall fn k v)))
                                       (rle-avl-set-tree mp)
                                       :comp (rle-avl-set-comp mp)
                                       :key-val t)
                    :comp (rle-avl-set-comp mp)))


(defun map-fold (fn keys mp st)
  "fn should have the form (lambda (state key val)), keys should be the keys in order"
  (if (null keys)
      st
      (map-fold fn
                (cdr keys) 
                mp 
                (funcall fn st (car keys) (cdr (map-find (car keys) mp))))))

(defun map-fold-forward (fn mp st)
  (map-fold fn (reverse (map-keys mp)) mp st)
  )

(defun map-fold-backward (fn mp st)
  (map-fold fn (map-keys mp) mp st)
  )


(defun map-reduce (fn mp st)
  "Fold the map \"mp\" over the function \"fn\"

\"fn\" should have the form (lambda (state key value) ...)"
  (declare (type (function (t t t) t) fn)
           (type rle-avl-set mp))
  ;;(break)
  (rle-avl-reduce (lambda (st k v)
                    (funcall fn st k v))
                  (rle-avl-set-tree mp)
                  st)
  )

(defun map-filter (fn mp)
  "Filter \"mp\" to produce the submap of all elements for which \"fn\" is true"
  (map-reduce (lambda (m x y)
                (if (funcall fn x y) ;; functions should be of form (key val)
                    (map-insert x y m)
                    m))
              mp 
              (map-empty :comp (rle-avl-set-comp mp))))
#|
(defun alist->map (alist &key (comp (carcomp #'eql)))
  "Convert an associative list to a map.  Note that the default
comparison operation is eql, which is the default for associative
lists."
  (labels ((make-map (lst &optional (ret (make-rle-avl-set :tree nil :comp (carcomp comp))))
             (if lst
                 (make-map (rest lst) (map-insert (caar lst) (cdar lst) ret))
                 ret
                 )
             )
           )
    (make-map alist)
    )
  )

(defun alist->map* (alist &key empty-m)
  "Convert an associative list to a map, using a specified empty map
object.  The purpose of this function is to allow us to maintain the
same function for all our maps."
  (labels ((make-map (lst &optional (ret empty-m))
             (if lst
                 (make-map (rest lst) (map-insert (caar lst) (cdar lst) ret))
                 ret
                 )
             )
           )
    (make-map alist)
    )
  )
|#
