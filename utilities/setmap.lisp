;; Author: Benjamin Kreuter
;;
;; This implements sets and maps more efficiently than the standard CL
;; way (which uses linked lists).

(defpackage :setmap (:use :tree :common-lisp)
            (:export set-member
                     singleton
                     set-insert
                     set-remove
                     set-diff
                     set-union
                     set-inter
                     set-map
                     set-from-list
                     list-from-set
                     set-equalp
                     set-reduce
		     set-subset
                     set-filter
                     map-keys
                     map-vals
                     map-insert
		     map-upsert
                     map-remove
                     map-find
                     map-val
                     map-map
                     map-reduce
                     map-empty
                     map-filter
                     map-singleton
                     empty-set
                     avl-set
                     alist->map
                     alist->map*
                     map-fold-forward
                     map-fold-backward)
            )
(in-package :setmap)

(defstruct (avl-set
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "{")
                (avl-tree-map (lambda (x) (format stream "~A " x)) (avl-set-tree struct) :comp (avl-set-comp struct))
                (format stream "}")
                )
              )
             )
  (tree)
  (comp)
  (:documentation "A key/value map based on an AVL tree.")
  )

(defmacro empty-set (&key comp)
  (if comp
      `(make-avl-set :tree nil :comp ,comp)
      `(make-avl-set :tree nil :comp #'<)
      )
  )


(defun set-member (x st)
  "Check if \"x\" is contained in \"set\""
  (multiple-value-bind (y v) (avl-tree-search x (avl-set-tree st) :comp (avl-set-comp st))
    (and y (equalp x v))
    )
  )

(defun singleton (x &key (comp #'<))
  "Create a new singleton set"
  (make-avl-set :tree
                (avl-tree-insert x nil :comp comp)
                :comp comp)
  )

(defun set-diff (set1 set2)
  "Compute the set containing only elements contained in \"set1\" but not \"set2\""
  (declare (type avl-set set1 set2))
  (assert (equalp (avl-set-comp set1) (avl-set-comp set2)))
  (let ((comp (avl-set-comp set1))
        )
    (make-avl-set :tree 
                  (avl-tree-reduce (lambda (st x)
                                     (if (set-member x set2)
                                         st
                                         (avl-tree-insert-unique x st :comp comp)
                                         )
                                     )
                                   (avl-set-tree set1)
                                   nil)
                  :comp comp)
    )
  )

(defun set-subset (set1 set2)
  (declare (type avl-set set1 set2))
  (cond
    ((not (equalp (avl-set-comp set1) (avl-set-comp set2))) nil)
    (t (avl-tree-reduce (lambda (x y)
                          (and x (set-member y set2))
                          )
                        (avl-set-tree set1)
                        t)
       )
    )
  )

(defun set-equalp (set1 set2)
  (declare (type avl-set set1 set2))
  (and (set-subset set1 set2)
       (set-subset set2 set1)
       )
  )

(defun set-union (set1 set2)
  "Compute the union of \"set1\" and \"set2\""
  (declare (type avl-set set1 set2)
           (optimize (debug 3)(speed 0)))
  (assert (equalp (avl-set-comp set1) (avl-set-comp set2)))
  (let ((comp (avl-set-comp set1)))
    (make-avl-set :tree
                  (avl-tree-reduce (lambda (st x)
                                     (avl-tree-insert-unique x st :comp comp))
                                   (avl-set-tree set1)
                                   (avl-set-tree set2))
                  :comp comp
                  )))

(defun set-insert (set x)
  (let ((comp (avl-set-comp set)))
    (make-avl-set :tree
                  (avl-tree-insert-unique x (avl-set-tree set) :comp comp)
                  :comp comp))
  ;; (set-union set (singleton x :comp (avl-set-comp set)))
  )

(defun set-remove (set x &optional (allow-no-result t))
  "Remove key \"x\" from the set"
  (declare (optimize (debug 3)(speed 0)))
   (let ((comp (avl-set-comp set)))
     (make-avl-set :tree
                   (avl-tree-remove x (avl-set-tree set) :comp comp :allow-no-result allow-no-result)
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
  (set-reduce (lambda (st x)
                (cons x st)
                )
              st nil)
  )

(defun set-inter (set1 set2)
  "Compute the intersection of \"set1\" and \"set2\""
  (declare (type avl-set set1 set2))
  (assert (equalp (avl-set-comp set1) (avl-set-comp set2)))
  (let ((comp (avl-set-comp set1))
        )
    (make-avl-set :tree 
                  (avl-tree-reduce (lambda (st x)
                                     (if (set-member x set2)
                                         (avl-tree-insert-unique x st :comp comp)
                                         st
                                         )
                                     )
                                   (avl-set-tree set1)
                                   nil)
                  :comp comp)
    )
  )

(defun set-map (fn st)
  "Compute the image of \"st\" under \"fn\""
  (make-avl-set :tree (avl-tree-map fn (avl-set-tree st) :comp (avl-set-comp st)) :comp (avl-set-comp st))
  )

(defun set-reduce (fn st state)
  "Fold \"st\" over \"fn\""
  (avl-tree-reduce fn (avl-set-tree st) state)
  )

(defun set-filter (fn st)
  "Filter \"st\" to produce the subset of all elements for which \"fn\" is true"
  (set-reduce (lambda (s x)
                (if (funcall fn x)
                    (set-insert s x)
                    s)
                )
              st 
              (make-avl-set :tree nil :comp (avl-set-comp st))
              )
  )

(defmacro carcomp (fn)
  `(lambda (x y) (funcall ,fn (car x) (car y)))
  )

(defun default-comp (x y)
  (funcall (carcomp #'<) x y)
  )

(defmacro map-empty (&key comp)
  (if comp
      `(make-avl-set :tree nil :comp (carcomp ,comp))
      `(make-avl-set :tree nil :comp #'default-comp)
      )
  )

(defun map-keys (mp)
  (declare (type avl-set mp))
  (map-reduce (lambda (state key val)
                (declare (ignore val))
                ;; (set-insert state key)
                (cons key state))
              mp
              nil ;;(empty-set :comp cmp)
              ))

(defun map-vals (mp &key (cmp #'<))
  (declare (type avl-set mp) (type function cmp))
  (map-reduce (lambda (state key val)
                (declare (ignore key))
                (set-insert state val))
              mp
              (empty-set :comp cmp)))

(defun map-insert (x y mp)
  "Insert \"x -> y\" into the map \"mp\", returning the new map containing x->y"
  (declare (type avl-set mp))
  (let ((comp (avl-set-comp mp))
        )
    (make-avl-set :tree
                  (avl-tree-insert-unique (cons x y) (avl-set-tree mp) :comp comp)
                  :comp comp)
    )
  )

(defun map-upsert (x y mp)
  (map-insert x y mp)
  )

(defun map-remove (x mp)
  "Remove the element with key \"x\" from the map \"mp\""
  (declare (optimize (debug 3)(speed 0)))
  (let ((comp (avl-set-comp mp))
        )
    (make-avl-set :tree
                  (avl-tree-remove (cons x nil) (avl-set-tree mp) :comp comp)
                  :comp comp)
    )
  )

(defun map-find (x mp &optional (allow-no-result nil))
  "Search the map \"mp\" for the key \"x\".  If found, return the value; else return nil"
  (declare (optimize (debug 3)(speed 0)))
  (let ((comp (avl-set-comp mp))
        )
    (multiple-value-bind (found value) (avl-tree-search (cons x nil)
                                                        (avl-set-tree mp) 
                                                        :comp comp)
      ;(declare (ignore found))
      (if (and (not allow-no-result) (not found))
          (error "Object not found in map")
          )
      value
      )
    )
  )

(defun map-val (x mp &optional (allow-no-result nil))
  (let ((val (map-find x mp allow-no-result)))
    (if (null val)
        nil
        (cdr val))))

(defmacro map-singleton (x y &key (comp nil))
  (if (null comp)
      `(map-insert ,x ,y (map-empty))
      `(map-insert ,x ,y (map-empty :comp comp))))

(defun map-map (fn mp)
  "Apply \"fn\" to each element of the map \"mp\" to create a new map.

\"fn\" should have the form (lambda (key value) ...) and return the new value"
  (declare (type (function (t t) t) fn)
           (type avl-set mp))
  (make-avl-set :tree
                (avl-tree-map (lambda (x)
                                (cons (car x)
                                      (funcall fn (car x) (cdr x))))
                              (avl-set-tree mp) :comp (avl-set-comp mp))
                :comp (avl-set-comp mp))
  )

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
           (type avl-set mp))
  (avl-tree-reduce (lambda (st x)
                     (funcall fn st (car x) (cdr x)))
                   (avl-set-tree mp)
                   st)
  )

(defun map-filter (fn mp)
  "Filter \"mp\" to produce the submap of all elements for which \"fn\" is true"
  (map-reduce (lambda (m x y)
                (if (funcall fn x y) ;; functions should be of form (key val)
                    (map-insert x y m)
                    m))
              mp 
              (map-empty :comp (avl-set-comp mp))))

(defun alist->map (alist &key (comp (carcomp #'eql)))
  "Convert an associative list to a map.  Note that the default
comparison operation is eql, which is the default for associative
lists."
  (labels ((make-map (lst &optional (ret (make-avl-set :tree nil :comp (carcomp comp))))
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
