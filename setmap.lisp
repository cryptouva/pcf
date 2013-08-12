;; Author: Benjamin Kreuter
;;
;; This implements sets and maps more efficiently than the standard CL
;; way (which uses linked lists).

(defpackage :setmap (:use :tree :common-lisp)
            (:export set-member
                     singleton
                     set-diff
                     set-union
                     set-inter
                     set-map
                     set-from-list
                     map-insert
                     map-remove
                     map-find
                     map-map
                     map-reduce
                     map-empty
                     empty-set)
            )
(in-package :setmap)

(defstruct (avl-set
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "{")
                (avl-tree-map (lambda (x) (format stream "~A " x)) (avl-set-tree struct))
                (format stream "}")
                )
              )
             )
  (tree)
  (comp)
  (:documentation "A key/value map based on an AVL tree.")
  )

(defmacro empty-set (&key (comp #'<))
  `(make-avl-set :tree nil :comp ,comp)
  )

(defun set-member (x st)
  "Check if \"x\" is contained in \"set\""
  (avl-tree-search x (avl-set-tree st) :comp (avl-set-comp st))
  )

(defun singleton (x &key (comp #'<))
  "Create a new singleton set"
  (make-avl-set :tree
                (avl-tree-insert x nil :comp comp)
                :comp comp)
  )

(defun set-diff (set1 set2)
  "Compute the set containing only elements contained in \"set1\" but not \"set2\""
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

(defun set-union (set1 set2)
  "Compute the union of \"set1\" and \"set2\""
  (assert (equalp (avl-set-comp set1) (avl-set-comp set2)))
  (let ((comp (avl-set-comp set1))
        )
    (make-avl-set :tree
                  (avl-tree-reduce (lambda (st x)
                                     (avl-tree-insert-unique x st :comp comp))
                                   (avl-set-tree set1)
                                   (avl-set-tree set2))
                  :comp comp
                  )
    )
  )

(defun set-from-list (lst &key (comp #'<))
  "Create a set that contains the elements of \"lst\""
  (make-avl-set :tree
                (reduce (lambda (st x)
                          (avl-tree-insert-unique x st :comp comp))
                        lst :initial-value nil)
                :comp comp
                )
  )

(defun set-inter (set1 set2)
  "Compute the intersection of \"set1\" and \"set2\""
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
  (make-avl-set :tree (avl-tree-map fn (avl-set-tree st)) :comp (avl-set-comp st))
  )

(defun set-reduce (fn st state)
  "Fold \"st\" over \"fn\""
  (avl-tree-reduce fn (avl-set-tree st) state)
  )

(defmacro carcomp (fn)
  `(lambda (x y) (funcall ,fn (car x) (car y)))
  )

(defmacro map-empty (&key (comp #'<))
  `(make-avl-set :tree nil :comp (carcomp ,comp))
  )

(defun map-insert (x y mp)
  "Insert \"x -> y\" into the map \"mp\""
  (let ((comp (avl-set-comp mp))
        )
    (make-avl-set :tree
                  (avl-tree-insert-unique (cons x y) (avl-set-tree mp) :comp (carcomp comp))
                  :comp comp)
    )
  )

(defun map-remove (x mp)
  "Remove the element with key \"x\" from the map \"mp\""
  (let ((comp (avl-set-comp mp))
        )
    (make-avl-set :tree
                  (avl-tree-remove (cons x nil) (avl-set-tree mp) :comp comp)
                  :comp comp)
    )
  )

(defun map-find (x mp)
  "Search the map \"mp\" for the key \"x\".  If found, return the value; else return nil"
  (let ((comp (avl-set-comp mp))
        )
    (multiple-value-bind (found value) (avl-tree-search x 
                                                        (avl-set-tree mp) 
                                                        :comp comp)
      (declare (ignore found))
      value
      )
    )
  )

(defun map-map (fn mp)
  "Apply \"fn\" to each element of the map \"mp\" to create a new map.

\"fn\" should have the form (lambda (key value) ...)"
  (declare (type (function (t t) t) fn)
           (type avl-set mp))
  (make-avl-set :tree
                (avl-tree-map (lambda (x)
                                (funcall fn (car x) (cdr x)))
                              (avl-set-tree mp))
                :comp (avl-set-comp mp))
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