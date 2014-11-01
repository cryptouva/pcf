;; Authors: Benjamin Kreuter and Benjamin Terner
;;
;; This implements sets and maps more efficiently than the standard CL
;; way (which uses linked lists).

(defpackage :setmap-rle (:use :rle-tree :common-lisp)
            (:export rle-set-member
                     rle-set-member-group
                     rle-empty-set
                     rle-singleton
                     rle-set-insert
                     rle-set-remove
                     rle-set-diff
                     rle-set-diff-efficient
                     rle-set-union
                     rle-set-inter
                     rle-set-map           
                     rle-set-from-list
                     rle-list-from-set
                     rle-set-equalp
                     rle-set-reduce
		     rle-set-subset
                     rle-set-subset-efficient
                     rle-set-filter
                     rle-map-equalp
                     rle-map-member-group
                     rle-map-submap
                     rle-map-submap-efficient
                     rle-map-weaker-efficient
                     rle-map-weaker-efficient-brk
                     rle-map-empty
                     rle-map-singleton
                     rle-map-insert
                     rle-map-insert-altern
                     rle-map-keys
                     rle-map-vals
                     rle-map-upsert
                     rle-map-remove
                     rle-map-find
                     rle-map-val
                     rle-map-map
                     rle-map-reduce
                     rle-map-filter
                     rle-map-fold-forward
                     rle-map-fold-backward
                     rle-avl-set
                    #|
                     alist->map
                     alist->map*
                     |#)
            )
(in-package :setmap-rle)
 
(defstruct (rle-avl-set
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                ;;(format stream "{")
                ;; (format stream "Tree: ")
                (format stream "~A" (rle-avl-set-tree struct))
                #|(rle-avl-map (lambda (x y)
                               (format stream "(~A.~A) " x y))
                             (rle-avl-set-tree struct)
                             :key-val t) |#
                ;;(format stream "~%")
                ;;(format stream "Comp: ~A" (rle-avl-set-comp struct))
                ;;(format stream "}")
                )
              )
             )
  (tree)
  (comp)
  (:documentation "A key/value map based on an AVL tree.")
  )

(defparameter *default-comp* #'<)

(defmacro rle-empty-set (&key comp)
  (if comp
      ;;`(make-rle-avl-set :tree (empty-avl-tree :comp ,comp) :comp ,comp)
      ;;`(make-rle-avl-set :tree (empty-avl-tree :comp #'<) :comp #'<)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp ,comp)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp *default-comp*)
      )
  )


(defun rle-set-member (x st)
  "Check if \"x\" is contained in \"set\""
  (declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (multiple-value-bind (y v) (rle-avl-search x (rle-avl-set-tree st) :comp (rle-avl-set-comp st))
    (and y v)))

(defun rle-set-member-group (x tr &key (length 1))
  "Check if \"x\" ... \"x + length \" is contained in \"tr\""
  (declare (optimize (debug 3)(speed 0)))
  (multiple-value-bind (found node) (rle-avl-node-search 
                                     x 
                                     (rle-avl-set-tree tr) 
                                     :comp (rle-avl-set-comp tr))
    ;;(break)
    (if (not found)
        nil
        (if (< (+ x length)
               (+ (avl-idx node) (avl-length node) 1)) ;; x might start somewhere in the middle, and it's OK if they're equal
            t
            (rle-set-member-group (+ x (min length (avl-length node)))
                                  tr
                                  :length (- (+ x length)
                                             (+ (avl-idx node)
                                                (avl-length node))))))))

(defun rle-map-member-group (x val tr &key (length 1))
  "Check if \"x\" ... \"x + length \" with value \"val\" is contained in \"tr\""
  (declare (optimize (debug 3)(speed 0)))
  (multiple-value-bind (found node) (rle-avl-node-search 
                                     x 
                                     (rle-avl-set-tree tr) 
                                     :comp (rle-avl-set-comp tr))
    (if (not found)
        nil
        (if (not (equal val (avl-data node)))
            nil
            (if (< (+ x length)
                   (+ (avl-idx node) (avl-length node) 1)) ;; x might start somewhere in the middle, and it's OK if they're equal
                t
                 (let ((elapse-length (min length (- (+ (avl-idx node) (avl-length node)) x ))))
                  (rle-map-group-weaker (+ x elapse-length)
                                        val
                                        tr
                                        :length (- (+ x length)
                                                   (+ (avl-idx node)
                                                      (avl-length node)))
                                        )))))))
#|
 (rle-map-member-group (+ x (min length (avl-length node)))
                                      val
                                      tr
                                      :length (- (+ x length)
                                                 (+ (avl-idx node)
                                                    (avl-length node)))))))))
|#

(defun rle-singleton (x &key (comp #'<))
  "Create a new singleton set"
  (make-rle-avl-set :tree
                ;;(avl-tree-insert x (empty-avl-tree :comp comp) :comp comp)
                    (rle-avl-insert x (empty-rle-avl) :comp comp)
                    :comp comp)
  )

(defun rle-set-diff (set1 set2)
  "Compute the set containing only elements contained in \"set1\" but not \"set2\""
  (declare (type rle-avl-set set1 set2))
  (assert (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2)))
  (let ((comp (rle-avl-set-comp set1))
        )
    (make-rle-avl-set :tree 
                  (rle-avl-reduce (lambda (st k v)
                                     (if (rle-set-member k set2)
                                         st
                                         (rle-avl-insert-unique k st :comp comp :data v)
                                         )
                                     )
                                   (rle-avl-set-tree set1)
                                   nil)
                  :comp comp)
    )
  )


(defun rle-set-diff-efficient (set1 set2)
  "Quickly remove all elements in set2 from set1, where set1 is much larger than set2"
  (declare (type rle-avl-set set1 set2))
  (assert (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2)))
  (let ((comp (rle-avl-set-comp set1)))
    (make-rle-avl-set :tree
                  (rle-avl-reduce (lambda (st k v)
                                    (declare (ignore v)
                                             (optimize (debug 3)(speed 0)))
                                    ;;(break)
                                    (if (rle-set-member k set1)
                                        (rle-avl-remove k st :comp comp)
                                        st)
                                    )
                                  (rle-avl-set-tree set2)
                                  (rle-avl-set-tree set1))
                  :comp comp)
    )
)

(defun rle-set-subset (set1 set2)
  (declare (type rle-avl-set set1 set2))
  (cond
    ((not (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2))) nil)
    (t (rle-avl-reduce (lambda (x k v)
                         (declare (ignore v))
                          (and x (rle-set-member k set2))
                          )
                        (rle-avl-set-tree set1)
                        t))))

(defun rle-set-subset-efficient (set1 set2)
  (declare 
   (optimize (debug 3)(speed 0))
   (type rle-avl-set set1 set2))
  ;;(break)
  (cond
    ((not (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2))) nil)
    (t (rle-avl-node-reduce (lambda (st node)
                              (and st (rle-set-member-group (avl-idx node)
                                                            set2
                                                            :length (avl-length node))))
                            (rle-avl-set-tree set1)
                            t))))

(defun rle-set-equalp (set1 set2)
  (declare (type rle-avl-set set1 set2))
  (and (rle-set-subset set1 set2)
       (rle-set-subset set2 set1)
       )
  )

(defun rle-set-union (set2 set1)
  "Compute the union of \"set1\" and \"set2\""
  ;; set1 should be the smaller one, since we reduce over it
  ;; (second argument)
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

(defun rle-set-insert (set x &key (length 1))
  (declare (optimize (debug 3)(speed 0)))
  (let ((comp (rle-avl-set-comp set)))
    (make-rle-avl-set :tree
                  (rle-avl-insert-unique x (rle-avl-set-tree set) :comp comp :length length)
                  :comp comp))
  )

(defun rle-set-remove (set x &optional (allow-no-result t))
  "Remove key \"x\" from the set"
  (declare (optimize (debug 3)(speed 0)))
   (let ((comp (rle-avl-set-comp set)))
     (make-rle-avl-set :tree
                   (rle-avl-remove x (rle-avl-set-tree set) :comp comp :allow-no-result allow-no-result)
                   :comp comp)))

(defun rle-set-from-list (lst &key (comp #'<))
  "Create a set that contains the elements of \"lst\""
  (declare (type list lst)
           (type (function (t t) boolean) comp))
  (reduce (lambda (st x)
            (rle-set-insert st x))
          lst
          :initial-value (rle-empty-set :comp comp)))

(defun rle-list-from-set (st)
  (rle-set-reduce (lambda (st x v)
                (declare (ignore v))
                (cons x st)
                )
              st
              nil)
  )

(defun rle-set-inter (set1 set2)
  "Compute the intersection of \"set1\" and \"set2\""
  (declare (type rle-avl-set set1 set2))
  (assert (equalp (rle-avl-set-comp set1) (rle-avl-set-comp set2)))
  (let ((comp (rle-avl-set-comp set1))
        )
    (make-rle-avl-set :tree 
                      (rle-avl-reduce (lambda (st k v)
                                        (if (rle-set-member k set2)
                                            (rle-avl-insert k st :comp comp :data v)
                                            st
                                            )
                                        )
                                      (rle-avl-set-tree set1)
                                      nil)
                      :comp comp)
    )
  )

(defun rle-set-map (fn st)
  "Compute the image of \"st\" under \"fn\""
  (make-rle-avl-set :tree (rle-avl-map fn (rle-avl-set-tree st)
                                       :comp (rle-avl-set-comp st))
                    :comp (rle-avl-set-comp st))
  )


(defun rle-set-reduce (fn st state)
  "Fold \"st\" over \"fn\""
  ;; fn should be of type (lambda (state key))
  (rle-avl-reduce (lambda (x y z) (declare (ignore z)) (funcall fn x y)) (rle-avl-set-tree st) state)
  ;;(rle-avl-reduce fn (rle-avl-set-tree st) state)
  )


(defun rle-set-filter (fn st)
  "Filter \"st\" to produce the subset of all elements for which \"fn\" is true"
  (rle-set-reduce (lambda (s k v)
                    (declare (ignore v))
                    (if (funcall fn k)
                        (rle-set-insert s k)
                        s)
                    )
                  st 
                  (make-rle-avl-set :tree nil :comp (rle-avl-set-comp st))
                  )
  )

;;;
;;; Maps
;;;

(defmacro rle-map-empty (&key (comp nil))
  (if (null comp)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp *default-comp*)
      `(make-rle-avl-set :tree (empty-rle-avl) :comp ,comp)
      )
  )

(defmacro rle-map-singleton (x y &key (comp nil))
  (if (null comp)
      `(rle-map-insert ,x ,y (rle-map-empty))
      `(rle-map-insert ,x ,y (rle-map-empty :comp comp) :comp comp)))

(defun rle-map-keys (mp)
  (declare (type rle-avl-set mp))
  (rle-map-reduce (lambda (state key val)
                    (declare (ignore val))
                    ;;(set-insert state key))
                    (cons key state))
                  mp
                  nil ;;(empty-set :comp cmp)
                  ))


(defun rle-map-vals (mp &key (cmp #'<))
  (declare (type rle-avl-set mp) (type function cmp))
  (rle-map-reduce (lambda (state key val)
                    (declare (ignore key))
                    (rle-set-insert state val))
                  mp
                  (rle-empty-set :comp cmp)))

(defun rle-map-insert (x y mp &key (length 1))
  "Insert \"x -> y\" into the map \"mp\", returning the new map containing x->y"
  (declare (type rle-avl-set mp)
           (optimize (debug 3)(speed 0)))
  ;;(break)
  (let ((comp (rle-avl-set-comp mp))
        )
    (make-rle-avl-set :tree
                  (rle-avl-insert-unique  x
                                          (rle-avl-set-tree mp)
                                          :comp comp
                                          :data y
                                          :length length)
                  :comp comp)
    )
  )

(defun rle-map-insert-altern (x y length mp)
  "Insert \"x -> y\" into the map \"mp\", returning the new map containing x->y"
  (declare (type rle-avl-set mp)
           (optimize (debug 3)(speed 0)))
  ;;(break)
  (let ((comp (rle-avl-set-comp mp))
        )
    (make-rle-avl-set :tree
                  (rle-avl-insert-unique  x
                                          (rle-avl-set-tree mp)
                                          :comp comp
                                          :data y
                                          :length length)
                  :comp comp)
    )
  )

(defun rle-map-upsert (x y mp)
  (rle-map-insert x y mp)
  )

(defun rle-map-remove (x mp &key (length 1))
  "Remove the element with key \"x\" from the map \"mp\""
  (declare (optimize (debug 3)(speed 0)))
  (let ((comp (rle-avl-set-comp mp))
        )
    (make-rle-avl-set :tree
                  (rle-avl-remove x (rle-avl-set-tree mp) :comp comp :length length)
                  :comp comp)
    )
  )

(defun rle-map-find (x mp &optional (allow-no-result nil))
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

(defun rle-map-val (x mp &optional (allow-no-result nil))
 (let ((val (rle-map-find x mp allow-no-result)))
    (if (null val)
        nil
        (cdr val))))



(defun rle-map-submap (map1 map2)
  (declare (type rle-avl-set map1 map2))
  (cond
    ((not (equalp (rle-avl-set-comp map1) (rle-avl-set-comp map2))) nil)
    (t (rle-avl-reduce (lambda (x k v)
                         ;;(declare (ignore v))
                          (and x (equal v (rle-map-val k map2 t)))
                          )
                        (rle-avl-set-tree map1)
                        t))))

(defun rle-map-submap-efficient (map1 map2)
  (declare 
   (optimize (debug 3)(speed 0))
   (type rle-avl-set map1 map2))
   (cond
    ((not (equalp (rle-avl-set-comp map1) (rle-avl-set-comp map2))) nil)
    (t (rle-avl-node-reduce (lambda (st node)
                              (and st 
                                   (rle-map-member-group (avl-idx node)
                                                         (avl-data node)
                                                         map2
                                                         :length (avl-length node))))
                            (rle-avl-set-tree map1)
                            t))))

(defun rle-map-equalp (map1 map2)
  (and
   (rle-map-submap-efficient map1 map2)
   (rle-map-submap-efficient map2 map1)))

(defun rle-map-group-weaker (x val tr &key (length 1))
  "Check if \"x\" ... \"x + length \" with value \"val\" is contained in \"tr\""
  (declare (optimize (debug 3)(speed 0)))
  (multiple-value-bind (found node) (rle-avl-node-search 
                                     x 
                                     (rle-avl-set-tree tr) 
                                     :comp (rle-avl-set-comp tr))
    (if (not found) ;;null (rle-map-val x tr t))
        (if (< length 2)
            t
            (rle-map-group-weaker (+ x 1) ;; proceed to next index
                                  val
                                  tr
                                  :length (- length 1)))
        (if (not (equal val (avl-data node)))
            nil ;; (avl-data node) != bottom && (avl-idx node) found in map2
            (if (< (+ x length)
                   (+ (avl-idx node) (avl-length node) 1)) ;; x might start somewhere in the middle, and it's OK if they're equal
                t
               (let ((elapse-length (min length (- (+ (avl-idx node) (avl-length node)) x ))))
                  (rle-map-group-weaker (+ x elapse-length)
                                        val
                                        tr
                                        :length (- (+ x length)
                                                   (+ (avl-idx node)
                                                      (avl-length node)))
                                        )))))))

(defun rle-map-group-weaker-brk (x val tr &key (length 1))
  "Check if \"x\" ... \"x + length \" with value \"val\" is contained in \"tr\""
  (declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (multiple-value-bind (found node) (rle-avl-node-search 
                                     x 
                                     (rle-avl-set-tree tr) 
                                     :comp (rle-avl-set-comp tr))
    (if (not found) ;;null (rle-map-val x tr t))
        (if (< length 2)
            t
            (rle-map-group-weaker (+ x 1) ;; proceed to next index
                                  val
                                  tr
                                  :length (- length 1)))
        (if (not (equal val (avl-data node)))
            nil ;; (avl-data node) != bottom && (avl-idx node) found in map2
            (if (< (+ x length)
                   (+ (avl-idx node) (avl-length node) 1)) ;; x might start somewhere in the middle, and it's OK if they're equal
                t
                (let ((elapse-length (min length (- (+ (avl-idx node) (avl-length node)) x ))))
                  (rle-map-group-weaker (+ x elapse-length)
                                        val
                                        tr
                                        :length (- (+ x length)
                                                   (+ (avl-idx node)
                                                      (avl-length node)))
                                        )))))))

(defun rle-map-weaker-efficient (map1 map2 bottom)
  (declare 
   (optimize (debug 3)(speed 0))
   (type rle-avl-set map1 map2))
  (cond
    ((not (equalp (rle-avl-set-comp map1) (rle-avl-set-comp map2))) nil)
    (t (rle-avl-node-reduce (lambda (st node)
                              (and st 
                                   (or
                                    (equalp (avl-data node) bottom)
                                    (rle-map-group-weaker (avl-idx node)
                                                          (avl-data node)
                                                          map2
                                                          :length (avl-length node)
                                                          ))))
                            (rle-avl-set-tree map1)
                            t))))

(defun rle-map-weaker-efficient-brk (map1 map2 bottom)
  (declare 
   (optimize (debug 3)(speed 0))
   (type rle-avl-set map1 map2))
  (cond
    ((not (equalp (rle-avl-set-comp map1) (rle-avl-set-comp map2))) nil)
    (t (rle-avl-node-reduce (lambda (st node)
                              (let ((cond1
                                     (and st 
                                          (or
                                           (equalp (avl-data node) bottom)
                                           (rle-map-group-weaker (avl-idx node)
                                                                 (avl-data node)
                                                                 map2
                                                                 :length (avl-length node)
                                                                 )))))
                                (if (and
                                     (not (rle-map-equalp (rle-map-empty) map2))
                                     (not cond1))
                                    (let ((cond2 (rle-map-group-weaker-brk (avl-idx node)
                                                                           (avl-data node)
                                                                           map2
                                                                           :length (avl-length node))))
                                      (break)
                                      cond2)
                                    cond1)))
                            (rle-avl-set-tree map1)
                            t))))

(defun rle-map-map (fn mp)
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


(defun rle-map-fold (fn keys mp st)
  "fn should have the form (lambda (state key val)), keys should be the keys in order"
  (if (null keys)
      st
      (rle-map-fold fn
                    (cdr keys) 
                    mp 
                    (funcall fn st (car keys) (cdr (rle-map-find (car keys) mp))))))

(defun rle-map-fold-forward (fn mp st)
  (rle-map-fold fn (reverse (rle-map-keys mp)) mp st)
  )

(defun rle-map-fold-backward (fn mp st)
  (rle-map-fold fn (rle-map-keys mp) mp st)
  )


(defun rle-map-reduce (fn mp st)
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

(defun rle-map-filter (fn mp)
  "Filter \"mp\" to produce the submap of all elements for which \"fn\" is true"
  (rle-map-reduce (lambda (m x y)
                (if (funcall fn x y) ;; functions should be of form (key val)
                    (rle-map-insert x y m)
                    m))
              mp 
              (rle-map-empty :comp (rle-avl-set-comp mp))))
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
