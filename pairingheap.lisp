;; A simple implementation of a pairing heap

(defpackage :pairing-heap (:use :common-lisp)
            (:export heap-insert
                     heap-delmin
                     heap-getmin
                     make-queue
                     enqueue
                     dequeue)
            (:nicknames :priority-queue))
(in-package :pairing-heap)

(defmacro heap-min (heap)
  `(car ,heap)
  )

(defmacro heap-children (heap)
  `(cdr ,heap)
  )

(defmacro singleton-heap (x)
  `(cons ,x nil)
  )

(defmacro new-min (x heap)
  `(cons ,x ,heap)
  )

(defmacro add-child (heap1 heap2)
  `(cons (heap-min ,heap1)
         (cons ,heap2 (heap-children ,heap1))
         )
  )

(defun heap-insert (x heap &key (comp #'<))
  "Insert \"x\" into thea heap \"heap\""
  (cond
    ((null heap) (singleton-heap x))
    ((funcall comp x (heap-min heap))
     (new-min x heap)
     )
    (t (add-child heap (singleton-heap x))
       )
    )
  )

(defun heap-getmin (heap)
  "Retrieve the minimum value from the heap"
  (heap-min heap)
  )

(defun heap-merge (heap1 heap2 &key (comp #'<))
  (cond
    ((null heap1) heap2)
    ((null heap2) heap1)
    ((funcall comp (heap-min heap1) (heap-min heap2))
     (add-child heap1 heap2)
     )
    (t (add-child heap2 heap1))
    )
  )

(defun merge-pairs (heaps &key (comp #'<))
  (cond
    ((null heaps)
     nil)
    ((null (rest heaps))
     (first heaps))
    (t (heap-merge (heap-merge (first heaps) (second heaps) :comp comp) (merge-pairs (cddr heaps) :comp comp) :comp comp))
    )
  )

(defun heap-delmin (heap &key (comp #'<))
  "Remove the minimum element from the heap \"heap\""
  (if (null heap)
      nil
      (merge-pairs (heap-children heap) :comp comp)
      )
  )

(defstruct priority-queue
  (heap)
  (comp)
  )

(defun make-queue (&key (comp #'<))
  (make-priority-queue :heap nil :comp (lambda (x y) (funcall comp (car x) (car y))))
  )

(defun enqueue (x queue)
  "Insert \"x\" intp \"queue\".  \"x\" should be of the form (key . value)"
  (declare (type priority-queue queue))
  (make-priority-queue :heap 
                       (heap-insert x (priority-queue-heap queue) :comp (priority-queue-comp queue))
                       :comp (priority-queue-comp queue))
  )

(defun dequeue (queue)
  "Remove the minimum element from \"queue\""
  (declare (type priority-queue queue))
  (let ((ret (heap-min (priority-queue-heap queue)))
        )
    (values (make-priority-queue :heap 
                                 (heap-delmin (priority-queue-heap queue) 
                                              :comp (priority-queue-comp queue))
                                 :comp (priority-queue-comp queue))
            ret)
    )
  )