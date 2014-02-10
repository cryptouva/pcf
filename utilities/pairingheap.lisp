;; A simple implementation of a pairing heap

(defpackage :pairing-heap (:use :common-lisp)
            (:export heap-insert
                     heap-delmin
                     heap-getmin
                     make-queue
                     peek-queue
                     queue-emptyp
                     enqueue
                     dequeue
                     update-queue-min)
            (:nicknames :priority-queue))
(in-package :pairing-heap)

(defstruct heap
  (min)
  (children nil :type list)
  (empty nil :type boolean)
  )

(defmacro make-empty-heap ()
  `(make-heap :empty t)
  )

(defmacro singleton-heap (x)
  `(make-heap :min ,x :children nil)
  )

(defmacro new-min (x heap)
  `(make-heap :min ,x :children ,heap)
  )

(defmacro add-child (heap1 heap2)
  `(make-heap :min (heap-min ,heap1)
              :children (cons ,heap2 (heap-children ,heap1))
              )
  )

(defun heap-insert (x heap &key (comp #'<))
  "Insert \"x\" into thea heap \"heap\""
  (declare (type heap heap)
           (optimize (debug 3) (speed 0)))
  (cond
    ((heap-empty heap) (singleton-heap x))
    ((funcall comp x (heap-min heap))
     (new-min x (list heap))
     )
    (t (add-child heap (singleton-heap x))
       )
    )
  )

(defun heap-getmin (heap1)
  "Retrieve the minimum value from the heap"
  (heap-min heap1)
  )

(defun heap-merge (heap1 heap2 &key (comp #'<))
  (declare (type heap heap1 heap2))
  (the heap
    (cond
      ((heap-empty heap1) heap2)
      ((heap-empty heap2) heap1)
      ((funcall comp (heap-min heap1) (heap-min heap2))
       (add-child heap1 heap2)
       )
      (t (add-child heap2 heap1))
      )
    )
  )

(defun merge-pairs (heaps &key (comp #'<))
  (declare (type list heaps)
           (optimize (debug 3) (speed 0))
           )
  (the heap
    (cond
      ((null heaps)
       (make-empty-heap))
      ((null (rest heaps))
       (first heaps))
      (t (heap-merge (heap-merge (first heaps) (second heaps) :comp comp) 
                     (merge-pairs (cddr heaps) :comp comp) :comp comp))
      )
    )
  )

(defun heap-delmin (heap &key (comp #'<))
  "Remove the minimum element from the heap \"heap\""
  (if (null heap)
      nil
      (the heap (merge-pairs (heap-children heap) :comp comp))
      )
  )

(defstruct priority-queue
  (heap (make-heap :empty t) :type heap)
  (comp)
  )

(defstruct priority-queue-item 
  (priority)
  (value)
  )

(defun make-queue (&key (comp #'<))
  (make-priority-queue :heap (make-empty-heap) :comp (lambda (x y) (funcall comp 
                                                              (priority-queue-item-priority x) 
                                                              (priority-queue-item-priority y))))
  )

(defun update-queue-min (queue newval)
  (declare (type priority-queue queue))
  (make-priority-queue :heap 
                       (make-heap :min 
                                  (make-priority-queue-item :priority
                                                            (priority-queue-item-priority 
                                                             (heap-min
                                                              (priority-queue-heap queue)))
                                                            :value newval)
                                  :children
                                  (heap-children
                                   (priority-queue-heap queue))
                                  )
                       :comp
                       (priority-queue-comp queue)
                       )
  )

(defun queue-emptyp (queue)
  (heap-empty (priority-queue-heap queue))
  )

(defun peek-queue (queue)
  (declare (type priority-queue queue)
           (optimize (debug 3) (speed 0)))
  (let ((min (heap-min (priority-queue-heap queue)))
        )
    (declare (type (or null priority-queue-item) min))
    (if min
        (the (not null) (priority-queue-item-value min))
        )
    )
  )

(defun enqueue (x queue)
  "Insert \"x\" intp \"queue\".  \"x\" should be of the form (key . value)"
  (declare (type priority-queue queue)
           (type cons x)
           (optimize (debug 3) (speed 0)))
  (make-priority-queue :heap 
                       (heap-insert (make-priority-queue-item :priority (car x)
                                                              :value (cdr x))
                                    (priority-queue-heap queue) :comp (priority-queue-comp queue))
                       :comp (priority-queue-comp queue))
  )

(defun dequeue (queue)
  "Remove the minimum element from \"queue\""
  (declare (type priority-queue queue)
           (optimize (debug 3) (speed 0)))
  (make-priority-queue :heap 
                       (heap-delmin (priority-queue-heap queue) 
                                    :comp (priority-queue-comp queue))
                       :comp (priority-queue-comp queue))
  )