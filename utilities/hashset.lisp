;;; A minimal set implementation based on hashmaps for speed performance on is-included, remove and merge operations

(defpackage :hashset
  (:use :common-lisp )
  (:export make-hash-set
           hash-set
           hset-insert
           hset-remove
           hmap-insert
           hmap-remove
           hmap-singleton
           get-hashset-table
           )
  )

(in-package :hashset)

(defstruct (hashset
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "{")
                (maphash #'(lambda (k v) (format stream "~a => ~a " k v)) (get-hashset-table struct))
                (format stream "}")
                )
              )
             )
  (set)
  (comp)
  (:documentation "An easier interface for hash sets.")
  )


(defun get-hashset-table (hashset)
  (hashset-set hashset))

(defun make-hash-set (&key (size 50) (comp #'equal))
  (make-hashset
   :set (make-hash-table :test comp :size size)
   :comp comp)
)

(defun hmap-empty ()
  (make-hash-set))

(defun hset-insert (key set)
  (progn
    (setf (gethash key (get-hashset-table set)) t)
    set))  

(defun hset-remove (key set)
  (progn
    (remhash key (get-hashset-table set))
    set))

(defun hmap-insert (key val map)
  (progn
    (setf (gethash key (get-hashset-table map)) val)
    map))

(defun hmap-remove (key map)
  (hset-remove key map))

(defun hmap-val (key map &optional (allow-no-result nil))
  (multiple-value-bind (value present) (gethash key (get-hashset-table map))
    (if (and (not allow-no-result)
             (not present))
        (error "Key not found in map.")
        value))) 

(defun hmap-find (key map &optional (allow-no-result nil))
  (multiple-value-bind (value present) (gethash key (get-hashset-table map))
    (if (and (not allow-no-result)
             (not present))
        (error "Key not found in map.")
        (cons key value))))

(defmacro hset-singleton (key)
  (let ((a (make-hashset)))
    (progn
      (setf (gethash key (get-hashset-table a)) t)
      a)))

(defmacro hmap-singleton (key val)
  `(let ((a (make-hash-set)))
     (progn
       (setf (gethash key (get-hashset-table a)) val)
       a))
  )

(defun hmap-keys (hmap)
  (loop for k being the hash-keys in (get-hashset-table hmap) collect k)
  )

(defun hmap-vals (hmap)
  (loop for k being the hash-values in (get-hashset-table hmap) collect k)
  )

(defun hset-reduce (fn hset init)
  ;; fn should be of the form (lambda (state key) ...)
  (reduce (lambda (st x)
            (funcall fn st x))
          (loop for i being the hash-keys in (get-hashset-table hset))
          :initial-value init))

(defun hmap-reduce (fn hmap init)
  ;; fn should be of the form (lambda (state key val) ...)
  (reduce (lambda (st x)
            (funcall fn st x (gethash x (get-hashset-table st))))
          (loop for i being the hash-keys in (get-hashset-table hmap))
          :initial-value init))

