;; Basic pointer analysis framework for LCC.  This will make other
;; dataflow more precise and allow us to handle conditional function
;; calls correctly, among other things.  Basically, we need to be able
;; to determine that we have a local pointer, at the very least, and
;; if that local pointer points to a constant (which is definitely
;; something we want to know).

(defpackage :lcc-ptranl
  (:use :cl
        :utils
        :lcc-dataflow
        :lcc-bc
        :setmap)
  (:export ptranl-dataflow-funs)
  (:shadow export import)
  )
(in-package :lcc-ptranl)


(defun const-gen (constL depL constR depR)
  (declare (ignore depL depR)
           (type avl-set constL constR))
  (the avl-set
    (set-reduce (lambda (mp x)
                  (map-insert x constR mp)
                  )
                (map-empty)
                constL)
    )
  )

(defun dep-gen (constL depL constR depR)
  (declare (type avl-set constL depL constR depR))
  (labels ((add-ptrs (st mp x)
             (let ((curval (aif (map-find x mp)
                                (cdr it)
                                (empty-set)
                                )
                     )
                   )
               (map-insert x (set-union curval st) mp)
               )
             )
           )
    (let* ((map1 (set-reduce (papply #'add-ptrs depR)
                             (map-empty)
                             constL)
             )
           (map2 (set-reduce (papply #'add-ptrs constR)
                             map1
                             depL))
           )
      (the avl-set (set-reduce (papply #'add-ptrs depR)
                               map2
                               depL)
           )
      )
    )
  )

(defun const-kill (constL depL constR depR)
  (declare (ignore depL constR depR)
           (type avl-set constL)
           (optimize (debug 3) (speed 0)))
  (the avl-set
    (set-reduce (lambda (mp x)
                  (map-insert x (empty-set) mp)
                  )
                constL
                (map-empty)
                )
    )
  )

(defun dep-kill (constL depL constR depR)
  (declare (ignore constL constR depR)
           (type avl-set depL))
  (the avl-set 
    (set-reduce (lambda (mp x)
                  (map-insert x (empty-set) mp)
                  )
                depL
                (map-empty)
                )
    )
  )

(defgeneric constleftl (op)
  )

(defgeneric depleftl (op ptrs)
  )

(defgeneric constrightl (op)
  )

(defgeneric deprightl (op ptrs)
  )

(defmacro def-left-right (type &key constleftl depleftl constrightl deprightl)
  `(progn
     (defmethod constleftl ((op ,type))
       (the list ,constleftl)
       )
     (defmethod depleftl ((op ,type) ptrs)
       (the list ,depleftl)
       )
     (defmethod constrightl ((op ,type))
       (the list ,constrightl)
       )
     (defmethod deprightl ((op ,type) ptrs)
       (the list ,deprightl)
       )
     )
  )

(def-left-right lcc-instruction)
