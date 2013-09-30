;; Basic pointer analysis framework
;;
;; This is needed because we use indirection quite a lot, and it makes
;; other dataflow analysis far more difficult when a pointer might
;; point to anything.

(defpackage :ptranl
  (:use :cl
        :utils
        :dataflow
        :pcf2-bc
        :setmap)
  (:export make-ptr-flow-fn
           ptr-flow-fn
           ptr-join-fn)
  )
(in-package :ptranl)

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

(defun ptrmap-merge (map1 map2)
  "Compute the union of the points-to maps for each \"x\" in map1/map2"
  (declare (type avl-set map1 map2)
           (optimize (debug 3) (speed 0)))
  (the avl-set
    (map-map (lambda (x y)
                  (the avl-set
                    (let ((curval (aif (map-find x map2)
                                       (cdr it)
                                       (map-empty)
                                       )
                            )
                          )
                      (set-union y curval)
                      )
                    )
                  )
                map1)
    )
  )

(defun get-gen (bb ptrs constL constR)
  (declare (type basic-block bb)
           (type avl-set ptrs constL constR))
  (the avl-set
    (let ((depL (reduce #'set-union
                        (mapcar (compose
                                 #'set-from-list 
                                 (papply* #'depleftl ptrs))
                                (basic-block-ops bb))
                        :initial-value (empty-set)))
          (depR (reduce #'set-union
                        (mapcar (compose
                                 #'set-from-list 
                                 (papply* #'deprightl ptrs))
                                (basic-block-ops bb))
                        :initial-value (empty-set)))
          )
      (ptrmap-merge (const-gen constL depL constR depR)
                    (dep-gen constL depL constR depR))
      )
    )
  )

(defun get-kill (bb ptrs constL constR)
  (declare (optimize (debug 3) (speed 0)))
  (let ((depL (reduce #'set-union
                      (mapcar (compose #'set-from-list
                                       (papply* #'depleftl ptrs))
                              (basic-block-ops bb))
                      :initial-value (empty-set)))
        (depR (reduce #'set-union
                      (mapcar (compose #'set-from-list
                                       (papply* #'deprightl ptrs))
                              (basic-block-ops bb))
                      :initial-value (empty-set)))
        )
    (ptrmap-merge (const-kill constL depL constR depR)
                  (dep-kill constL depL constR depR))
    )
  )

(defun ptr-flow-fn* (bb map1 map2 constL constR)
  (declare (optimize (debug 3) (Speed 0)))
  (set-union 
   (set-diff map1 
             (get-kill bb 
                       map2 
                       constL 
                       constR)) 
   (get-gen bb 
            map1 
            constL 
            constR))
  )

(defun make-ptr-flow-fn ()
  (lambda (map1 map2 bb)
    (let ((constL (reduce #'set-union
                          (mapcar (compose #'set-from-list
                                           #'constleftl)
                                  (basic-block-ops bb))
                          :initial-value (empty-set))
            )
          (constR (reduce #'set-union
                          (mapcar (compose #'set-from-list
                                           #'constleftl)
                                  (basic-block-ops bb))
                          :initial-value (empty-set))
            )
          )
      (list (ptr-flow-fn* bb map1 map2 constL constR)
            (ptr-flow-fn* bb map2 map1 constL constR)
            )
      )
    )
  )

(defun ptr-join-fn (curmaps newmaps)
  (list
   (set-union (first curmaps) (first newmaps))
   (set-inter (second curmaps) (second newmaps)))
  )

;; Neat trick: we can ignore mkptr instructions until we do
;; interprocedural analysis, because these instructions only add the
;; base pointer to whatever their destination is, and we can just
;; assume the base pointer is 0 during intraprocedural analysis.  So
;; we only really care about other instructions that modify a
;; destination address.

;; We represent pointers as cons cells, so x -> y is (x . y)

(declaim (ftype (function (t) list) constleftl))
(defgeneric constleftl (op)
  )

(declaim (ftype (function (t avl-set) list) depleftl))
(defgeneric depleftl (op ptrs)
  )

(declaim (ftype (function (t) list) constrightl))
(defgeneric constrightl (op)
  )

(declaim (ftype (function (t avl-set) list) deprightl))
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

;; We do not need to check the deprightl condition for these
;; operations, because a single bit is not a pointer.
(def-left-right gate
    :constleftl (with-slots (dest) op
                  (list dest)
                  )
    )

(def-left-right bits
    :constleftl (with-slots (dest) op
                  dest
                  )
    )

(def-left-right join
    :constleftl (with-slots (dest) op
                  (list dest)
                  )
    )

(def-left-right const
    :constleftl (with-slots (dest) op
                    (list dest))
    :deprightl (with-slots (op1) op
                   (cdr (map-find op1 ptrs))
                 )
    )

(def-left-right mkptr
    :constleftl (with-slots (dest) op
                  (list dest))
    :deprightl (with-slots (dest) op
                 (cdr (map-find dest ptrs))
                 )
    )

(def-left-right copy
    :constleftl (with-slots (dest op2) op
                    (loop for i from 0 to (1- op2) collect
                         (+ dest i)
                         )
                  )
    :deprightl (with-slots (op1 op2) op
                 (loop for i from 0 to (1- op2) nconc
                      (cdr (map-find (+ op1 i) ptrs))
                      )
                 )
    )

(def-left-right copy-indir
    :constleftl (with-slots (dest op2) op
                  (loop for i from 0 to (1- op2) collect
                       (+ dest i)
                       )
                  )
    :deprightl (with-slots (op1 op2) op
                 (let ((ys (loop for i from 0 to (1- op2) nconc
                                (cdr (map-find (+ op1 i) ptrs))
                                )
                         )
                       )
                   (loop for i in ys nconc
                        (cdr (map-find i ptrs))
                        )
                   )
                 )
    )

(def-left-right indir-copy
    :depleftl (with-slots (dest op2) op
                (loop for i from 0 to (1- op2) nconc
                     (cdr (map-find (+ dest i) ptrs))
                     )
                )
    :deprightl (with-slots (op1 op2) op
                 (loop for i from 0 to (1- op2) nconc
                      (cdr (map-find (+ op1 i) ptrs))
                      )
                 )
    )

;; Other instructions do not matter.  If we do pointer arithmetic, we
;; will just assume that we never overflow array boundaries.
(def-left-right instruction)