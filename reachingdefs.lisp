(defpackage :reaching-defs
  (:use :dataflow
        :pcf2-bc
        :setmap
        :cl
        :utils)
  (:export find-all-defs)
  )
(in-package :reaching-defs)

(defun find-all-defs (ops)
  "Create a map of definition IDs to opcodes.  The definition IDs are lists of the form (function-name destination idx) where \"idx\" is incremented for each assignment to \"destination\"."
  (declare (optimize (debug 3) (speed 0)))
  (labels ((id-cmp (id1 id2)
             (cond
               ((string< (first id1) (first id2))
                t)
               ((< (second id1) (second id2))
                t)
               ((< (third id1) (third id2))
                t)
               (t nil)
               )
             )
           (asgn-def-ids (lst curfun id-map asgnids)
             (if (null lst)
                 id-map
                 (typecase (first lst)
                   (label (asgn-def-ids (rest lst)
                                        (if (typep (second lst) 'clear)
                                            (slot-value (first lst) 'str)
                                            curfun)
                                        id-map
                                        asgnids
                                        )
                          )
                   (bits (asgn-def-ids (rest lst) curfun id-map asgnids))
                   ((or one-op two-op)
                    (with-slots (dest) (first lst)
                      (let ((ccnt (aif (map-find dest asgnids)
                                       it
                                       0)
                              )
                            )
                        (asgn-def-ids
                         (rest lst)
                         curfun
                         (map-insert (list curfun dest ccnt) (first lst) id-map)
                         (map-insert dest (1+ ccnt) asgnids)
                         )
                        )
                      )
                    )
                   (otherwise (asgn-def-ids (rest lst) curfun id-map asgnids))
                   )
                 )
               )
             )
    (let ((ret (map-empty :comp id-cmp))
          (asgnids (map-empty))
          )
      (asgn-def-ids ops "" ret asgnids)
      )
    )
  )