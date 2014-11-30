;;; some utilities for flow functions in our data flow analysis, which is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-flow-utils
  (:use :common-lisp :pcf2-bc :setmap :setmap-rle :utils :hashset :pcf2-block-graph)
  (:export map-extract-val
           hmap-extract-val
           rle-map-extract-val
           ;;with-true-address
           ;;with-true-addresses
           ;;with-true-address-list
           hmap-eliminate-extra-consts
           eliminate-extra-consts
           eliminate-extra-faints
           hmap-eliminate-extra-faints
           map-union-without-conflicts
           hmap-union-without-conflicts
           rle-map-union-without-conflicts
           hmap-weaker-fn
           rle-map-weaker-fn
           rle-map-weaker-fn-brk
           map-weaker-fn)
  )

(in-package :pcf2-flow-utils)


(defmacro map-extract-val (var data)
  `(aif (map-val ,var ,data t)
        (if (equalp 'pcf2-block-graph:pcf-not-const it) nil it)
        0)
  )

(defmacro rle-map-extract-val (var data)
  `(aif (rle-map-val ,var ,data t)
        (if (equalp 'pcf2-block-graph:pcf-not-const it) nil it)
        0)
  )

(defmacro hmap-extract-val (var data)
  `(aif (hmap-val ,var ,data t)
        (if (equalp 'pcf2-block-graph:pcf-not-const it) nil it)
        0)
  )
#|
(defmacro with-true-addresses ((&rest syms) &body body)
  `(let ,(loop for sym in syms
            collect `(,sym (+ ,sym (aif base it 0))))
     ,@body))

(defmacro with-true-address (sym &body body)
  `(let ((,sym (+ ,sym base)))
     ,@body))

(defmacro with-true-address-list (lst &body body)
  `(let ((,lst (mapcar (lambda(x) (+ x base)) ,lst)))
     ,@body))
|#

(defun hmap-eliminate-extra-consts (flow blck use-map)
  ;;(declare (optimize (debug 3)(speed 0)))
  (let ((blckid (get-block-id blck))
        (lives (get-block-lives blck)))
    ;;(break)
    (hmap-reduce (lambda (map key val)
                      (declare (ignore val))
                      (let ((key-use (map-val key use-map t)))
                        (if key-use
                            (if (and (not (set-member key lives))
                                     (> blckid (cdr key-use))
                                     (equal key (get-block-base blck)))
                                (hmap-remove key map)
                                map)
                            map)))
                    flow
                    flow)))

(defun eliminate-extra-consts (flow blck use-map)
  ;;(declare (optimize (debug 3)(speed 0)))
  (let ((blckid (get-block-id blck))
        (lives (get-block-lives blck)))
    ;;(break)
    (rle-map-reduce (lambda (map key val)
                      (declare (ignore val))
                      (let ((key-use (map-val key use-map t)))
                        (if key-use
                            (if (and (not (rle-set-member key lives))
                                     (> blckid (cdr key-use))
                                     (equal key (get-block-base blck)))
                                (rle-map-remove key map)
                                map)
                            map)))
                    flow
                    flow)))
    #|(map-reduce (lambda (map key val)
                  (let ((key-use (map-val key use-map t)))
                    (if key-use
                        (if (and (not (set-member key lives))
                                 (> blckid (cdr key-use))) ;; use-map is (first . last )
                            map ;; eliminate
                            (map-insert key val map)) ;; not done with it yet
                        (map-insert key val map)))) ;; don't know when it's used, must retain
                flow
                (map-empty))))|#

(defun eliminate-extra-faints (flow blck use-map)
  (let ((blckid (get-block-id blck))
        (lives (get-block-lives blck)))
    (rle-set-reduce (lambda (set key)
                  (let ((key-use (map-val key use-map t)))
                    (if key-use
                        (if (and (not (rle-set-member key lives))
                                 (< blckid (car key-use))) ;; use-map is (first . last )
                            set ;; eliminate
                            (rle-set-insert set key)) ;; not done with it yet
                        (rle-set-insert set key)))) ;; don't know when it's used, must retain
                flow
                (rle-empty-set))))

(defun hmap-eliminate-extra-faints (flow blck use-map)
  (let ((blckid (get-block-id blck))
        (lives (get-block-lives blck)))
    (set-reduce (lambda (set key)
                  (let ((key-use (map-val key use-map t)))
                    (if key-use
                        (if (and (not (set-member key lives))
                                 (< blckid (car key-use))) ;; use-map is (first . last )
                            set ;; eliminate
                            (set-insert set key)) ;; not done with it yet
                        (set-insert set key)))) ;; don't know when it's used, must retain
                flow
                (empty-set))))

(defun map-union-without-conflicts (map1 map2)
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (let ((newmap (map-reduce (lambda (map-accum key val)
                               (declare (optimize (debug 3)(speed 0)))
                               (aif (map-val key map2 t)
                                    (if (equal it val)
                                        map-accum ;; already have the element
                                        (map-insert key 'pcf-not-const map-accum)) ;; element duplicates not equivalent
                                    (map-insert key val map-accum))) ;; if it's not found, it's new and needs to be added
                            map1
                            map2)));;(copy-hash-set map2))))
    newmap))

;; hmap is "hash map"
(defun hmap-union-without-conflicts (map1 map2)
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (let ((newmap (hmap-reduce (lambda (map-accum key val)
                               (declare (optimize (debug 3)(speed 0)))
                               (aif (hmap-val key map2 t)
                                    (if (equal it val)
                                        map-accum ;; already have the element
                                        (hmap-insert key 'pcf-not-const map-accum)) ;; element duplicates not equivalent
                                    (hmap-insert key val map-accum))) ;; if it's not found, it's new and needs to be added
                             map1
                             (copy-hash-set map2))))
    newmap))

;; rle-map is our compressed map
(defun rle-map-union-without-conflicts (map2 map1)
  ;;(declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (let ((newmap (rle-map-reduce (lambda (map-accum key val)
                                  ;;(declare (optimize (debug 3)(speed 0)))
                                  (aif (rle-map-val key map2 t)
                                       (if (equal it val)
                                           map-accum ;; already have the element
                                           (rle-map-insert key 'pcf-not-const map-accum)) ;; element duplicates not equivalent
                                       (rle-map-insert key val map-accum))) ;; if it's not found, it's new and needs to be added
                                map1
                                map2)))
    newmap))


(defun hmap-weaker-fn (map1 map2)
  ;; map 1 is weaker than (safely estimates) map 2 if map 1 is a subset of map2
  ;; and every entry in map 1 is either the same as in map 2 or not-const
  ;;(set-subset set1 set2)
;;  (declare (optimize (debug 3)(speed 0)))
  (labels ((weaker-map-vals (m1 m2)
             (hmap-reduce (lambda (state key m1-val)
                            ;;(declare (optimize (debug 3)(speed 0)))
                            (let ((m2-val (hmap-val key m2 t)))
                              (and state
                                   (or (equal m1-val m2-val)
                                       (equalp m1-val 'pcf2-block-graph:pcf-not-const)
                                       (null m2-val)))))
                          m1
                          t)))
    (and
     (weaker-map-vals map1 map2)
     (not (hset-subset map2 map1)))
    ))


(defun rle-map-weaker-fn (map1 map2)
  ;; map 1 is weaker than (safely estimates) map 2 if map 1 is a subset of map2
  ;; and every entry in map 1 is either the same as in map 2 or not-const
  ;;(set-subset set1 set2)
;;  (declare (optimize (debug 3)(speed 0)))
  (labels ((weaker-map-vals (m1 m2)
             (rle-map-reduce (lambda (state key m-val1)
                               ;;(declare (optimize (debug 3)(speed 0)))
                               (let ((m-val2 (rle-map-val key m2 t)))
                                 (and state
                                      (or (equal m-val1 m-val2)
                                          (equalp m-val1 'pcf2-block-graph:pcf-not-const)
                                          (null m-val2)
                                          ))))
                             m1
                             t)))
    (and
     ;;(weaker-map-vals map1 map2)
     (rle-map-weaker-efficient map1 map2 'pcf2-block-graph:pcf-not-const) 
     (not (rle-map-submap-efficient map2 map1)))
    ))

(defun rle-map-weaker-fn-brk (map1 map2)
  ;; map 1 is weaker than (safely estimates) map 2 if map 1 is a subset of map2
  ;; and every entry in map 1 is either the same as in map 2 or not-const
  ;;(set-subset set1 set2)
  (declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (labels ((weaker-map-vals (m1 m2)
             (rle-map-reduce (lambda (state key m-val1)
                               ;;(declare (optimize (debug 3)(speed 0)))
                               (let ((m-val2 (rle-map-val key m2 t)))
                                 (and state
                                      (or (equal m-val1 m-val2)
                                          (equalp m-val1 'pcf2-block-graph:pcf-not-const)
                                          (null m-val2)
                                          ))))
                             m1
                             t)))
    (let ((cond1 (rle-map-weaker-efficient-brk map1 map2 'pcf2-block-graph:pcf-not-const))
          (cond2 (not (rle-map-submap-efficient map2 map1))))
      ;;(break)
      (and cond1 cond2))))

(defun map-weaker-fn (map1 map2)
  ;; map 1 is weaker than (safely estimates) map 2 if map 1 is a subset of map2
  ;; and every entry in map 1 is either the same as in map 2 or not-const
  ;;(set-subset set1 set2)
;;  (declare (optimize (debug 3)(speed 0)))
  (labels ((weaker-map-vals (m1 m2)
             (map-reduce (lambda (state key m-val1)
                           ;;(declare (optimize (debug 3)(speed 0)))
                            (let ((m-val2 (map-val key m2 t)))
                              (and state
                                   (or (equal m-val1 m-val2)
                                       (equalp m-val1 'pcf2-block-graph:pcf-not-const)
                                       (null m-val2)))))
                          m1
                          t)))
    (and
     (weaker-map-vals map1 map2)
     (not (set-subset map2 map1)))
    ))

#|
(defun map-intersect (map1 map2)
  (map-reduce (lambda (map-accum key val)
                (aif (map-val key map2 t)
                     (if (eq it val)
                         (map-insert key val map-accum) ;; values correspond
                         map-accum) ;; values do not correspond
                     map-accum ;; value not in both maps
                ))
              map1
              (map-empty)))
|#
