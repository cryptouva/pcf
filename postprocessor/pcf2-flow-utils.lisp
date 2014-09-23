;;; some utilities for flow functions in our data flow analysis, which is adapted from Data Flow Analysis: Theory and Practice by Khedker, Sanyal, and Karkare
;;; author: bt3ze@virginia.edu
(defpackage :pcf2-flow-utils
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-block-graph)
  (:export map-extract-val
           ;;with-true-address
           ;;with-true-addresses
           ;;with-true-address-list
           eliminate-extra-consts
           eliminate-extra-faints)
  )

(in-package :pcf2-flow-utils)


(defmacro map-extract-val (var data)
  `(aif (map-val ,var ,data t)
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
(defun eliminate-extra-consts (flow blck use-map)
  ;; if blckid is past 
  (let ((blckid (get-block-id blck))
        (lives (get-block-lives blck)))
    (map-reduce (lambda (map key val)
                  (if (and (not (set-member key lives))
                           (> blckid (cdr (map-val key use-map)))) ;; use-map is (first . last )
                      map
                      (map-insert key val map)))
              flow
              (map-empty))))

(defun eliminate-extra-faints (flow blck use-map)
  (let ((blckid (get-block-id blck))
        (lives (get-block-lives blck)))
    (set-reduce (lambda (set key)
                  (if (and (not (set-member key lives))
                           (< blckid (car (map-val key use-map)))) ;; use-map is (first . last )
                      set
                      (set-insert set key)))
                flow
                (empty-set))))
