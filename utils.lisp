;; Some utility functions

(defpackage :utils
  (:use :cl)
  (:export aif it)
  )
(in-package :utils)

(defmacro aif (test-form if-form &optional else-form)
  "The \"anaphoric\" if macro from On Lisp"
  `(let ((it ,test-form)
         )
     (if it
         ,if-form
         ,else-form)
     )
  )