;; Some utility functions

(defpackage :utils
  (:use :cl)
  (:export aif it curry papply papply* compose)
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

(defun curry (fn)
  "Curry \"fn\" to one level i.e. create a function of one argument
  that returns a function taking the rest of fn's arguments"
  (declare (type function fn))
  (lambda (x)
    (lambda (&rest more-args)
      (apply fn (cons x more-args))
      )
    )
  )

(defun papply (fn &rest args)
  "Partially apply fn to \"args\" from the leftmost argument"
  (declare (type function fn))
  (lambda (&rest more-args)
    (apply fn (append args more-args))
    )
  )

(defun papply* (fn &rest args)
  "Partially apply fn to \"args\", from the rightmost argument"
  (declare (type function fn))
  (lambda (&rest more-args)
    (apply fn (append more-args args))
    )
  )

(defun compose (f g)
  (declare (type (function (t) t) f g))
  (the (function (t) t)
    (lambda (x)
      (funcall f (funcall g x))
      )
    )
  )