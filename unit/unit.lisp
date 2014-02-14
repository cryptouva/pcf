;; Unit testing framework

(defpackage :unit
  (:use :cl)
  (:export defun-ut 
           run-unit-tests
           defmethod-ut)
  )
(in-package :unit)

(defparameter unit-tests (make-hash-table))

(defun add-tests (name tests)
  (setf (gethash name unit-tests)
        (mapcar (lambda (x)
                  (cons x 
                        (intern (concatenate 'string
                                             (symbol-name name)
                                             "/"
                                             (symbol-name (car x)))))) tests))
  )

(defmacro defun-ut (name lambda-list body &key (tests nil) (documentation ""))
  (if (null tests)
      (warn (with-output-to-string (str)
              (format str "Function ~A has no unit tests~%" name)
              )
            )
      )
  (add-tests name tests)
  `(progn
     ,@(loop for test in tests collect
            `(defun ,(intern (concatenate 'string 
                                          (symbol-name name)
                                          "/"
                                          (symbol-name (car test))))
                 ()
               (funcall ,(cdr test))
               )
            )
     (defun ,name ,lambda-list 
       ,documentation
       ,body)
     )
  )

(defmacro defmethod-ut (name lambda-list body &key (tests nil))
  (if (null tests)
      (warn (with-output-to-string (str)
              (format str "Method ~A has no unit tests~%" name)
              )
            )
      )
  (add-tests (intern (apply #'concatenate 
                            (append
                             (list 'string 
                                   (symbol-name name))
                             (loop for arg in lambda-list collect
                                  (concatenate 'string "/" (symbol-name (cadr arg)))
                                  )
                             )
                            )
                     )
             tests)
  `(progn
     ,@(loop for test in tests collect
            `(defun ,(intern (concatenate 'string 
                                          (symbol-name name)
                                          (apply #'concatenate 'string
                                                 (loop for arg in lambda-list collect
                                                      (concatenate 'string "/" (symbol-name (cadr arg)))
                                                      )
                                                 )
                                          "/"
                                          (symbol-name (car test))))
                 ()
               (funcall ,(cdr test))
               )
            )
     (defmethod ,name ,lambda-list 
       ,body)
     )
  )

(defun run-unit-tests ()
  (declare (optimize (debug 3)))
  (let ((ignore-all-tests nil))
    (maphash (lambda (x y)
               (setf ignore-all-tests nil)
               (format *error-output* "Running tests for function ~A~%" x)
               (loop for test in y until ignore-all-tests do
                    (restart-case 
                        (if (not (funcall (cdr test)))
                            (error (with-output-to-string (str)
                                     (format str "Unit test ~A failed for function ~A~%" (car test) x)
                                     )
                                   )
                            )
                      (ignore () 
                        :report 
                        "Ignore this unit test."
                        nil
                        )
                      (ignore-function ()
                        :report
                        "Ignore all unit tests for this function"
                        (setf ignore-all-tests t)
                        )
                      )
                    )
               )
             unit-tests
             )
    )
  )