;; New lccyao compiler
;;
;; This compiler will improve on the original lccyao's organization,
;; efficiency, and the memory model of the output format.
;;
;; Author: Benjamin Kreuter

(defpackage :lccyao-main (:use :lcc-translator 
                               :pcf2-bc 
                               :pcf2-interpreter 
                               :common-lisp
                               :skew-list
                               :dataflow)
            (:export test-interp
                     pcf-compile
                     save-pcf-ops
                     load-pcf-ops
                     pcf-simulate
                     test-get-ops-from-cfg)
            (:import-from :lcc-bc read-instructions)
            )
(in-package :lccyao-main)

(defun pcf-compile (fname)
  "Compile the bytecode in \"fname\" and return a list of PCF2 instructions"
  (with-open-file (strm fname :direction :input)
    (first (exec-instructions (read-instructions strm)))
    )
  )

(defun save-pcf-ops (fname ops)
  "Save PCF2 bytecode to \"fname\""
  (with-open-file (strm fname :direction :output :if-exists :supersede :if-does-not-exist :create)
    (let ((*print-pretty* nil))
      (mapc #'(lambda (x)
                (format strm "~A~%" x)
                )
            ops)
      )
    t
    )
  )

(defun load-pcf-ops (fname)
  "Load PCF2 bytecode from \"fname\""
  (with-open-file (inpt fname :direction :input)
    (read-bytecode inpt)
    )
  )

(defun pcf-simulate (ops inpname)
  "Simulate the execution of the instructions in \"ops\" using inputs from \"inpname\""
  (declare (optimize (debug 3) (speed 0)))
  (restart-case
      (let ((state (setup-labels ops
                                 (with-open-file (inputs inpname :direction :input)
                                   (init-state 20000 ops inputs 16384 16384)
                                   )
                                 )
              )
            )
        (run-opcodes state)
        )
    (set-watch ()
      (format t "~&Enter watched address:~%")
      (add-to-watch-list (parse-integer (read-line) :junk-allowed nil))
      (pcf-simulate ops inpname)
      )
    (set-watch-for-bit ()
      (format t "~&Enter watched address:~%")
      (add-to-watch-list-bit (parse-integer (read-line) :junk-allowed nil))
      (pcf-simulate ops inpname)
      )
    )
  )

(defun test-interp (fname inpname)
  (let* ((ops (first 
               (with-open-file (strm fname :direction :input) (exec-instructions (read-instructions strm)))))
         (state (setup-labels ops (with-open-file (inputs inpname :direction :input) (init-state 20000 ops inputs 16384 16384))))
         )
    ;; (with-open-file (dump (concatenate 'string fname ".pcf2") :direction :output)
    ;;   (loop for op in ops do
    ;;        (format dump "~A~%" op)
    ;;        )
    ;;   )
    (assert (typep (first ops) 'pcf2-bc:initbase))
    (run-opcodes state)
    )
  )

(defun test-get-ops-from-cfg ()
  (declare (optimize (debug 3) (speed 0)))
  (let* ((ops (load-pcf-ops "test.pcf2"))
         (op2 (ops-from-cfg (make-cfg ops) (get-lbls-in-order ops nil)))
         ) 
    (labels ((assert-equality (o1 o2)
               (if (and o1 o2)
                   (progn
                     (assert (equalp (car o1) (car o2)))
                     (assert-equality (cdr o1) (cdr o2))
                     )
                   )
               )
             )
      (assert-equality ops op2)
      )
    )
  )