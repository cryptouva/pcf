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
                               :skew-list)
            (:export test-interp))
(in-package :lccyao-main)
(use-package :lcc-translator)

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