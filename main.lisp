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

(defun test-interp ()
;  (let* ((ops (read-bytecode (open "test.pcf2"))
;          )
  (let* ((ops (first 
               (with-open-file (strm "16384.lcc" :direction :input) (exec-instructions (read-instructions strm)))))
         (state (setup-labels ops (with-open-file (inputs "inp.txt" :direction :input) (init-state 3000 ops inputs 16384 16384))))
         )
    (print ops)
    (run-opcodes state)
    )
  )