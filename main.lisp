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
               (with-input-from-string (strm "proc main 8 4
ADDRLP4 0
CNSTU4 15
CNSTU4 23
ADDU4
ASGNU4") (exec-instructions (read-instructions strm)))))
         (state (setup-labels ops (init-state 200 ops)))
         )
    (run-opcodes state)
    )
  )