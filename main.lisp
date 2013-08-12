;; New lccyao compiler
;;
;; This compiler will improve on the original lccyao's organization,
;; efficiency, and the memory model of the output format.
;;
;; Author: Benjamin Kreuter

(defpackage :lccyao-main (:use :lcc-translator :pcf2-bc :pcf2-interpreter :common-lisp)
            (:export test-interp))
(in-package :lccyao-main)

(defun test-interp ()
  (let* ((ops (read-bytecode (open "test.pcf2"))
           )
         (state (setup-labels ops (init-state 20 ops)))
         )
    (run-opcodes state)
    )
  )