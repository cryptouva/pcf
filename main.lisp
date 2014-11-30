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
                               :lcc-dataflow
                               :pcf2-block-graph
			       :pcf2-dataflow
                               :pcf2-faintgate
                               :pcf2-const
                               :pcf2-live
                               )
            (:export test-interp
                     pcf-cfg
                     pcf-deadgates
                     pcf-compile
                     save-pcf-ops
                     load-pcf-ops
                     pcf-simulate
                     test-get-ops-from-cfg
                     faint-analyze-cfg
                     const-analyze-cfg
                     live-analyze-cfg
                     test-analyze-cfg
                     compute-wire-uses)
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
 
(defun pcf-cfg (ops)
  (make-pcf-cfg ops))

(defun compute-wire-uses (ops)
  (wire-use-map ops)
  )

#|
(defun live-analyze-cfg (ops)
  (flow-backward-test ops #'live-flow-fn #'live-confluence-op #'live-weaker-fn #'get-block-preds #'get-block-lives #'block-with-lives))


(defun const-analyze-cfg (ops)
  (flow-forward-test ops #'const-flow-fn #'const-confluence-op #'const-weaker-fn #'get-block-succs #'get-block-consts #'block-with-consts))
|#

(defun faint-analyze-cfg (ops)
  (let* ((cfg (make-pcf-cfg ops))
         (wiremap (find-wire-uses cfg))
         (live-cfg (flow-backward cfg #'live-flow-fn #'live-confluence-op #'live-weaker-fn #'get-block-preds #'get-block-lives #'block-with-lives wiremap))
         (const-cfg (flow-forward live-cfg #'const-flow-fn #'const-confluence-op #'const-weaker-fn #'get-block-succs #'get-block-consts #'block-with-consts wiremap))
         (faint-cfg (flow-backward const-cfg #'faint-flow-fn #'faint-confluence-op #'faint-weaker-fn #'get-block-preds #'get-block-faints #'block-with-faints wiremap)))
    faint-cfg))

(defun test-analyze-cfg (ops)
  (let* ((cfg (make-pcf-cfg ops))
         (wiremap (find-wire-uses cfg))
         (live-cfg (flow-backward cfg #'live-flow-fn #'live-confluence-op #'live-weaker-fn #'get-block-preds #'get-block-lives #'block-with-lives wiremap))
         (const-cfg (flow-forward live-cfg #'const-flow-fn #'const-confluence-op #'const-weaker-fn #'get-block-succs #'get-block-consts #'block-with-consts wiremap))
         (faint-cfg (flow-backward const-cfg #'faint-flow-fn #'faint-confluence-op #'faint-weaker-fn #'get-block-preds #'get-block-faints #'block-with-faints wiremap)))
    (optimize-circuit faint-cfg)))


(defun pcf-simulate (ops inpname)
  "Simulate the execution of the instructions in \"ops\" using inputs from \"inpname\""
  (declare (optimize (debug 3) (speed 0)))
  (restart-case
      (let ((state (setup-labels ops
                                 (with-open-file (inputs inpname :direction :input)
                                   (init-state 2000000 ops inputs 16384 16384)
                                   )
                                 )
              )
            )
        (print (run-opcodes state))
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


(unit:run-unit-tests)
