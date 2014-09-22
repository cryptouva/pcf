;; Dataflow analysis framework for PCF2 bytecode. We use this to eliminate unnecessary gates that don't contribute to the output

(defpackage :pcf2-dataflow
  (:use :common-lisp :pcf2-bc :setmap :utils :pcf2-block-graph :pcf2-use-map)
  (:export make-pcf-cfg
           flow-forward-test
           flow-backward-test
           flow-forward
           flow-backward
           optimize-circuit
           wire-use-map
           )
  )
(in-package :pcf2-dataflow)


;; these special functions are included by the PCF interpreters and therefore will not have lookups in the .PCF2 file
;; alice and bob return unsigned integers
;; output_alice and output_bob give outputs to the parties
(defparameter *specialfunctions* (set-from-list (list "alice" "bob" "output_alice" "output_bob") :comp #'string<))


;; id should be an integer
;; val should be a block
;; blocks should be the map of blocks
(defmacro insert-block (id val blocks &body body)
  `(let ((,blocks (graph-insert ,id ,val ,blocks)))
     ,@body))

(defun get-idx-by-label (targ lbls)
  (cdr (map-find targ lbls)))

;;;
;;;
;;; cfg-basic-block functions that instruct how to behave when building the cfg and encountering all of the possible ops
;;;
;;;

(defgeneric cfg-basic-block (next-op cur-op blocks lbls fns idx callstack base-stack)
  (:documentation "update the entities in the cfg for each op that we encounter from ops")
  ;; blocks is a map of all idx to basic blocks
  ;; lbls is a map of all of the label names to idxs
  ;; fns is the set of function names
  ;; idx is the index of current op
  )

;; this one catches all the stuff i don't define. it performs a standard operation.
(defmacro add-standard-block () ; next-op cur-op blocks lbls fns idx
  `(let ((newblock (new-block :id idx :op cur-op :base (car base-stack))))
     (add-succ (1+ idx) newblock
       (close-add-block))))

(defmacro close-add-block ()
  `(insert-block idx newblock blocks
     (list next-op
           blocks
           lbls
           fns
           (1+ idx)
           callstack
           base-stack)))

(defmethod cfg-basic-block (next-op (cur-op instruction) blocks lbls fns idx callstack base-stack)
  (add-standard-block))

(defmacro definstr (type &body body)
  "PCF instruction processing methods are defined with this macro.  It is a convenience macro that ensures that the method takes the right number of arguments."
  `(defmethod cfg-basic-block ((next-op instruction) (cur-op ,type) blocks lbls fns idx callstack base-stack)
     (declare (optimize (debug 3) (speed 0)))
     (aif (locally ,@body)
          it
          (add-standard-block)
          )))

(defmacro initbase-instr ()
  `(with-slots (base) cur-op
     (let ((newblock (new-block :id idx :op cur-op :base (car base-stack)))
           (base-stack (list base)))
       ;; this one's successor is ALWAYS main
       (add-succ (get-idx-by-label "main" lbls) newblock
         (close-add-block)))))

(definstr initbase
  (initbase-instr))

(defmacro ret-instr ()
  `(let ((newblock (new-block :id idx :op cur-op :base (car base-stack))))
     (close-add-block)))
;; successors are added after the initial cfg is built using the call/ret maps

(definstr ret
  (ret-instr))

(definstr call
  (with-slots (fname) cur-op
    (cond
      ((set-member fname *specialfunctions*)
       (add-standard-block))
      (t (let ((newblock (new-block :id idx :op cur-op :base (car base-stack))))
           (add-succ (1+ idx) newblock
               (add-succ (get-idx-by-label fname lbls) newblock
                   (close-add-block))))))))

(defmacro branch-instr ()
  `(with-slots (targ) cur-op
     (let ((newblock (new-block :id idx :op cur-op :base (car base-stack))))
       (add-succ (1+ idx) newblock
           (add-succ (get-idx-by-label targ lbls) newblock
               (close-add-block))))))

(definstr branch
  (branch-instr))

(defmethod cfg-basic-block ((next-op label) (cur-op instruction) blocks lbls fns idx callstack base-stack)
  (declare (optimize (debug 3)(speed 0)))
  (with-slots (str) next-op
    (cond
      ((set-member str fns) ;; if we're about to declare a function, it doesn't get added as a successor right now. main is preceded by initbase and functions will get their successors from the call instruction 
       (typecase cur-op
         (initbase (initbase-instr))
         (t
          (let ((newblock (new-block :id idx :op cur-op :base (car base-stack))))
            (format t "~A~%" newblock)
            (format t "~A~%" next-op)
            (close-add-block))))) 
      (t 
       (typecase cur-op
         ;; not every instruction can be followed by "label," so here we identify the important things that some might have to do
         (branch (branch-instr))
         (initbase (initbase-instr))
         (ret (ret-instr))
         (t (add-standard-block)))))))



(defun get-label-and-fn-map (ops)
  ;; iterate through all of the ops; when hit a label, insert its (name->idx) pair into lbls
  ;; also get the names of all of the functions (other than main) that are called
  ;; ret-addrs will contain the return addresses of all of the functions {(fname)->(return-address)}
  ;; call-addrs will contain addresses where each function is called { (addr)->(fname)}
  (reduce #'(lambda(y op)
              (declare (optimize (debug 3) (speed 0)))
              (let ((lbls (first y))
                    (fns (second y))
                    (idx (third y))
                    (ret-addrs (fourth y))
                    (callstack (fifth y))
                    (call-addrs (sixth y)))
                (typecase op
                  (label 
                   (with-slots (str) op
                     (if (or (equalp (subseq str 0 1) "$")
                             (equalp str "pcfentry")) ;; main can be included here because it returns;
                         (list 
                          (map-insert str idx lbls)
                          fns
                          (+ 1 idx)
                          ret-addrs
                          callstack
                          call-addrs) ;; we have a regular label
                         (list
                          (map-insert str idx lbls)
                          fns
                          (+ 1 idx)
                          ret-addrs ;; some function whose ret address should be known
                          (cons str callstack)
                          call-addrs ))))
                  (call (with-slots (fname) op
                          (list lbls
                                (set-insert fns fname)
                                (+ 1 idx)
                                ret-addrs
                                callstack
                                (if (set-member fname *specialfunctions*)
                                    call-addrs
                                    (map-insert idx fname call-addrs)))))
                  (ret (list lbls
                             fns
                             (+ 1 idx)
                             (map-insert (car callstack) idx ret-addrs)
                             (cdr callstack)
                             call-addrs))
                  (t (list lbls fns (+ 1 idx) ret-addrs callstack call-addrs)))))
          ops
          :initial-value (list (map-empty :comp #'string<)
                               (empty-set :comp #'string<)
                               0
                               (map-empty :comp #'string<)
                               nil
                               (map-empty :comp #'<))))


(defun find-preds (f-cfg)
  (declare (optimize (debug 3) (speed 0)))
  ;;(print "find preds")
  ;; for every item in blocks, get its successors and update those to identify a predecessor
  (map-reduce #'(lambda(cfg blockid blck) 
		  (reduce (lambda (cfg* succ)
			    (declare (optimize (debug 3)(speed 0)))
                            (let ((updateblock (get-block-by-id succ cfg*))
				  ;; (blockid (parse-integer blockid)
                                  )
			      (add-pred blockid updateblock
                                  (insert-block (get-block-id updateblock) updateblock cfg*
                                    cfg*))))
			  (get-block-succs blck) ; for each successor, add the pred
		 	  :initial-value cfg))
	      (get-graph-map f-cfg) ;map
	      f-cfg ;state
	      ))

(defun update-ret-succs (f-cfg call-addrs ret-addrs)
  ;; reduce over all the calling addresses in the cfg to update their return addresses. 1:1 map of call to return addresses
  (declare (optimize (debug 3)(speed 0)))
  ;;(print "update-ret-succs")
  ;;(print call-addrs)
  (first (map-reduce #'(lambda (state address fname)
                         (let ((cfg (first state))
                               (call-addrs (second state))
                               (ret-addrs (third state)))
                           (let ((retblock (get-block-by-id (get-idx-by-label fname ret-addrs) cfg)))
                             (add-succ (1+ address) retblock
                                 (insert-block (get-block-id retblock) retblock cfg
                                   (list
                                    cfg
                                    call-addrs
                                    ret-addrs))))))
                     call-addrs
                     (list f-cfg call-addrs ret-addrs))))


(defun make-pcf-cfg (ops)
  (declare (optimize (debug 3) (speed 0)))
  (let ((op1 (first ops))
        (restops (rest ops))
	(lbl-fn-map (get-label-and-fn-map ops)))
    ;;(print lbl-fn-map)
    (let* ((reduce-forward
            (reduce #'(lambda(x y)
                        (declare (optimize (debug 3)(speed 0)))
                        (apply #'cfg-basic-block (cons y x)))
                    restops
                    :initial-value (list op1
					 (new-cfg) ; blocks 
					 (first lbl-fn-map) ;; lbls
					 (second lbl-fn-map) ;; fns
					 0 ;;idx
                                         nil ;; callstack
                                         nil ;; base-stack
                                         )))
           (blocks (second reduce-forward))
           (forward-cfg
            (insert-block 
                (fifth reduce-forward) ;id
                (new-block :id (fifth reduce-forward)
                           :op (first reduce-forward)
                           :base (seventh reduce-forward))
                blocks
              blocks))
           (cfg-bottom (cfg-with-bottom :cfg forward-cfg :bottom (fifth reduce-forward)))
           (preds (find-preds (update-ret-succs cfg-bottom
                                                (sixth lbl-fn-map)
                                                (fourth lbl-fn-map))
                              )))
      preds
      )))


;;; the overall order of flow operations (once the cfg is constructed) should be as follows:
;;; 1. usage map
;;; 2. live-variable
;;; 3. const
;;; 4. faint-variable
;;; (although one and two are interchangeable, usage-map is simpler to perform 


;; when flowing,
;; each node carries info about its own data
;; and updates its information using predecessors' inputs
;; then, if changed, it adds its successors to the worklist
;; flow functions should be parameterizable by method used to get successors

;; need:
;; make sure that every node is touched by the worklist at least once
;; then, pull from the worklist until it is nil, remembering to add successors every time a node's value changes

;; need to construct some functions for comparing datas with those that are just "top". Any confluence operation with "top" (conf x top) = x


;; a couple of functions to test forward and backward flows

(defun flow-backward-test (ops flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn)
  (let* ((cfg (make-pcf-cfg ops))
         (worklist (map-keys (get-graph-map cfg))))
    (do-flow cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn worklist (set-from-list worklist))))

(defun flow-forward-test (ops flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn)
  (let* ((cfg (make-pcf-cfg ops))
         (worklist (reverse (map-keys (get-graph-map cfg)))))
    (do-flow cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn worklist (set-from-list worklist))))

;; the actual flow-forward and flow-backward functions (could be replaced with macros and a single flow function, but not necessary

(defun flow-forward (cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn)
  (let ((worklist (reverse (map-keys (get-graph-map cfg)))))
    (do-flow cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn worklist (set-from-list worklist))))

(defun flow-backward (cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn)
  (let ((worklist (map-keys (get-graph-map cfg))))
    (do-flow cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn worklist (set-from-list worklist))))

;; functions for the implementation of the worklist algorithm

(defun flow-once (cur-node cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn)
  (declare (optimize (debug 3)(speed 0)))
  ;;(format t "~A~%" cur-node)
  (let ((new-flow (funcall flow-fn cur-node cfg)))
    (insert-block (get-block-id cur-node) (funcall set-data-fn new-flow cur-node) cfg
      (let ((vals (reduce (lambda (state neighbor-id)
                            (let* ((cfg (first state))
                                   (worklist (second state))
                                   (neighbor-flow (funcall get-data-fn (get-block-by-id neighbor-id cfg)))
                                   (compare-flow (funcall join-fn new-flow neighbor-flow)))
                              (if (funcall weaker-fn compare-flow neighbor-flow)
                                  (list (insert-block
                                            neighbor-id
                                            (funcall set-data-fn compare-flow (get-block-by-id neighbor-id cfg)) 
                                            cfg
                                          cfg)
                                        (append worklist (list neighbor-id)))
                                  state)))
                          (funcall get-neighbor-fn cur-node)
                          :initial-value (list cfg nil))))
        (values (first vals) (second vals))))))
#|    
  (insert-block (get-block-id cur-node) (funcall set-data-fn new-flow cur-node) cfg
      (values cfg
              (reduce (lambda (worklist neighbor-id)
                        (let* ((neighbor-flow (funcall get-data-fn (get-block-by-id neighbor-id cfg)))
                               (compare-flow (funcall join-fn new-flow neighbor-flow)))
                          (if (funcall weaker-fn compare-flow neighbor-flow)
                              (append worklist (list neighbor-id))
                              worklist)))
                      (funcall get-neighbor-fn cur-node)
                      :initial-value nil)))))
|#


(defun do-flow (cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn worklist worklist-set)
  (declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (if (null worklist)
      cfg ; done
      (let* ((cur-node-id (car worklist))
             (worklist (cdr worklist))
             (new-work-set 
              (set-remove worklist-set cur-node-id)))
        (multiple-value-bind (cfg* worklist*) (flow-once (get-block-by-id cur-node-id cfg) cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn)
          (let ((more-work (reduce (lambda (wlist candidate)
                                     (if (set-member candidate new-work-set)
                                         wlist
                                         (append wlist (list candidate))))
                                   worklist*
                                   :initial-value worklist))
                (more-work-set (set-union (set-from-list worklist*) new-work-set)))
            (do-flow cfg* flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn more-work more-work-set))))))
#|
       (multiple-value-bind (cfg* worklist*) (flow-once (get-block-by-id cur-node-id cfg) cfg flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn)
          (let ((more-work (reduce (lambda (wlist candidate)
                                     (if (member candidate worklist)
                                         worklist
                                         (append wlist (list candidate))))
                                   worklist*
                                   :initial-value nil)))
            (do-flow cfg* flow-fn join-fn weaker-fn get-neighbor-fn get-data-fn set-data-fn (append worklist more-work)))))))
|#


(defun find-wire-uses (cfg)
  ;; the idea here is to compute the first and last uses of all the wires in the map.
  ;; we represent this as a map from wireid -> (cons first-use last-use). returns a map of these.
  (map-reduce (lambda (map blockid blck)
                (declare (ignore blockid))
                (let ((wires (compute-used-wires (get-block-op blck))))
                  ;; we don't have to ignore this, but better for decoupling if we use the accessor on the block itself
                  (reduce (lambda (mp wire)
                            (aif (map-val wire mp t)
                                 (map-insert wire (cons (car it) (get-block-id blck)) mp) ;; was found, preserve first and get new last
                                 (map-insert wire (cons (get-block-id blck) (get-block-id blck)) mp))) ;; not found, this is first use. insert blockid for first and last
                          wires
                          :initial-value map)))
              (get-graph-map cfg)
              (map-empty)))


(defun wire-use-map (ops)
  (find-wire-uses (make-pcf-cfg ops)))



;; functions for eliminating gates (and potentially other pcf2 ops) from the cfg as a result of the analysis we've performed

(defun remove-block-from-cfg (blck cfg)
  ;; remove this block from its preds' succs and its succs' preds
  ;; and add all of its succs to its preds' succs, and add all of its preds to its succs' preds
  (declare (optimize (debug 3)(speed 0)))
  ;;(break)
  (let ((preds (get-block-preds blck))
        (succs (get-block-succs blck))
        (blckid (get-block-id blck)))
    (let ((remove-back (reduce (lambda(cfg* pred)
                                 (declare (optimize (debug 3) (speed 0)))
                                 ;;(break)
                                 (let* ((predblck (map-val pred cfg*))
                                        (predsuccs (get-block-succs predblck)))
                                   (map-insert pred
                                               (block-with-succs (append (remove blckid predsuccs) succs) predblck)
                                               cfg*)))
                               preds
                               :initial-value cfg)))
      (let ((remove-forward (reduce (lambda(cfg* succ)
                                      (declare (optimize (debug 3)(speed 0)))
                                      (let* ((succblck (map-val succ cfg*))
                                             (succpreds (get-block-preds succblck)))
                                        (map-insert succ (block-with-preds (append (remove blckid succpreds) preds) succblck) cfg*)))
                                    succs
                                    :initial-value remove-back)))
        (map-remove blckid remove-forward)))))
      
(defmacro with-true-addresses ((&rest syms) &body body)
  `(let ,(loop for sym in syms
            collect `(,sym (+ ,sym (aif base it 0))))
     ,@body))

(defun eliminate-extra-gates (cfg)
  ;; gates that are unnecessary may be eliminated here!
  ;; rules:
  ;; if the block is a gate with a constant in its output, replace the gate with a const 
  ;; if the output of the gate is faint, remove it entirely
  ;; remember that blocks in the original cfg may not be the same as those we find later
  (declare (optimize (debug 3) (speed 0)))
  (map-reduce (lambda (cfg* blockid blck)
                (declare (ignore blck))
                (aif (map-val blockid cfg* t) 
                     (let* ((blk it)
                            (op (get-block-op blk))
                            (base (get-block-base blk))
                            (faints (get-block-faints blk)))
                       ;;(format t "looking at ~A~% ~A~%" blockid blk)
                       (typecase op
                         (gate (with-slots (dest op1 op2) op
                                 (with-true-addresses (dest op1 op2)
                                   (if (not (and (set-member op1 faints) (set-member op2 faints))) ;; this logic is faint gate in reverse; if the gate were not live, both of its inputs would be also
                                       (remove-block-from-cfg blk cfg*);; remove this op from the cfg
                                       (aif (map-val dest (get-block-consts blk) t)
                                            (if (not (equalp it 'pcf2-block-graph:pcf-not-const))
                                                (map-insert blockid
                                                            (block-with-op (list (make-instance 'const :dest dest :op1 it)) blk)
                                                            cfg*)
                                                cfg*)
                                            cfg*)))))
                         (otherwise (map-insert blockid blk cfg*))))
                     cfg*))
              cfg
              cfg))

(defun extract-ops (cfg)
  (map-reduce (lambda (ops id blck)
                (declare (ignore id))
                (cons (get-block-op blck) ops))
              cfg
              nil))


;; the big cahoona
(defun optimize-circuit (cfg)
  ;;(print cfg)
  (reverse (extract-ops (eliminate-extra-gates (get-graph-map cfg))))
)
