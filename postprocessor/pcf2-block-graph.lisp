(defpackage :pcf2-block-graph
  (:use :common-lisp :pcf2-bc :setmap :utils)
  (:export pcf-graph
           pcf-basic-block
           get-cfg-top
           get-block-data
           get-block-op
           get-block-op-list
           get-block-succs
           get-block-preds
           get-block-faints
           get-block-consts
           get-block-lives
           get-block-id
           get-block-by-id
           get-block-base
           get-block-inputs
           get-block-dests
           block-with-lives
           block-with-faints
           block-with-consts
           block-with-op
           block-with-op-list
           block-with-preds
           block-with-succs
           new-block
           add-op
           add-pred
           add-succ
           new-cfg
           get-graph-map
           get-graph-bottom
           graph-insert
           cfg-with-bottom
           cfg-with-map
           pcf-not-const
           remove-block-from-cfg
           map-union-without-conflicts
           blocks-conflict
           )
  )
(in-package :pcf2-block-graph)

;;;
;;; the pcf-basic-block struct 
;;; and supporting macros
;;;

(defstruct (pcf-graph
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&PCF2 CFG Block Bottom ~A:~%" (get-graph-bottom struct))
                (format stream "~&PCF2 CFG Block Map ~A:~%" (get-graph-map struct)))))
  (cfg (map-empty :comp #'<) :type avl-set)
  (bottom nil)
  )

(defun get-graph-map (cfg)
  (pcf-graph-cfg cfg)
  )

(defun get-graph-bottom (cfg)
  (pcf-graph-bottom cfg)
  )

(defun graph-insert (key val cfg)
  (make-pcf-graph
    :cfg (map-insert key val (get-graph-map cfg))
    :bottom (get-graph-bottom cfg)
    ))                  

(defun new-cfg (&key (cfg (map-empty :comp #'<)) (bottom nil))
  (make-pcf-graph
    :cfg cfg
    :bottom bottom)
  )

(defun cfg-with-bottom (&key cfg bottom)
  (make-pcf-graph
    :cfg (get-graph-map cfg)
    :bottom bottom))

(defun cfg-with-map (&key cfg map)
  (make-pcf-graph
    :cfg map
    :bottom (get-graph-bottom cfg)))


(defstruct (pcf-basic-block
             (:print-function
              (lambda (struct stream depth)
                (declare (ignore depth))
                (format stream "~&PCF2 basic block ~A:~%" (get-block-id struct))
                (format stream "Op: ~A~%" (get-block-op struct))
                (format stream "Preds: ~A~%" (get-block-preds struct))
                (format stream "Succs: ~A~%" (get-block-succs struct))
                (format stream "Base: ~A~%" (get-block-base struct))
                (format stream "Faint-Out: ~A~%" (get-block-faints struct))
                ;;(format stream "Consts: ~A~%" (get-block-consts struct))
                (format stream "Live-Out: ~A~%" (get-block-lives struct))
                )
              )
             )
  (id)
  (op nil :type list)
  (base)
  (preds nil :type list)
  (succs nil :type list)
  ;; (out-set (empty-set) :type avl-set)
  (data (list (map-empty) (empty-set) (empty-set)) :type list) ;; this is a list of flow values; first is constants, second is faint variables 
  (:documentation "This represents a basic block in the control flow graph.")
  )


(defun get-block-id (blck)
  (pcf-basic-block-id blck))

(defun get-block-preds (blck)
  (pcf-basic-block-preds blck))

(defun get-block-succs (blck)
  (pcf-basic-block-succs blck))

(defun get-block-base (blck)
  (pcf-basic-block-base blck))

(defun get-block-op-list (blck)
  (pcf-basic-block-op blck))
  
(defun get-block-op (blck)
  (car (get-block-op-list blck)))

(defun get-block-data (blck)
  (pcf-basic-block-data blck))

(defun get-block-faints (blck)
  (second (pcf-basic-block-data blck)))

(defun get-block-consts (blck)
  (first (pcf-basic-block-data blck)))

(defun get-block-lives (blck)
  (third (pcf-basic-block-data blck)))

(defun get-block-inputs (blck)
  (reduce (lambda (in-list op)
            (typecase op
              (gate 
               (with-slots (op1 op2) op
                 (append in-list (list op1 op2))))
              (const
               (with-slots (op1) op
                 (append in-list (list op1))))
              (otherwise
               ;; can fill in the rest as needed
               in-list)))
  (get-block-op-list blck)
  :initial-value nil)
)

(defun get-block-dests (blck)
  (reduce (lambda (dest-list op)
            (typecase op
              (gate 
               (with-slots (dest) op
                 (append dest-list (list dest))))
              (const
               (with-slots (dest) op
                 (append dest-list (list dest))))
              (otherwise
               ;; can fill in the rest as needed
               dest-list)))
    (get-block-op-list blck)
    :initial-value nil)
)

(defun get-block-by-id (id blocks)
  (cdr (map-find id (get-graph-map blocks))))

(defun new-block (&key id op (base nil))
  (make-pcf-basic-block
   :id id
   :base base
   :op (list op)))

(defun block-with-base (new-base bb)
   (make-pcf-basic-block
    :id (get-block-id bb)
    :op  (get-block-op-list bb)
    :base new-base
    :preds (get-block-preds bb)
    :succs (get-block-succs bb)
    :data (get-block-data bb)))

(defun block-with-op (new-op bb)
  (make-pcf-basic-block
   :id (get-block-id bb)
   :op  new-op
   :base (get-block-base bb)    
   :preds (get-block-preds bb)
   :succs (get-block-succs bb)
   :data (get-block-data bb)))

(defun block-with-op-list (new-ops bb)
  (make-pcf-basic-block
   :id (get-block-id bb)
   :op  new-ops
   :base (get-block-base bb)    
   :preds (get-block-preds bb)
   :succs (get-block-succs bb)
   :data (get-block-data bb)))

(defun block-with-preds (preds bb)
  (make-pcf-basic-block
   :id (get-block-id bb)
   :op  (get-block-op-list bb)
   :base (get-block-base bb)
   :preds preds
   :succs (get-block-succs bb)
   :data (get-block-data bb)))

(defun block-with-succs (succs bb)
  (make-pcf-basic-block
   :id (get-block-id bb)
   :op  (get-block-op-list bb)
   :base (get-block-base bb)
   :preds (get-block-preds bb)
   :succs succs
   :data (get-block-data bb)))

(defun block-with-faints (new-faint bb)
  (make-pcf-basic-block
   :id (get-block-id bb)
   :op (get-block-op-list bb)
   :base (get-block-base bb)
   :preds (get-block-preds bb)
   :succs (get-block-succs bb)
   :data (list (get-block-consts bb) new-faint (get-block-lives bb))
   ))

(defun block-with-lives (new-lives bb)
  (make-pcf-basic-block
   :id (get-block-id bb)
   :op (get-block-op-list bb)
   :base (get-block-base bb)
   :preds (get-block-preds bb)
   :succs (get-block-succs bb)
   :data (list (get-block-consts bb) (get-block-faints bb) new-lives)
   ))

(defun block-with-consts (new-consts bb)
  (make-pcf-basic-block
   :id (get-block-id bb)
   :op (get-block-op-list bb)
   :base (get-block-base bb)
   :preds (get-block-preds bb)
   :succs (get-block-succs bb)
   :data (list new-consts (get-block-faints bb) (get-block-lives bb))
   ))

;; op is an opcode, bb is the block itself
(defmacro add-op (op bb &body body)
  `(let ((,bb (make-pcf-basic-block
               :id (get-block-id ,bb)
               :op (cons ,op (get-block-op-list ,bb))
               :base (get-block-base ,bb)
               :preds (get-block-preds ,bb)
               :succs (get-block-succs ,bb)
               :data (get-block-data ,bb)
               )))
     ,@body))

;; prd is an index, bb is the block itself
(defmacro add-pred (prd bb &body body)
  `(let ((,bb (make-pcf-basic-block
               :id (get-block-id ,bb)
               :op (get-block-op-list ,bb)
               :base (get-block-base ,bb)
               :preds (cons ,prd (get-block-preds ,bb))
               :succs (get-block-succs ,bb)
               :data (get-block-data ,bb)
               )))
     ,@body))

;; succ is an index, bb is the block itself
(defmacro add-succ (succ bb &body body)
  `(let ((,bb (make-pcf-basic-block
               :id (get-block-id ,bb)
               :op (get-block-op-list ,bb)
               :base (get-block-base ,bb)
               :preds (get-block-preds ,bb)
               :succs (cons ,succ (get-block-succs ,bb))
               :data (get-block-data ,bb)
               )))
     ,@body))



(defun remove-block-from-cfg (blck cfg)
  ;; remove this block from its preds' succs and its succs' preds
  ;; and add all of its succs to its preds' succs, and add all of its preds to its succs' preds
  (declare (optimize (debug 3)(speed 0)))
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


(defun merge-blocks (blck1 blck2)
  (make-pcf-basic-block
   :id (get-block-id blck1)
   :op (append (get-block-op-list blck1) (get-block-op-list blck2))
   :base (get-block-base blck1)
   :preds (reduce (lambda (preds x)
                    (if (or (member x preds) (= x (get-block-id blck1)))
                        preds
                        (append preds (list x))))
                  (get-block-preds blck2)
                  :initial-value (get-block-preds blck1))
   :succs (reduce (lambda (succs x)
                    (if (or (member x succs) (= x (get-block-id blck1)))
                        succs
                        (append succs (list x))))
                  (get-block-succs blck2)
                  :initial-value (get-block-succs blck1))
   :data (list
          (map-union-without-conflicts (get-block-consts blck1) (get-block-consts blck2))
          (set-union (get-block-faints blck1) (get-block-faints blck2))
          (set-union (get-block-lives blck1) (get-block-lives blck2))
          )
   )
  )

(defun map-union-without-conflicts (map1 map2)
  (let ((newmap (map-reduce (lambda (map-accum key val)
                              (declare (optimize (debug 3)(speed 0)))
                              (aif (map-val key map2 t)
                                   (if (equal it val)
                                       map-accum ;; already have the element
                                       (map-insert key 'pcf-not-const map-accum)) ;; element duplicates not equivalent
                                   (map-insert key val map-accum))) ;; if it's not found, it's new and needs to be added
                            map1
                            map2)))
    newmap))

(defun blocks-conflict (blck1 blck2)
  (let ((in-blck1 (get-block-inputs blck1))
        (in-blck2 (get-block-inputs blck2))
        (dest-blck1 (get-block-dests blck1))
        (dest-blck2 (get-block-dests blck2)))
    ;; blocks may not share inputs or dests, or have crossover between them
    ;; returns boolean value of whether the blocks conflict. t for yes, nil for no.
    (or (> 0 (length (intersection in-blck1 in-blck2)))
        (> 0 (length (intersection dest-blck1 dest-blck2)))
        (> 0 (length (intersection in-blck1 dest-blck2)))
        (> 0 (length (intersection in-blck2 dest-blck1)))))
)
