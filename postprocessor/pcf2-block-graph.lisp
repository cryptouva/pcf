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
           block-with-lives
           block-with-faints
           block-with-consts
           block-with-op
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
                (format stream "Faint-Out: ~A~%" (get-block-faints struct))
                ;;(format stream "Consts: ~A~%" (get-block-consts struct))
                (format stream "Live-Out: ~A~%" (get-block-lives struct))
                )
              )
             )
  (id)
  (op nil :type list)
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

(defun get-block-by-id (id blocks)
  (cdr (map-find id (get-graph-map blocks))))

(defun new-block (&key id op)
  (make-pcf-basic-block
   :id id
   :op (list op)))


(defun block-with-op (new-op bb)
   (make-pcf-basic-block
               :id (get-block-id bb)
               :op  new-op
               :preds (get-block-preds bb)
               :succs (get-block-succs bb)
               :data (get-block-data bb)))

(defun block-with-preds (preds bb)
   (make-pcf-basic-block
               :id (get-block-id bb)
               :op  (get-block-op-list bb)
               :preds preds
               :succs (get-block-succs bb)
               :data (get-block-data bb)))

(defun block-with-succs (succs bb)
   (make-pcf-basic-block
               :id (get-block-id bb)
               :op  (get-block-op-list bb)
               :preds (get-block-preds bb)
               :succs succs
               :data (get-block-data bb)))

(defun block-with-faints (new-faint bb)
  (make-pcf-basic-block
               :id (get-block-id bb)
               :op (get-block-op-list bb)
               :preds (get-block-preds bb)
               :succs (get-block-succs bb)
               :data (list (get-block-consts bb) new-faint (get-block-lives bb))
               ))

(defun block-with-lives (new-lives bb)
  (make-pcf-basic-block
               :id (get-block-id bb)
               :op (get-block-op-list bb)
               :preds (get-block-preds bb)
               :succs (get-block-succs bb)
               :data (list (get-block-consts bb) (get-block-faints bb) new-lives)
               ))

(defun block-with-consts (new-consts bb)
  (make-pcf-basic-block
               :id (get-block-id bb)
               :op (get-block-op-list bb)
               :preds (get-block-preds bb)
               :succs (get-block-succs bb)
               :data (list new-consts (get-block-faints bb) (get-block-lives bb))
               ))

;; op is an opcode, bb is the block itself
(defmacro add-op (op bb &body body)
  `(let ((,bb (make-pcf-basic-block
               :id (get-block-id ,bb)
               :op (cons ,op (get-block-op-list ,bb))
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
               :preds (get-block-preds ,bb)
               :succs (cons ,succ (get-block-succs ,bb))
               :data (get-block-data ,bb)
               )))
     ,@body))
