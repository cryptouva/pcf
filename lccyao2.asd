(defsystem "lccyao2"
  :description "LCCyao compiler system version 2"
  :author "Benjamin Kreuter, Benjamin Terner"
  :components (
	       (:file "main" :depends-on (utilities translator interpreter postprocessor))
	       (:module unit 
			:components ((:file "unit")
                                     )
			)
	       (:module utilities
		       :components ((:file "string-tokenizer")
				    (:file "skewlist")
				    (:file "avl2" :depends-on ("utils"))
				    (:file "pairingheap")
				    (:file "utils")
				    (:file "setmap2" :depends-on ("avl2"))
				    (:file "pcf2-bytecode")
                                    (:file "hashset")
                                    (:file "rle-avl" :depends-on ("utils"))
                                    (:file "setmap-rle" :depends-on ("rle-avl"))
				    )
                       :depends-on (unit)
                       )
	       (:module translator
			:components (
				     ;;(:file "lcc-translator"
                                     (:file "lcc-translator-inter"
                                            :depends-on ("lcc-bc" "lcc-dataflow-inter" "lcc-const-inter"))
				     (:file "lcc-bc")
				     (:file "lcc-const-inter" :depends-on ("lcc-bc" "lcc-dataflow-inter"))
				    ; (:file "lcc-pointer-analysis" :depends-on ("lcc-bc" "lcc-dataflow" )) not in use
				     (:file "lcc-dataflow-inter" :depends-on ("lcc-bc"))
              			    ; (:file "dataflow")
				    ; (:file "pointer-analysis" :depends-on ("dataflow" utilities)) not in use
				    ; (:file "reachingdefs" :depends-on ("datafloww" utilities)) not in use
				    ; (:file "deadcode" :depends-on ("dataflow" utilities)) not in use
				    ; (:file "constprop" :depends-on ("dataflow" utilities)) not in use
				     )
			:depends-on (utilities)
                        )
	       (:module interpreter
			:components (
				     (:file "pcf2-interpreter")
				     )
			:depends-on (utilities unit)
			)
	       (:module postprocessor
			:components (
                                      ;; (:file "pcf2-block-graph")
                                      ;; (:file "pcf2-flow-utils" :depends-on ("pcf2-block-graph"))
                                      ;; (:file "pcf2-dataflow" :depends-on ("pcf2-block-graph" "pcf2-use-map" "pcf2-flow-utils"))
                                      ;; (:file "pcf2-faintgate" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                      ;; (:file "pcf2-const" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                      ;; (:file "pcf2-use-map" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                      ;; (:file "pcf2-live" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                     
                                     (:file "pcf2-block-graph-rle")
                                     (:file "pcf2-flow-utils" :depends-on ("pcf2-block-graph-rle"))
				     (:file "pcf2-dataflow-rle" :depends-on ("pcf2-block-graph-rle" "pcf2-use-map" "pcf2-flow-utils"))
				     (:file "pcf2-faintgate-rle" :depends-on ("pcf2-block-graph-rle" "pcf2-flow-utils"))
                                     (:file "pcf2-const-rle" :depends-on ("pcf2-block-graph-rle" "pcf2-flow-utils"))
                                     (:file "pcf2-use-map" :depends-on ("pcf2-block-graph-rle" "pcf2-flow-utils"))
                                     (:file "pcf2-live-rle" :depends-on ("pcf2-block-graph-rle" "pcf2-flow-utils"))
                                     )
			:depends-on (utilities)
			)
	       )
  )
