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
				    (:file "avl")
				    (:file "pairingheap")
				    (:file "utils")
				    (:file "setmap" :depends-on ("avl"))
				    (:file "pcf2-bytecode")
				    )
                       :depends-on (unit)
                       )
	       (:module translator
			:components (
				     (:file "lcc-translator"
					    :depends-on ("lcc-bc" "lcc-dataflow" "lcc-const"))
				     (:file "lcc-bc")
				     (:file "lcc-const" :depends-on ("lcc-bc" "lcc-dataflow"))
				    ; (:file "lcc-pointer-analysis" :depends-on ("lcc-bc" "lcc-dataflow" )) not in use
				     (:file "lcc-dataflow" :depends-on ("lcc-bc"))
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
                                     (:file "pcf2-block-graph")
                                     (:file "pcf2-flow-utils" :depends-on ("pcf2-block-graph"))
				     (:file "pcf2-dataflow" :depends-on ("pcf2-block-graph" "pcf2-use-map" "pcf2-flow-utils"))
				     (:file "pcf2-faintgate" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                     (:file "pcf2-const" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                     (:file "pcf2-use-map" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                     (:file "pcf2-live" :depends-on ("pcf2-block-graph" "pcf2-flow-utils"))
                                     )
			:depends-on (utilities)
			)
	       )
  )
