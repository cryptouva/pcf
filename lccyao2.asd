(defsystem "lccyao2"
  :description "LCCyao compiler system version 2"
  :author "Benjamin Kreuter"
  :components ((:file "string-tokenizer")
               (:file "skewlist")
               (:file "avl")
               (:file "pairingheap")
               (:file "setmap" :depends-on ("avl"))
               (:file "pcf2-bytecode")
               (:file "pcf2-interpreter" :depends-on ("pcf2-bytecode" "skewlist"))
               (:file "lcc-translator" :depends-on ("pcf2-bytecode" "string-tokenizer" "pairingheap" "skewlist"))
               (:file "dataflow" :depends-on ("pcf2-bytecode" "setmap"))
               (:file "main" :depends-on ("lcc-translator" "pcf2-interpreter" "dataflow"))
               )
  )