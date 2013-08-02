(defsystem "lccyao2"
  :description "LCCyao compiler system version 2"
  :author "Benjamin Kreuter"
  :components ((:file "string-tokenizer")
               (:file "pcf2-bytecode")
               (:file "lcc-translator" :depends-on ("pcf2-bytecode" "string-tokenizer"))
               (:file "main" :depends-on ("lcc-translator"))
               )
  )