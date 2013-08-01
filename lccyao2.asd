(defsystem "lccyao2"
  :description "LCCyao compiler system version 2"
  :author "Benjamin Kreuter"
  :components ((:file "pcf2-bytecode")
               (:file "main" :depends-on ("pcf2-bytecode"))
               )
  )