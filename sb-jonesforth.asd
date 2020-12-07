(defsystem "sb-jonesforth"
  :name "sb-jonesforth"
  :description "A port of jonesforth using SBCL compiler internals."
  :components
  ((:file "package")
   (:file "assembler" :depends-on ("package"))
   (:file "util" :depends-on ("assembler"))
   (:file "jonesforth" :depends-on ("assembler"))
   (:file "vop" :depends-on ("assembler"))
   (:file "go" :depends-on ("vop" "jonesforth"))))
