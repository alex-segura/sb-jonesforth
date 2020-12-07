(defpackage #:forth
  (:use #:cl)
  (:shadow #:disassemble #:byte)
  (:import-from
   #:sb-vm
   #:ea
   #:inst
   #:system-area-pointer
   #:rax-tn
   #:rbx-tn
   #:rcx-tn
   #:rdx-tn
   #:rsi-tn
   #:rdi-tn
   #:rsp-tn
   #:rbp-tn
   #:r14-tn
   #:r13-tn
   #:r15-tn)
  (:export #:go-forth))
