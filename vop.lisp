(in-package #:sb-vm)

(define-vop (forth::%go-forth)
  (:translate forth::%go-forth)
  (:policy :fast-safe)
  (:args
   (entry :scs (sap-reg) :target rax)
   (base :scs (sap-reg) :target rbx))
  (:arg-types system-area-pointer system-area-pointer)
  (:results)
  (:temporary (:sc sap-reg :offset rax-offset :from (:argument 0)) rax)
  (:temporary (:sc sap-reg :offset rbx-offset :from (:argument 1)) rbx)
  (:generator 0
   (inst push r8-tn)
   (inst push r9-tn)
   (inst push r10-tn)
   (inst push r11-tn)
   (inst push r12-tn)
   (inst push r13-tn)
   (inst push r14-tn)
   (inst push r15-tn)
   (inst push rdi-tn)
   (inst push rsi-tn)
   (inst push rsp-tn)
   (inst push rbp-tn)
   (move rbx base)
   (move rax entry)
   (inst mov r15-tn rbx)
   (inst call rax)
   (inst pop rbp-tn)
   (inst pop rsp-tn)
   (inst pop rsi-tn)
   (inst pop rdi-tn)
   (inst pop r15-tn)
   (inst pop r14-tn)
   (inst pop r13-tn)
   (inst pop r12-tn)
   (inst pop r11-tn)
   (inst pop r10-tn)
   (inst pop r9-tn)
   (inst pop r8-tn)))