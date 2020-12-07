(in-package #:forth)

(defun %go-forth (entry-point base-address)
  (declare (type system-area-pointer entry-point base-address))
  (%go-forth entry-point base-address))

(defun go-forth (&key warm-boot)
  (%go-forth (if warm-boot warm (get-label 'cold)) *environment*))
