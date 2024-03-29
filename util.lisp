(in-package #:forth)

(defun hexdump (pointer &optional (length 64) (stream t))
  (flet ((printable-char-p (code) (< #x20 code #x7f)))
    (macrolet ((do-bytes ((byte row &key (limit 16) (add-space t)) &body body)
                 `(dotimes (i ,limit)
                    (let* ((offset (+ i (* ,row 16)))
                           (,byte (sb-sys:sap-ref-8 pointer offset)))
                      ,@body
                      (when (and ,add-space (= i 7))
                        (write-char #\Space stream))))))
      (multiple-value-bind (nrows remaining)
          (floor length 16)
        (dotimes (row nrows)
          (format stream "~2,'0X: " (sb-sys:sap-int (sb-sys:sap+ pointer (* row 16))))
          (do-bytes (b row)
            (format stream "~2,'0X " b))
          (write-char #\| stream)
          (do-bytes (b row :add-space nil)
            (if (printable-char-p b)
                (write-char (code-char b) stream)
                (write-char #\Period stream)))
          (write-char #\| stream)
          (terpri stream))

        (dotimes (i remaining)
          (let* ((offset (+ i (* nrows 16)))
                 (byte (sb-sys:sap-ref-8 pointer offset)))
            (format stream "~2,'0X " byte)
            (when (= i 7)
            (write-char #\Space stream))))

        (dotimes (i (- 16 remaining))
          (write-char #\Space stream))

        (dotimes (i remaining)
          (let* ((offset (+ i (* nrows 16)))
                 (byte (sb-sys:sap-ref-8 pointer offset)))
            (if (printable-char-p byte)
                (write-char (code-char byte) stream)
                (write-char #\Period stream))))))))

(defun disassemble (word &optional (length 32))
  "Disassebmle the word named WORD-NAME."
  (sb-disassem:disassemble-memory (get-label word) length))
