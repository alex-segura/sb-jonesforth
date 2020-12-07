;; With inspiration from Richard W.M. Jones and Paul Khuong

(in-package #:forth)

(defconstant +environment-size+ (* 1024 1024) ; 1MB
  "Size (in bytes) of the embedded forth environment.")

(defconstant +return-stack-offset+ +environment-size+
  "Start offset of the forth return stack.")

(defconstant +dictionary-offset+ 0
  "Offset of the first entry in the forth dictionary.")

(defconstant +flag-immediate+ #x80
  "Flag for 'immediate' words.")

(defconstant +flag-hidden+ #x20
  "Flag for 'hidden' words.")

(defconstant +length-mask+ #x1f
  "Mask for string length.")

(defparameter *flags*
  `((:hidden . ,+flag-hidden+)
    (:immediate . ,+flag-immediate+))
  "Table of defined flags and their corresponding bit fields.")

(defvar *rax* rax-tn)
(defvar *rbx* rbx-tn)
(defvar *rcx* rcx-tn)
(defvar *rdx* rdx-tn)
(defvar *rdi* rdi-tn)
(defvar *rsi* rsi-tn)
(defvar *r14* r14-tn)
(defvar *r15* r15-tn)

(defvar *ip* *rsi*
  "Register used for the forth Instruction Pointer (IP).")

(defvar *dsp* rsp-tn
  "Register used for the forth Data Stack Pointer (DSP).")

(defvar *rsp* rbp-tn
  "Register used for the forth Return Stack Pointer (RSP).")

(defvar *old* r13-tn "Saved stack pointer.")

(defvar *env* *r15*
  "Register allocated for a pointer to the base of the forth environment.")

(defvar *environment*
  (sb-posix:mmap nil +environment-size+
                 (logior sb-posix:prot-read sb-posix:prot-write sb-posix:prot-exec)
                 (logior sb-posix:map-anon sb-posix:map-private)
                 -1 0)
  "Mapped address space for the forth environment.")

(define-symbol-macro *dictionary-start* (sb-sys:sap+ *environment* +dictionary-offset+))

(defvar *here* *dictionary-start*
  "Address of next dictionary entry.")

(defvar *link* (sb-sys:int-sap 0)
  "Current address of the dictionary dictionary entry.")

(defvar *segment* nil
  "The current assembler segment.")

(defun reset ()
  "Reset the forth environment it's initial state."
  (setf *here* *dictionary-start* *link* (sb-sys:int-sap 0))
  (dotimes (i +environment-size+)
    (setf (sb-sys:sap-ref-8 *environment* i) 0)))

(defun add-label (name)
  (setf (get name 'label) *here*))

(defun get-label (name) (get name 'label))

(defmacro label (name) `(add-label ',name))

(defun byte (byte)
  "Emit the 8-bit byte BYTE."
  (setf (sb-sys:sap-ref-8 *here* 0) byte
        *here* (sb-sys:sap+ *here* 1)))

(defun quad (quad)
  "Emit an 8-byte quad word QUAD."
  (setf (sb-sys:signed-sap-ref-64 *here* 0) quad
        *here* (sb-sys:sap+ *here* 8)))

(defun bytes (bytes &aux (length (length bytes)))
  "Emit BYTES, with optional LIMIT."
  (prog1 (sb-kernel:copy-ub8-to-system-area bytes 0 *here* 0 length)
    (setf *here* (sb-sys:sap+ *here* length))))

(defun align ()
  "Align *HERE* to a 8-byte boundary."
  (setf *here* (sb-sys:sap+ *here* 7)
        *here* (sb-sys:int-sap (logand (sb-sys:sap-int *here*) (lognot 7)))))

(defun ascii (string)
  (bytes (sb-ext:string-to-octets string)))

(defun asciiz (string)
  (bytes (sb-ext:string-to-octets string))
  (byte #x00))

(defun allot (size)
  "Return the current value of *HERE*, then increment it SIZE bytes (aligned to 8-byte boundary)."
  (prog1 *here*
    (setf *here* (sb-sys:sap+ *here* size))
    (align)))

(defmacro call (name)
  `(progn
     (inst lea :qword *r14* (env ,name))
     (inst call *r14*)))

;; Fake a STOSB,  SBCL assembler doesn't know how to emit this and I haven't figured out
;; how to and instructions to it yet.
(defun stosb (reg)
  (inst mov :byte (ea 0 *rdi*) reg)
  (inst inc *rdi*))

#+openbsd
(progn
  ;; HACK: OpenBSD does not allow syscalls W/X pages or from anything besides
  ;; program text, libc.so, and ld.so
  ;;
  ;; So, let's satify that, shall we?
  ;; There's a syscall; ret gadget 8 bytes away from gretthrid in libc.
  ;; OpenBSD seems to randomize the libc layout, this should work across
  ;; reboots.
  (defparameter *getthrid*
    (load-time-value (sb-sys:find-foreign-symbol-address "getthrid")))
  (defparameter *syscall-gadget* (+ *getthrid* 8))
  (defun syscall ()
    (inst mov :qword *r14* *syscall-gadget*)
    (inst call *r14*)))

#-openbsd
(defun syscall () (inst syscall))

(defun call-with-assembler (emitter)
  "Call EMITTER within an assembler context, returning the assembled bytes."
  (let* ((asmstream (sb-assem:make-asmstream))
         (sb-assem:*asmstream* asmstream)
         (*segment* (sb-assem:make-segment)))
    (sb-assem:assemble (:code)
      (funcall emitter))
    (sb-assem:segment-contents-as-vector
     (sb-assem:assemble-sections asmstream nil *segment*))))

(defmacro assemble (&body body)
  "Assemble BODY within an assembler context, returning the assembled bytes."
  `(call-with-assembler (lambda () ,@body)))

(defun compute-flags (flags)
  "Compute a flag value given a list of FLAGS, as keywords."
  (flet ((accumulate-flag (value flag)
           (let ((v (cdr (assoc flag *flags*))))
             (unless v
               (error "No such flag: ~A" flag))
             (logior value v))))
    (reduce #'accumulate-flag flags :initial-value 0)))

(defun header (name flags)
  (let ((entry-name (intern (concatenate 'string (symbol-name name) "-NAME")))
        (name-string (symbol-name name)))
    (add-label entry-name)
    (quad (sb-sys:sap-int *link*))
    (setf *link* (get-label entry-name))
    (byte (logior (compute-flags flags)
                  (logand +length-mask+ (length name-string))))
    (ascii name-string)
    (align)
    (add-label name)))

(defmacro define-code (name (&rest flags) &body body)
  "Define the word named NAME. with a CODE field set to BODY. BODY is a sequence of assembly instructions."
  `(prog1 *here*
     (header ',name ',flags)
     (quad (sb-sys:sap-int (sb-sys:sap+ *here* 8)))
     (label ,(intern (concatenate 'string (symbol-name name) "-CODE")))
     (bytes (assemble
                (macrolet ((label (n) `(get-label ',n)))
                  (sb-assem:assemble ()
                    ,@body))))
     (align)))

(defmacro define-word (name (&rest flags) &body body)
  "Define the word named NAME, with a CODE field set to ENTER. BODY should be a list of previously defined words."
  (flet ((generate-body (form)
           `(quad
             ,(typecase form
                (symbol `(if (get-label ',form)
                             (sb-sys:sap-int (get-label ',form))
                             (symbol-value ',form)))
                (number form)))))
    `(prog1 *here*
       (header ',name ',flags)
       (quad (sb-sys:sap-int (get 'docol 'label)))
       ,@(mapcar #'generate-body body)
       (align))))

(defmacro define-constant (name (&rest flags) value)
  "Define NAME as a constant word returning VALUE."
  `(define-code ,name ,flags (inst push ,value) (next)))

(defmacro define-variable (name (&rest flags) &optional (initial-value 0))
  "Define NAME as a variable, with value VALUE."
  `(prog1 *here*
     (header ',name ',flags)
     (quad (sb-sys:sap-int (get-label 'dovar)))
     (label ,(intern (concatenate 'string "VAR-" (symbol-name name))))
     (quad ,initial-value)))

(defmacro define-allotted (name size)
  "Allocate SIZE bytes from the forth dictionary, as if from ALLOT. Define NAME (in the lisp environment) as the address of the start of that space. SIZE is rounded to the nearest word."
  `(prog1 *here*
     (label ,name)
     (allot ,size)))

(defmacro define-routine (name &body body)
  "Define NAME as the address of the assembled BODY, alloted from *HERE*."
  `(prog1 *here*
     (label ,name)
     (bytes (assemble
                (macrolet ((label (n) `(get-label ',n)))
                  (sb-assem:assemble () ,@body))))
     (align)))

(defmacro define-data (name data)
  "Define NAME as the address of allotted DATA."
  `(prog1 *here*
     (label ,name)
     (quad (macrolet ((label (n) `(get-label ',n))) ,data))))

(defmacro define-string (name string)
  "Define NAME as the start address of the string STRING."
  `(prog1 *here* (label ,name) (ascii ,string)))

(defmacro env (reference)
  "Create an effective address for REFERENCE, built as a displacement from *ENV*."
  `(ea (sb-sys:sap-
        ,(cond ((symbolp reference)
                `(or (get-label ',reference)
                     (symbol-value ',reference)))
               ((typep reference 'system-area-pointer)
                reference)
               (t (error "Undefined reference ~A:" reference)))
        *environment*)
       *env*))

(sb-c:defknown %go-forth (system-area-pointer system-area-pointer) (values)
    (sb-c:any) :overwrite-fndb-silently t)
