(in-package #:forth)

(defun next ()
  (inst lods *rax*)
  (inst jmp (ea 0 *rax*)))

(defun pushrsp (reg)
  (inst lea *rsp* (ea -8 *rsp*))
  (inst mov :qword (ea 0 *rsp*) reg))

(defun poprsp (reg)
  (inst mov :qword reg (ea 0 *rsp*))
  (inst lea *rsp* (ea 8 *rsp*)))

(define-routine docol ()
  (pushrsp *ip*)
  (inst lea :qword *ip* (ea 8 *rax*))
  (next))

(define-routine dovar ()
  (inst lea :qword *rax* (ea 8 *rax*))
  (inst push *rax*)
  (next))

(define-code drop ()
  (inst lea *dsp* (ea 8 *dsp*))
  (next))

(define-code swap ()
  (inst pop *rax*)
  (inst pop *rbx*)
  (inst push *rax*)
  (inst push *rbx*)
  (next))

(define-code dup ()
  (inst mov :qword *rax* (ea 0 *dsp*))
  (inst push *rax*)
  (next))

(define-code over ()
  (inst mov :qword *rax* (ea 8 *dsp*))
  (inst push *rax*)
  (next))

(define-code pick ()
  (inst pop *rcx*)
  (inst mov :qword *rax* (ea 0 *dsp* *rcx* 8))
  (inst push *rax*)
  (next))

(defmacro popn (&rest registers)
  `(progn ,@(mapcar #'(lambda (reg) `(inst pop ,reg)) registers)))

(defmacro pushn (&rest registers)
  `(progn ,@(mapcar #'(lambda (reg) `(inst push ,reg)) registers)))

(define-code rot ()
  (popn *rax* *rbx* *rcx*)
  (pushn *rbx* *rax* *rcx*)
  (next))

(define-code -rot ()
  (popn *rax* *rbx* *rcx*)
  (pushn *rax* *rcx* *rbx*)
  (next))

(define-code 2drop ()
  (popn *rax* *rax*)
  (next))

(define-code 2dup ()
  (inst mov :qword *rax* (ea 0 *dsp*))
  (inst mov :qword *rbx* (ea 8 *dsp*))
  (pushn *rbx* *rax*)
  (next))

(define-code 2swap ()
  (popn *rax* *rbx* *rcx* *rdx*)
  (pushn *rbx* *rax* *rdx* *rcx*)
  (next))

(define-code ?dup ()
  (inst mov :qword *rax* (ea 0 *dsp*))
  (inst test *rax* *rax*)
  (inst jmp :z l0)
  (inst push *rax*)
  l0
  (next))

(define-code 1+ ()
  (inst inc :qword (ea 0 *dsp*))
  (next))

(define-code 1- ()
  (inst dec :qword (ea 0 *dsp*))
  (next))

(define-code 8+ ()
  (inst add :qword (ea 0 *dsp*) 8)
  (next))

(define-code 8- ()
  (inst sub :qword (ea 0 *dsp*) 8)
  (next))

(define-code + ()
  (inst pop *rax*)
  (inst add :qword (ea 0 *dsp*) *rax*)
  (next))

(define-code - ()
  (inst pop *rax*)
  (inst sub :qword (ea 0 *dsp*) *rax*)
  (next))

(define-code * ()
  (popn *rax* *rbx*)
  (inst imul *rax* *rbx*)
  (inst push *rax*)
  (next))

(define-code /mod ()
  (inst xor *rdx* *rdx*)
  (popn *rbx* *rax*)
  (inst cqo)
  (inst idiv *rax* *rbx*)
  (pushn *rdx* *rax*)
  (next))

(define-code = ()
  (popn *rax* *rbx*)
  (inst cmp *rax* *rbx*)
  (inst set *rax* :eq)
  (inst push *rax*)
  (next))

(define-code <> ()
  (popn *rax* *rbx*)
  (inst cmp *rax* *rbx*)
  (inst set *rax* :ne)
  (inst push *rax*)
  (next))

(defun binary-compare (cc)
  (popn *rax* *rbx*)
  (inst cmp *rax* *rbx*)
  (inst set *rax* cc)
  (inst push *rax*))

(defun zero-compare (cc)
  (inst pop *rax*)
  (inst test *rax* *rax*)
  (inst set *rax* cc)
  (inst push *rax*))

(define-code < ()
  (binary-compare :l)
  (next))

(define-code > ()
  (binary-compare :g)
  (next))

(define-code <= ()
  (binary-compare :le)
  (next))

(define-code >= ()
  (binary-compare :ge)
  (next))

(define-code 0= ()
  (zero-compare :z)
  (next))

(define-code 0<> ()
  (zero-compare :nz)
  (next))

(define-code 0< ()
  (zero-compare :l)
  (next))

(define-code 0> ()
  (zero-compare :g)
  (next))

(define-code 0<= ()
  (zero-compare :le)
  (next))

(define-code 0>= ()
  (zero-compare :ge)
  (next))

(define-code and ()
  (inst pop *rax*)
  (inst and (ea 0 *dsp*) *rax*)
  (next))

(define-code or ()
  (inst pop *rax*)
  (inst or (ea 0 *dsp*) *rax*)
  (next))

(define-code xor ()
  (inst pop *rax*)
  (inst xor (ea 0 *dsp*) *rax*)
  (next))

(define-code invert ()
  (inst not :qword (ea 0 *dsp*))
  (next))

(define-code exit ()
  (poprsp *ip*)
  (next))

(define-code lit ()
  (inst lods *rax*)
  (inst push *rax*)
  (next))

(define-code ! ()
  (inst pop *rbx*)
  (inst pop *rax*)
  (inst mov (ea 0 *rbx*) *rax*)
  (next))

(define-code @ ()
  (inst pop *rbx*)
  (inst mov *rax* (ea 0 *rbx*))
  (inst push *rax*)
  (next))

(define-code +! ()
  (inst pop *rbx*)
  (inst pop *rax*)
  (inst add (ea 0 *rbx*) *rax*)
  (next))

(define-code -! ()
  (inst pop *rbx*)
  (inst pop *rax*)
  (inst sub (ea 0 *rbx*) *rax*)
  (next))

(define-code c! ()
  (inst pop *rbx*)
  (inst pop *rax*)
  (inst mov :byte (ea 0 *rbx*) *rax*)
  (next))

(define-code c@ ()
  (inst pop *rbx*)
  (inst xor *rax* *rax*)
  (inst mov :byte *rax* (ea 0 *rbx*))
  (inst push *rax*)
  (next))

(define-code c@c! ()
  (inst mov *rdx* *ip*) ; save ip
  (inst pop *rdi*)      ; destination address
  (inst pop *ip*)       ; source address
  (inst movs :byte)     ; copy to destination
  (inst push *ip*)      ; push incremented source
  (inst push *rdi*)     ; push incremented destination
  (inst mov *ip* *rdx*) ; restore ip
  (next))

(define-code cmove ()
  (inst mov *rdx* *ip*)
  (popn *rcx* *rdi* *ip*)
  (inst rep)
  (inst movs :byte)
  (inst mov *ip* *rdx*)
  (next))

(define-variable state ())
(define-variable here ())
(define-variable latest ())
(define-variable s0 ())
(define-variable base () 10)

(define-code r0 ()
  (inst lea *rax* (ea +return-stack-offset+ *env*))
  (inst push *rax*)
  (next))

(define-constant f_immed () +flag-immediate+)
(define-constant f_hidden () +flag-hidden+)
(define-constant f_lenmask () +length-mask+)

(defconstant +sys-exit+ 1)
(defconstant +sys-read+ 3)
(defconstant +sys-write+ 4)
(defconstant +sys-open+ 5)
(defconstant +sys-close+ 6)

(define-constant sys_exit () +sys-exit+)
(define-constant sys_open () +sys-open+)
(define-constant sys_close () +sys-close+)
(define-constant sys_read () +sys-read+)
(define-constant sys_write () +sys-write+)

(define-constant o_rdonly () sb-posix:o-rdonly)
(define-constant o_wronly () sb-posix:o-wronly)
(define-constant o_rdwr () sb-posix:o-rdwr)
(define-constant o_creat () sb-posix:o-creat)
(define-constant o_excl () sb-posix:o-excl)
(define-constant o_trunc () sb-posix:o-trunc)
(define-constant o_append () sb-posix:o-append)
(define-constant o_nonblock () sb-posix:o-nonblock)

(define-code >r ()
  (inst pop *rax*)
  (pushrsp *rax*)
  (next))

(define-code r> ()
  (poprsp *rax*)
  (inst push *rax*)
  (next))

(define-code rsp@ ()
  (inst push *rsp*)
  (next))

(define-code rsp! ()
  (inst pop *rsp*)
  (next))

(define-code rsp! ()
  (inst pop *rsp*)
  (next))

(define-code rdrop! ()
  (inst add :qword *rsp* 8)
  (next))

(define-code dsp@ ()
  (inst mov *rax* *dsp*)
  (inst push *rax*)
  (next))

(define-code dsp! ()
  (inst pop *dsp*)
  (next))

(defconstant +buffer-size+ 4096)

(define-allotted *buffer* +buffer-size+)
(define-data *currkey* (sb-sys:sap-int (label *buffer*)))
(define-data *bufftop* (sb-sys:sap-int (label *buffer*)))

(define-routine %key
  top
  (inst mov :qword *rbx* (env *currkey*))
  (inst cmp *rbx* (env *bufftop*))
  (inst jmp :ge do-read)
  (inst xor *rax* *rax*)
  (inst mov :byte *rax* (ea 0 *rbx*))
  (inst inc *rbx*)
  (inst mov :qword (env *currkey*) *rbx*)
  (inst ret)

  do-read
  ;;  out of input, use read(2)
  (inst push *rdi*)
  (inst push *rsi*)                      ; save instruction pointer
  (inst xor *rdi* *rdi*)                 ; first param: stdin
  (inst lea :qword *rsi* (env *buffer*)) ; second param: buffer
  (inst mov :qword (env *currkey*) *rsi*)
  (inst mov :qword *rdx* +buffer-size+)  ; third param: buffer length
  (inst mov :qword *rax* +sys-read+)     ; read syscall
  (syscall)
  (inst test *rax* *rax*)
  (inst jmp :be exit)
  (inst add *rsi* *rax*)
  (inst mov (env *bufftop*) *rsi*)
  (inst pop *rsi*)
  (inst pop *rdi*)
  (inst jmp top)

  exit ; end of input
  (inst xor *rdi* *rdi*)
  (inst mov :qword *rax* +sys-exit+)
  (syscall))

(define-code key ()
  (call %key)
  (inst push *rax*)
  (next))

(define-allotted *emit-scratch* 1)

(define-routine %emit
  (inst push *ip*)
  (inst mov :qword *rdi* 1)
  (inst lea :qword *rsi* (env *emit-scratch*))
  (inst mov :byte (env *emit-scratch*) *rax*)
  (inst mov :qword *rdx* 1)
  (inst mov :qword *rax* +sys-write+)
  (syscall)
  (inst pop *ip*)
  (inst ret))

(define-code emit ()
  (inst pop *rax*)
  (call %emit)
  (next))

(define-allotted *word-buffer* 32)

(define-routine %word
  top
  (call %key)
  (inst cmp :byte *rax* (char-code #\\))
  (inst jmp :eq consume-line)
  (inst cmp :byte *rax* (char-code #\Space))
  (inst jmp :be top)

  (inst lea *rdi* (env *word-buffer*))

  consume-whitespace
  (stosb *rax*)
  (call %key)
  (inst cmp :byte *rax* (char-code #\Space))
  (inst jmp :a consume-whitespace)

  (inst lea *rcx* (env *word-buffer*))
  (inst sub *rdi* *rcx*)
  (inst mov *rcx* *rdi*)
  (inst lea *rdi* (env *word-buffer*))
  (inst ret)

  consume-line
  (call %key)
  (inst cmp :byte *rax* (char-code #\Newline))
  (inst jmp :ne consume-line)
  (inst jmp top))

(define-code word ()
  (call %word)
  (inst push *rdi*)
  (inst push *rcx*)
  (next))

(define-routine %number
  (inst xor *rax* *rax*)
  (inst xor *rbx* *rbx*)
  (inst test *rcx* *rcx*)
  (inst jmp :z five)

  (inst mov :qword *rdx* (env var-base))
  (inst mov :byte *rbx* (ea 0 *rdi*))
  (inst inc *rdi*)
  (inst push *rax*)
  (inst cmp *rbx* (char-code #\-))
  (inst jmp :nz two)
  (inst pop *rax*)
  (inst push *rbx*) ; push <> 0 on stack, indicating negative
  (inst dec *rcx*)
  (inst jmp :nz one)
  (inst pop *rbx*)
  (inst mov :qword *rcx* 1)
  (inst ret)

  one
  (inst imul *rax* *rdx*)
  (inst mov :byte *rbx* (ea 0 *rdi*))
  (inst inc *rdi*)

  two
  (inst sub :byte *rbx* (char-code #\0)) ; < '0'?
  (inst jmp :b four)
  (inst cmp *rbx* 10)                    ; <= '9'?
  (inst jmp :b three)
  (inst sub :byte *rbx* 17)              ;
  (inst jmp :b four)
  (inst add :byte *rbx* 10)

  three
  (inst cmp :byte *rbx* *rdx*)
  (inst jmp :ge four)

  (inst add *rax* *rbx*)
  (inst dec *rcx*)
  (inst jmp :nz one)

  four
  (inst pop *rbx*)
  (inst test *rbx* *rbx*)
  (inst jmp :z five)
  (inst neg *rax*)

  five
  (inst ret))

(define-code number ()
  (inst pop *rcx*)
  (inst pop *rdi*)
  (call %number)
  (inst push *rax*)
  (inst push *rcx*)
  (next))

(define-routine %find
  (inst push *ip*)
  (inst mov *rdx* (env var-latest))

  one
  (inst test *rdx* *rdx*)
  (inst jmp :e four)

  ;; compare the length expected and the length of the word
  (inst xor *rax* *rax*)
  (inst mov :byte *rax* (ea 8 *rdx*)) ; al = flags+length field
  (inst and :byte *rax* (logior +flag-hidden+ +length-mask+))
  (inst cmp :byte *rax* *rcx*)
  (inst jmp :ne two)

  ;; compare strings in detail
  (inst push *rcx*)              ; save the length
  (inst push *rdi*)              ; save the address
  (inst lea *rsi* (ea 9 *rdx*))  ; dictionary string we are checking against
  (inst repe)
  (inst cmps :byte)              ; compare the strings
  (inst pop *rdi*)
  (inst pop *rcx*)
  (inst jmp :ne two)             ; not the same

  ;; the strings are the same - return the header in *rax*
  (inst pop *rsi*)
  (inst mov *rax* *rdx*)
  (inst ret)

  two
  (inst mov *rdx* (ea 0 *rdx*)) ; move back through the link field to previous word
  (inst jmp one)                ; ... and loop

  four ; not found
  (inst pop *rsi*)
  (inst xor *rax* *rax*)
  (inst ret))

(define-code find ()
  (inst pop *rcx*)
  (inst pop *rdi*)
  (call %find)
  (inst push *rax*)
  (next))

(define-routine %>cfa ()
  (inst xor *rax* *rax*)
  (inst add :qword *rdi* 8)
  (inst mov :byte *rax* (ea 0 *rdi*))
  (inst inc *rdi*)
  (inst and :byte *rax* +length-mask+)
  (inst add :qword *rdi* *rax*)
  (inst add :qword *rdi* 7)
  (inst and :qword *rdi* (lognot 7))
  (inst ret))

(define-code >cfa ()
  (inst pop *rdi*)
  (call %>cfa)
  (inst push *rdi*)
  (next))

(define-word >dfa () >cfa 8+ exit)

(define-code create ()
  (inst pop *rcx*) ; length
  (inst pop *rbx*) ; address of name

  ;; link pointer
  (inst mov :qword *rdi* (env var-here))
  (inst mov :qword *rax* (env var-latest))
  (inst stos *rax*)

  ;; length byte and name
  (inst movzx '(:byte :qword) *rax* *rcx*)   ; get the length
  (stosb *rax*)                              ; store the length/flags byte
  (inst push *ip*)
  (inst mov *rsi* *rbx*)
  (inst rep)
  (inst movs :byte)
  (inst pop *ip*)
  (inst add *rdi* 7)
  (inst and *rdi* (lognot 7))

  (inst mov :qword *rax* (env var-here))
  (inst mov :qword (env var-latest) *rax*)
  (inst mov :qword (env var-here) *rdi*)
  (next))

(define-routine |%,| ()
  (inst mov :qword *rdi* (env var-here))
  (inst stos *rax*)
  (inst mov :qword (env var-here) *rdi*)
  (inst ret))

(define-code |,| ()
  (inst pop *rax*)
  (call |%,|)
  (next))

(define-code [ (:immediate)
  (inst xor *rax* *rax*)
  (inst mov :qword (env var-state) *rax*)
  (next))

(define-code ] ()
  (inst mov :qword (env var-state) 1)
  (next))

(define-code hidden ()
  (inst pop *rdi*)
  (inst xor :byte (ea 8 *rdi*) +flag-hidden+)
  (next))

(define-word |:| ()
  word
  create
  lit docol |,|
  latest @ hidden
  ]
  exit)

(define-word |;| (:immediate)
  lit exit |,|
  latest @ hidden
  [
  exit)

(define-code immediate (:immediate)
  (inst mov *rdi* (env var-latest))
  (inst xor :byte (ea 8 *rdi*) +flag-immediate+)
  (next))

(define-word hide () word find hidden exit)

(define-code |'| ()
  (inst lods *rax*)
  (inst push *rax*)
  (next))

(define-code branch ()
  (inst add *ip* (ea 0 *ip*))
  (next))

(define-code 0branch ()
  (inst pop *rax*)
  (inst test *rax* *rax*)
  (inst jmp :z branch)
  (inst lods *rax*)
  (next)

  branch
  (inst add *ip* (ea 0 *ip*))
  (next))

(define-code litstring ()
  (inst lods *rax*)
  (inst push *rsi*)
  (inst push *rax*)
  (inst add :qword *rsi* *rax*)
  (inst add :qword *rsi* 7)
  (inst and :qword *rsi* (lognot 7))
  (next))

(define-code tell ()
  (inst mov *rdi* 1)
  (inst pop *rdx*)
  (inst mov *rbx* *ip*)
  (inst pop *rsi*)
  (inst push *rbx*)
  (inst mov :qword *rax* +sys-write+)
  (syscall)
  (inst pop *ip*)
  (next))

(define-string *error-message* "PARSE ERROR: ")
(label *error-message-end*)

(define-string *error-message-newline* (format nil "~%"))
(align)

(define-data *interpret-is-lit* 0)

(define-code interpret ()
  (call %word)

  (inst xor *rax* *rax*)
  (inst mov (env *interpret-is-lit*) *rax*)
  (call %find)
  (inst test *rax* *rax*)
  (inst jmp :z one)

  (inst mov *rdi* *rax*)
  (inst xor *rax* *rax*)
  (inst mov :byte *rax* (ea 8 *rdi*))
  (inst push *rax*)
  (call %>cfa)
  (inst pop *rax*)
  (inst and :byte *rax* +flag-immediate+)
  (inst mov *rax* *rdi*)
  (inst jmp :nz four)
  (inst jmp two)

  one
  (inst inc :qword (env *interpret-is-lit*))
  (call %number)
  (inst test *rcx* *rcx*)
  (inst jmp :nz six)
  (inst mov *rbx* *rax*)
  (inst lea *rax* (env lit))

  two
  (inst mov :qword *rdx* (env var-state))
  (inst test *rdx* *rdx*)
  (inst jmp :z four)

  (call |%,|)
  (inst mov :qword *rcx* (env *interpret-is-lit*))
  (inst test *rcx* *rcx*)
  (inst jmp :z three)
  (inst mov *rax* *rbx*)
  (call |%,|)
  three
  (next)

  four
  (inst mov :qword *rcx* (env *interpret-is-lit*))
  (inst test *rcx* *rcx*)
  (inst jmp :nz five)

  (inst jmp (ea 0 *rax*))

  five
  (inst push *rbx*)
  (next)

  six
  (inst push *ip*)
  (inst mov :qword *rdi* 2)
  (inst lea *rsi* (env *error-message*))
  (inst mov :qword *rdx* (sb-sys:sap- (label *error-message-end*)
                                      (label *error-message*)))
  (inst mov :qword *rax* +sys-write+)
  (syscall)

  (inst mov :qword *rsi* (env *currkey*))
  (inst lea *rdx* (env *buffer*))
  (inst mov *rax* *rsi*)
  (inst sub :qword *rax* *rdx*)
  (inst mov :qword *rdx* *rax*)
  (inst cmp :qword *rdx* 40)
  (inst jmp :le seven)
  (inst mov :qword *rdx* 40)
  seven
  (inst mov :qword *rax* +sys-write+)
  (syscall)

  (inst lea *rsi* (env *error-message-newline*))
  (inst mov :qword *rdx* 1)
  (inst mov *rax* +sys-write+)
  (syscall)

  (inst pop *ip*)
  (next))

(define-word quit () r0 >r interpret branch -16)

(define-code char ()
  (call %word)
  (inst xor *rax* *rax*)
  (inst mov :byte *rax* (ea 0 *rdi*))
  (inst push *rax*)
  (next))

(define-code execute ()
  (inst pop *rax*)
  (inst jmp (ea 0 *rax*)))

(define-code syscall3 ()
  (inst mov *rbx* *rsi*)
  (popn *rax* *rdi* *rsi* *rdx*)
  (inst push *rbx*)
  (syscall)
  (inst pop *rsi*)
  (inst push *rax*)
  (next))

(define-code syscall2 ()
  (inst mov *rbx* *rsi*)
  (popn *rax* *rdi* *rsi*)
  (inst push *rbx*)
  (syscall)
  (inst pop *rsi*)
  (inst push *rax*)
  (next))

(define-code syscall1 ()
  (inst mov *rbx* *rsi*)
  (popn *rax* *rsi*)
  (inst push *rbx*)
  (syscall)
  (inst pop *rsi*)
  (inst push *rax*)
  (next))

(define-code syscall0 ()
  (inst pop *rax*)
  (syscall)
  (inst push *rax*)
  (next))

(define-code bye ()
  (inst mov *dsp* *old*)
  (inst pop *rsp*)
  (inst ret))

(label %boot)
(quad (get-label 'quit))

(define-routine warm
  (inst push *rsp*)
  (inst mov *old* *dsp*)
  (inst cld)
  (inst lea *rsp* (ea +return-stack-offset+ *env*))

  (inst lea :qword *rax* (env *buffer*))
  (inst mov :qword (env *bufftop*) *rax*)
  (inst mov :qword (env *currkey*) *rax*)

  (inst lea :qword *ip* (env %boot))
  (next))

(define-routine cold
  (inst push *rsp*)
  (inst mov *old* *dsp*)

  ;; initialize variables
  (inst mov :qword (env var-s0) *dsp*)

  (inst lea :qword *rax* (env bye-name))
  (inst mov :qword (env var-latest) *rax*)

  (inst lea :qword *rax* (env *here*))
  (inst add :qword *rax* #x70)
  (inst mov :qword (env var-here) *rax*)

  (inst lea :qword *rax* (env *buffer*))
  (inst mov :qword (env *bufftop*) *rax*)
  (inst mov :qword (env *currkey*) *rax*)

  (inst cld)
  (inst lea *rsp* (ea +return-stack-offset+ *env*))
  (inst lea :qword *ip* (env %boot))
  (next))
