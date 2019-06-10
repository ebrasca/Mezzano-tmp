;;;; copyright (c) 2019 bruno cichon <ebrasca@librepanther.com>
;;;; this code is licensed under the mit license.
;;;; Documentation for PPC64LE CPU architecture : PowerISA_public.v3.0B.pdf

(in-package :mezzano.lap.ppc64le)

(defparameter *instruction-assemblers* (make-hash-table))

;; (defmethod sys.lap:perform-assembly-using-target ((target sys.c:arm64-target) code-list &rest args &key &allow-other-keys)
;;   (apply 'sys.lap:perform-assembly *instruction-assemblers* code-list args))

(defun add-instruction (name function)
  (unless (keywordp name)
    (export name :mezzano.lap.ppc64le))
  (setf (gethash name *instruction-assemblers*) function)
  name)

(defmacro define-instruction (name lambda-list &body body)
  (let ((fname (intern (format nil "~s-assembler" name))))
    `(progn
       (defun ,fname ,lambda-list
         ,@body)
       (add-instruction ',name ',fname))))

(defun emit-instruction (value)
  (check-type value (unsigned-byte 32))
  (format t "~b~%" value))

(defun b-form (bo bi bd aa lk)
  (emit-instruction
   (logior (ash 16 26)
           (ash bo 21)
           (ash bi 16)
           (ash bd 2)
           (ash aa 1)
           lk)))

(defun d-form (po a ra b)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash ra 16)
           b)))

(defun dq-form (po a ra dq b)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash ra 16)
           (ash dq 4)
           b)))

(defun ds-form (po a ra ds xo)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash ra 16)
           (ash ds 2)
           xo)))

(defun dx-form (rt d1 d0 xo d2)
  (emit-instruction
   (logior (ash 19 26)
           (ash rt 21)
           (ash d1 16)
           (ash d0 6)
           (ash xo 1)
           d2)))

(defun l-form (li aa lk)
  (emit-instruction
   (logior (ash 18 26)
           (ash li 2)
           (ash aa 1)
           lk)))

(defun sc-form (lev a b)
  (emit-instruction
   (logior (ash 17 26)
           (ash lev 5)
           (ash a 1)
           b)))

(defun x-form (po a b c xo &optional (d 0))
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           (ash xo 1)
           d)))

(defun xl-form (a b c xo &optional (lk 0))
  (emit-instruction
   (logior (ash 19 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           (ash xo 1)
           lk)))

(defun xo-form (po rt ra rb oe xo rc)
  (emit-instruction
   (logior (ash po 26)
           (ash rt 21)
           (ash ra 16)
           (ash rb 11)
           (ash oe 10)
           (ash xo 1)
           rc)))

;;; chapter 2. branch facility
;; branch
(define-instruction b (li)
  (l-form li 0 0))

(define-instruction ba (li)
  (l-form li 1 0))

(define-instruction bl (li)
  (l-form li 0 1))

(define-instruction bla (li)
  (l-form li 1 1))

;; branch conditional
(define-instruction bc (bo bi bd)
  (b-form bo bi bd 0 0))

(define-instruction bca (bo bi bd)
  (b-form bo bi bd 1 0))

(define-instruction bcl (bo bi bd)
  (b-form bo bi bd 0 1))

(define-instruction bcla (bo bi bd)
  (b-form bo bi bd 1 1))

;; branch conditional to link register
(define-instruction bclr (bo bi bh)
  (xl-form bo bi bh 16))

(define-instruction bclrl (bo bi bh)
  (xl-form bo bi bh 16 1))

;; branch conditional to count register
(define-instruction bcctr (bo bi bh)
  (xl-form bo bi bh 528))

(define-instruction bcctrl (bo bi bh)
  (xl-form bo bi bh 528 1))

;; branch conditional to branch target address register
(define-instruction bctar (bo bi bh)
  (xl-form bo bi bh 560))

(define-instruction bctarl (bo bi bh)
  (xl-form bo bi bh 560 1))

;; condition register and
(define-instruction crand (bt ba bb)
  (xl-form bt ba bb 257))

;; condition register or
(define-instruction cror (bt ba bb)
  (xl-form bt ba bb 449))

;; condition register nand
(define-instruction crnand (bt ba bb)
  (xl-form bt ba bb 225))

;; condition register xor
(define-instruction crxor (bt ba bb)
  (xl-form bt ba bb 193))

;; condition register nor
(define-instruction crnor (bt ba bb)
  (xl-form bt ba bb 33))

;; condition register and with complement
(define-instruction crandc (bt ba bb)
  (xl-form bt ba bb 129))

;; condition register equivalent
(define-instruction creqv (bt ba bb)
  (xl-form bt ba bb 289))

;; condition register or with complement
(define-instruction crorc (bt ba bb)
  (xl-form bt ba bb 417))

;; move condition register field
(define-instruction mcrf (bf bfa)
  (xl-form (ash bf 2) (ash bfa 2) 0 0))

;; system call
(define-instruction sc (lev)
  (sc-form lev 1 0))

;; system call vectored
(define-instruction scv (lev)
  (sc-form lev 0 1))

;;; chapter 3. fixed-point facility
;; load byte and zero
(define-instruction lbz (rt ra d)
  (d-form 34 rt ra d))

;; load byte and zero with update
(define-instruction lbzu (rt ra d)
  (d-form 35 rt ra d))

;; load byte and zero indexed
(define-instruction lbzx (rt ra rb)
  (x-form 31 rt ra rb 87))

;; load byte and zero with update indexed
(define-instruction lbzux (rt ra rb)
  (x-form 31 rt ra rb 119))

;; load halfword and zero
(define-instruction lhz  (rt ra d)
  (d-form 40 rt ra d))

;; load halfword and zero with update
(define-instruction lhzu (rt ra d)
  (d-form 41 rt ra d))

;; load halfword and zero indexed
(define-instruction lhzx (rt ra rb)
  (x-form 31 rt ra rb 279))

;; load halfword and zero with update indexed
(define-instruction lhzux (rt ra rb)
  (x-form 31 rt ra rb 311))

;; load halfword algebraic
(define-instruction lha (rt ra d)
  (d-form 42 rt ra d))

;; load halfword algebraic with update
(define-instruction lhau (rt ra d)
  (d-form 43 rt ra d))

;; load halfword algebraic indexed
(define-instruction lhax (rt ra rb)
  (x-form 31 rt ra rb 343))

;; load halfword algebraic with update indexed
(define-instruction lhaux (rt ra rb)
  (x-form 31 rt ra rb 375))

;; load word and zero
(define-instruction lwz (rt ra d)
  (d-form 32 rt ra d))

;; load word and zero with update
(define-instruction lwzu (rt ra d)
  (d-form 33 rt ra d))

;; load word and zero indexed
(define-instruction lwzx (rt ra rb)
  (x-form 31 rt ra rb 23))

;; load word and zero with update indexed
(define-instruction lwzux (rt ra rb)
  (x-form 31 rt ra rb 55))

;; load word algebraic
(define-instruction lwa (rt ra ds)
  (ds-form 58 rt ra ds 2))

;; load word algebraic indexed
(define-instruction lwax (rt ra rb)
  (x-form 31 rt ra rb 341))

;; load word algebraic with update indexed
(define-instruction lwaux (rt ra rb)
  (x-form 31 rt ra rb 373))

;; load doubleword
(define-instruction ld (rt ra ds)
  (ds-form 58 rt ra ds 0))

;; load doubleword with update
(define-instruction ldu (rt ra ds)
  (ds-form 58 rt ra ds 1))

;; load doubleword indexed
(define-instruction ldx (rt ra rb)
  (x-form 31 rt ra rb 21))

;; load doubleword with update indexed
(define-instruction ldux (rt ra rb)
  (x-form 31 rt ra rb 53))

;; store byte
(define-instruction stb (rs ra d)
  (ds-form 38 rs ra d))

;; store byte with update
(define-instruction stbu (rs ra d)
  (ds-form 39 rs ra d))

;; store byte indexed
(define-instruction stbx (rs ra rb)
  (x-form 31 rs ra rb 215))

;; store byte with update indexed
(define-instruction stbux (rs ra rb)
  (x-form 31 rs ra rb 247))

;; store halfword
(define-instruction sth (rs ra d)
  (d-form 44 rs ra d))

;; store halfword with update
(define-instruction sthu (rs ra d)
  (d-form 45 rs ra d))

;; store halfword indexed
(define-instruction sthx (rs ra rb)
  (x-form 31 rs ra rb 407))

;; store halfword with update indexed
(define-instruction sthux (rs ra rb)
  (x-form 31 rs ra rb 439))

;; store word
(define-instruction stw (rs ra d)
  (d-form 36 rs ra d))

;; store word with update
(define-instruction stwu (rs ra d)
  (d-form 37 rs ra d))

;; store word indexed
(define-instruction stwx (rs ra rb)
  (x-form 31 rs ra rb 151))

;; store word with update indexed
(define-instruction stwux (rs ra rb)
  (x-form 31 rs ra rb 183))

;; store doubleword
(define-instruction std (rs ra ds)
  (ds-form 62 rs ra ds 0))

;; store doubleword with update
(define-instruction stdu (rs ra ds)
  (ds-form 62 rs ra ds 1))

;; store doubleword indexed
(define-instruction stdx (rs ra rb)
  (x-form 31 rs ra rb 149))

;; store doubleword with update indexed
(define-instruction stdux (rs ra rb)
  (x-form 31 rs ra rb 181))

;; load quadword
(define-instruction lq (rtp ra dq)
  (dq-form 56 rtp ra dq 0))

;; store quadword
(define-instruction stq (rsp ra ds)
  (ds-form 62 rsp ra ds 2))

;; load halfword byte-reverse indexed
(define-instruction lhbrx (rt ra rb)
  (x-form 31 rt ra rb 790))

;; load word byte-reverse indexed
(define-instruction lwbrx (rt ra rb)
  (x-form 31 rt ra rb 534))

;; store halfword byte-reverse indexed
(define-instruction sthbrx (rs ra rb)
  (x-form 31 rs ra rb 918))

;; store word byte-reverse indexed
(define-instruction stwbrx (rs ra rb)
  (x-form 31 rs ra rb 662))

;; load doubleword byte-reverse indexed
(define-instruction ldbrx (rt ra rb)
  (x-form 31 rt ra rb 532))

;; store doubleword byte-reverse indexed
(define-instruction stdbrx (rs ra rb)
  (x-form 31 rs ra rb 660))

;; load multiple word
(define-instruction lmw (rt ra d)
  (d-form 46 rt ra d))

;; store multiple word
(define-instruction stmw (rs ra d)
  (d-form 47 rs ra d))

;; load string word immediate
(define-instruction lswi (rt ra nb)
  (x-form 31 rt ra nb 597))

;; load string word indexed
(define-instruction lswx (rt ra rb)
  (x-form 31 rt ra rb 533))

;; store string word immediate
(define-instruction stswi ()
  (x-form 31 rs ra nb 725))

;; store string word indexed
(define-instruction stswx ()
  (x-form 31 rs ra rb 661))

;; add immediate
(define-instruction addi (rt ra si)
  (d-form 14 rt ra si))

;; add immediate shifted
(define-instruction addis (rt ra si)
  (d-form 15 rt ra si))

;; add pc immediate shifted
(define-instruction addpcis (rt d1 d0 d2)
  (dx-form rt d1 d0 2 d2))

;; add
(define-instruction add (rt ra rb)
  (xo-form 31 rt ra rb 0 266 0))

(define-instruction add. (rt ra rb)
  (xo-form 31 rt ra rb 0 266 1))

(define-instruction addo (rt ra rb)
  (xo-form 31 rt ra rb 1 266 0))

(define-instruction addo. (rt ra rb)
  (xo-form 31 rt ra rb 1 266 1))

;; add immediate carrying
(define-instruction addic (rt ra si)
  (d-form 12 rt ra si))

;; subtract from
(define-instruction subf (rt ra rb)
  (xo-form 31 rt ra rb 0 40 0))

(define-instruction subf. (rt ra rb)
  (xo-form 31 rt ra rb 0 40 1))

(define-instruction subfo (rt ra rb)
  (xo-form 31 rt ra rb 1 40 0))

(define-instruction subfo. (rt ra rb)
  (xo-form 31 rt ra rb 1 40 1))

;; add immediate carrying and record
(define-instruction addic. ()
  (d-form 13 rt ra si))

;;; TODO all instrusctions after page 69
