;;;; copyright (c) 2019 bruno cichon <ebrasca@librepanther.com>
;;;; this code is licensed under the mit license.
;;;; Documentation for PPC64LE CPU architecture : PowerISA_public.v3.0B.pdf

(in-package :mezzano.lap.ppc64le)

(defparameter *instruction-assemblers* (make-hash-table))

(defmethod sys.lap:perform-assembly-using-target ((target sys.c:ppc64le-target) code-list &rest args &key &allow-other-keys)
  (apply 'sys.lap:perform-assembly *instruction-assemblers* code-list args))

(defun add-instruction (name function)
  (unless (keywordp name)
    (export name :mezzano.lap.ppc64le))
  (setf (gethash name *instruction-assemblers*) function)
  name)

(defmacro define-instruction (name lambda-list &body body)
  (let ((fname (intern (format nil "~:@(~a~)-ASSEMBLER" name))))
    `(progn
       (defun ,fname ,lambda-list
         ,@body)
       (add-instruction ',name ',fname))))

(defun emit-instruction (value)
  (check-type value (unsigned-byte 32))
  (sys.lap:emit (ldb (byte 8 0) value)
                (ldb (byte 8 8) value)
                (ldb (byte 8 16) value)
                (ldb (byte 8 24) value)))

(defun a-form (po a b c d xo rc)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           (ash d 6)
           (ash xo 1)
           rc)))

(defun b-form (po bo bi bd aa lk)
  (emit-instruction
   (logior (ash po 26)
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

(defun dx-form (po rt d1 d0 xo d2)
  (emit-instruction
   (logior (ash po 26)
           (ash rt 21)
           (ash d1 16)
           (ash d0 6)
           (ash xo 1)
           d2)))

(defun i-form (po li aa lk)
  (emit-instruction
   (logior (ash po 26)
           (ash li 2)
           (ash aa 1)
           lk)))

(defun m-form (po rs ra a mb me rc)
  (emit-instruction
   (logior (ash po 26)
           (ash rs 21)
           (ash ra 16)
           (ash a 11)
           (ash mb 6)
           (ash me 1)
           rc)))

;; TODO sh0 , sh1
;; (defun md-form (po rs ra sh a xo rc)
;;   (emit-instruction
;;    (logior (ash po 26)
;;            (ash rs 21)
;;            (ash ra 16)
;;            (ash sh0 11)
;;            (ash a 5)
;;            (ash xo 2)
;;            (ash sh1 1)
;;            rc)))

(defun mds-form (po rs ra rb a xo rc)
  (emit-instruction
   (logior (ash po 26)
           (ash rs 21)
           (ash ra 16)
           (ash rb 11)
           (ash a 5)
           (ash xo 1)
           rc)))

(defun sc-form (po lev a b)
  (emit-instruction
   (logior (ash po 26)
           (ash lev 5)
           (ash a 1)
           b)))

(defun va-form (po a b c d xo)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           (ash d 6)
           xo)))

(defun vc-form (po vrt vra vrb rc xo)
  (emit-instruction
   (logior (ash po 26)
           (ash vrt 21)
           (ash vra 16)
           (ash vrb 11)
           (ash rc 10)
           xo)))

(defun vx-form (po a b c xo)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           xo)))

(defun x-form (po a b c xo &optional (d 0))
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           (ash xo 1)
           d)))

(defun xfl-form (po l flm w frb xo rc)
  (emit-instruction
   (logior (ash po 26)
           (ash l 25)
           (ash flm 17)
           (ash w 16)
           (ash frb 11)
           (ash xo 1)
           rc)))

(defun xfx-form (po a b xo)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 11)
           (ash xo 1))))

(defun xl-form (po a b c xo &optional (lk 0))
  (emit-instruction
   (logior (ash po 26)
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

;; TODO sh0 sh1
;; (defun xs-form (po rs ra sh xo rc)
;;   (emit-instruction
;;    (logior (ash po 26)
;;            (ash rs 21)
;;            (ash ra 16)
;;            (ash sh0 11)
;;            (ash xo 2)
;;            (ash sh1 1)
;;            rc)))

;; TODO
;; xx2-form
;; xx3-form

(defun xx4-form (po target a b c xo cx ax bx tx)
  (emit-instruction
   (logior (ash po 26)
           (ash target 21)
           (ash a 16)
           (ash b 11)
           (ash c 8)
           (ash xo 4)
           (ash cx 3)
           (ash ax 2)
           (ash bx 1)
           tx)))


(defun z22-form (po a b c xo rc)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           (ash xo 1)
           rc)))

(defun z23-form (po a b c rmc xo d)
  (emit-instruction
   (logior (ash po 26)
           (ash a 21)
           (ash b 16)
           (ash c 11)
           (ash rmc 9)
           (ash xo 1)
           d)))

;; branch
;; (define-instruction b (target_addr)
;;   (i-form 18 li 0 0))

;; (define-instruction ba (target_addr)
;;   (i-form 18 li 1 0))

;; (define-instruction bl (target_addr)
;;   (i-form 18 li 0 1))

;; (define-instruction bla (target_addr)
;;   (i-form 18 li 1 1))

;; TODO
;; branch conditional
;; (define-instruction bc (bo bi target_addr)
;;   (b-form 16 bo bi bd 0 0))

;; (define-instruction bca (bo bi target_addr)
;;   (b-form 16 bo bi bd 0 1))

;; (define-instruction bcl (bo bi target_addr)
;;   (b-form 16 bo bi bd 1 0))

;; (define-instruction bcla (bo bi target_addr)
;;   (b-form 16 bo bi bd 1 1))

;; branch conditional to link register
(define-instruction bclr (bo bi bh)
  (xl-form 19 bo bi bh 16 0))

(define-instruction bclrl (bo bi bh)
  (xl-form 19 bo bi bh 16 1))

;; branch conditional to count register
(define-instruction bcctr (bo bi bh)
  (xl-form 19 bo bi bh 528 0))

(define-instruction bcctrl (bo bi bh)
  (xl-form 19 bo bi bh 528 1))

;;branch conditional to branch target address register
(define-instruction bctar (bo bi bh)
  (xl-form 19 bo bi bh 560 0))

(define-instruction bctarl (bo bi bh)
  (xl-form 19 bo bi bh 560 1))

;; condition register and
(define-instruction crand (bt ba bb)
  (xl-form 19 bt ba bb 257))

;; condition register or
(define-instruction cror (bt ba bb)
  (xl-form 19 bt ba bb 449))

;; condition register nand
(define-instruction crnand (bt ba bb)
  (xl-form 19 bt ba bb 225))

;; condition register xor
(define-instruction crxor (bt ba bb)
  (xl-form 19 bt ba bb 193))

;; condition register nor
(define-instruction crnor (bt ba bb)
  (xl-form 19 bt ba bb 33))

;; condition register and with complement
(define-instruction crandc (bt ba bb)
  (xl-form 19 bt ba bb 129))

;; move condition register field
(define-instruction mcrf (bf bfa)
  (xl-form 19 (ash bf 2) (ash bfa 2) 0 0))

;; condition register equivalent
(define-instruction creqv (bt ba bb)
  (xl-form 19 bt ba bb 289))

;; condition register or with complement
(define-instruction crorc (bt ba bb)
  (xl-form 19 bt ba bb 417))

;; system call
(define-instruction sc (lev)
  (sc-form 17 lev 1 0))

;; system call vectored
(define-instruction scv (lev)
  (sc-form 17 lev 0 1))

;; load byte and zero
(define-instruction lbz (rt d ra)
  (d-form 34 rt ra d))

;; load byte and zero with update
(define-instruction lbzu (rt d ra)
  (d-form 35 rt ra d))

;; load byte and zero indexed
(define-instruction lbzx (rt ra rb)
  (x-form 31 rt ra rb 87))

;; load byte and zero with update indexed
(define-instruction lbzux (rt ra rb)
  (x-form 31 rt ra rb 119))

;; load halfword and zero
(define-instruction lhz (rt d ra)
  (d-form 40 rt ra d))

;; load halfword and zero with update
(define-instruction lhzu (rt d ra)
  (d-form 41 rt ra d))

;; load halfword and zero indexed
(define-instruction lhzx (rt ra rb)
  (x-form 31 rt ra rb 279))

;; load halfword and zero with update indexed
(define-instruction lhzux (rt ra rb)
  (x-form 31 rt ra rb 311))

;; load halfword algebraic
(define-instruction lha (rt d ra)
  (d-form 42 rt ra d))

;; load halfword algebraic with update
(define-instruction lhau (rt d ra)
  (d-form 43 rt ra d))

;; load halfword algebraic indexed
(define-instruction lhax (rt ra rb)
  (x-form 31 rt ra rb 343))

;; load halfword algebraic with update indexed
(define-instruction lhaux (rt ra rb)
  (x-form 31 rt ra rb 375))

;; load word and zero
(define-instruction lwz (rt d ra)
  (d-form 32 rt ra d))

;; load word and zero with update
(define-instruction lwzu (rt d ra)
  (d-form 33 rt ra d))

;; load word and zero indexed
(define-instruction lwzx (rt ra rb)
  (x-form 31 rt ra rb 23))

;; load word and zero with update indexed
(define-instruction lwzux (rt ra rb)
  (x-form 31 rt ra rb 55))

;; load word algebraic
(define-instruction lwa (rt ds ra)
  (ds-form 58 rt ra ds 2))

;; load word algebraic indexed
(define-instruction lwax (rt ra rb)
  (x-form 31 rt ra rb 341))

;; load word algebraic with update indexed
(define-instruction lwaux (rt ra rb)
  (x-form 31 rt ra rb 373))

;; load doubleword
(define-instruction ld (rt ds ra)
  (ds-form 58 rt ra ds 0))

;; load doubleword with update
(define-instruction ldu (rt ds ra)
  (ds-form 58 rt ra ds 1))

;; load doubleword indexed
(define-instruction ldx (rt ra rb)
  (x-form 31 rt ra rb 21))

;; load doubleword with update indexed
(define-instruction ldux (rt ra rb)
  (x-form 31 rt ra rb 53))

;; store byte
(define-instruction stb (rs d ra)
  (d-form 38 rs ra d))

;; store byte with update
(define-instruction stbu (rs d ra)
  (d-form 39 rs ra d))

;; store byte indexed
(define-instruction stbx (rs ra rb)
  (x-form 31 rs ra rb 215))

;; store byte with update indexed
(define-instruction stbux (rs ra rb)
  (x-form 31 rs ra rb 247))

;; store halfword
(define-instruction sth (rs d ra)
  (d-form 44 rs ra d))

;; store halfword with update
(define-instruction sthu (rs d ra)
  (d-form 45 rs ra d))

;; store halfword indexed
(define-instruction sthx (rs ra rb)
  (x-form 31 rs ra rb 407))

;; store halfword with update indexed
(define-instruction sthux (rs ra rb)
  (x-form 31 rs ra rb 439))

;; store word
(define-instruction stw (rs d ra)
  (d-form 36 rs ra d))

;; store word with update
(define-instruction stwu (rs d ra)
  (d-form 37 rs ra d))

;; store word indexed
(define-instruction stwx (rs ra rb)
  (x-form 31 rs ra rb 151))

;; store word with update indexed
(define-instruction stwux (rs ra rb)
  (x-form 31 rs ra rb 183))

;; store doubleword
(define-instruction std (rs ds ra)
  (ds-form 62 rs ra ds 0))

;; store doubleword with update
(define-instruction stdu (rs ds ra)
  (ds-form 62 rs ra ds 1))

;; store doubleword indexed
(define-instruction stdx (rs ra rb)
  (x-form 31 rs ra rb 149))

;; store doubleword with update indexed
(define-instruction stdux (rs ra rb)
  (x-form 31 rs ra rb 181))

;; load quadword
(define-instruction lq (rtp dq ra)
  (dq-form 56 rtp ra dq 0))

;; store quadword
(define-instruction stq (rsp ds ra)
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
(define-instruction lmw (rt d ra)
  (d-form 46 rt ra d))

;; store multiple word
(define-instruction stmw (rs d ra)
  (d-form 47 rs ra d))

;; load string word immediate
(define-instruction lswi (rt ra nb)
  (x-form 31 rt ra nb 597))

;; load string word indexed
(define-instruction lswx (rt ra rb)
  (x-form 31 rt ra rb 533))

;; store string word immediate
(define-instruction stswi (rs ra nb)
  (x-form 31 rs ra nb 725))

;; store string word indexed
(define-instruction stswx (rs ra rb)
  (x-form 31 rs ra rb 661))

;; add immediate
(define-instruction addi (rt ra si)
  (d-form 14 rt ra si))

;; add immediate shifted
(define-instruction addis (rt ra si)
  (d-form 15 rt ra si))

;; TODO
;; add pc immediate shifted
;; (define-instruction addpcis (rt d)
;;   (dx-form 19 rt d1 d0 2 d2))

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
(define-instruction addic. (rt ra si)
  (d-form 13 rt ra si))

;; subtract from immediate carrying
(define-instruction subfic (rt ra si)
  (d-form 8 rt ra si))

;; add carrying
(define-instruction addc (rt ra rb)
  (xo-form 31 rt ra rb 0 10 0))

(define-instruction addc. (rt ra rb)
  (xo-form 31 rt ra rb 0 10 1))

(define-instruction addco (rt ra rb)
  (xo-form 31 rt ra rb 1 10 0))

(define-instruction addco. (rt ra rb)
  (xo-form 31 rt ra rb 1 10 1))

;; subtract from carrying
(define-instruction subfc (rt ra rb)
  (xo-form 31 rt ra rb 0 8 0))

(define-instruction subfc. (rt ra rb)
  (xo-form 31 rt ra rb 0 8 1))

(define-instruction subfco (rt ra rb)
  (xo-form 31 rt ra rb 1 8 0))

(define-instruction subfco. (rt ra rb)
  (xo-form 31 rt ra rb 1 8 1))

;; add extended
(define-instruction adde (rt ra rb)
  (xo-form 31 rt ra rb 0 138 0))

(define-instruction adde. (rt ra rb)
  (xo-form 31 rt ra rb 0 138 1))

(define-instruction addeo (rt ra rb)
  (xo-form 31 rt ra rb 1 138 0))

(define-instruction addeo. (rt ra rb)
  (xo-form 31 rt ra rb 1 138 1))

;; add to minus one extended
(define-instruction addme (rt ra)
  (xo-form 31 rt ra 0 0 234 0))

(define-instruction addme. (rt ra)
  (xo-form 31 rt ra 0 0 234 1))

(define-instruction addmeo (rt ra)
  (xo-form 31 rt ra 0 1 234 0))

(define-instruction addmeo. (rt ra)
  (xo-form 31 rt ra 0 1 234 1))

;; subtract from extended
(define-instruction subfe (rt ra rb)
  (xo-form 31 rt ra rb 0 136 0))

(define-instruction subfe. (rt ra rb)
  (xo-form 31 rt ra rb 0 136 1))

(define-instruction subfeo (rt ra rb)
  (xo-form 31 rt ra rb 1 136 0))

(define-instruction subfeo. (rt ra rb)
  (xo-form 31 rt ra rb 1 136 1))

;; subtract from minus one extended
(define-instruction subfme (rt ra)
  (xo-form 31 rt ra 0 0 232 0))

(define-instruction subfme. (rt ra)
  (xo-form 31 rt ra 0 0 232 1))

(define-instruction subfmeo (rt ra)
  (xo-form 31 rt ra 0 1 232 0))

(define-instruction subfmeo. (rt ra)
  (xo-form 31 rt ra 0 1 232 1))

;; add extended using alternate carry bit
(define-instruction addex (rt ra rb cy)
  (z23-form 31 rt ra rb cy 170 0))

;; add to zero extended
(define-instruction addze (rt ra)
  (xo-form 31 rt ra 0 0 202 0))

(define-instruction addze. (rt ra)
  (xo-form 31 rt ra 0 0 202 1))

(define-instruction addzeo (rt ra)
  (xo-form 31 rt ra 0 1 202 0))

(define-instruction addzeo. (rt ra)
  (xo-form 31 rt ra 0 1 202 1))

;; subtract from zero extended
(define-instruction subfze (rt ra)
  (xo-form 31 rt ra 0 0 200 0))

(define-instruction subfze. (rt ra)
  (xo-form 31 rt ra 0 0 200 1))

(define-instruction subfzeo (rt ra)
  (xo-form 31 rt ra 0 1 200 0))

(define-instruction subfzeo. (rt ra)
  (xo-form 31 rt ra 0 1 200 1))

;; negate
(define-instruction neg (rt ra)
  (xo-form 31 rt ra 0 0 104 0))

(define-instruction neg. (rt ra)
  (xo-form 31 rt ra 0 0 104 1))

(define-instruction nego (rt ra)
  (xo-form 31 rt ra 0 1 104 0))

(define-instruction nego. (rt ra)
  (xo-form 31 rt ra 0 1 104 1))

;; multiply low immediate
(define-instruction mulli (rt ra si)
  (d-form 7 rt ra si))

;; multiply low word
(define-instruction mullw (rt ra rb)
  (xo-form 31 rt ra rb 0 235 0))

(define-instruction mullw. (rt ra rb)
  (xo-form 31 rt ra rb 0 235 1))

(define-instruction mullwo (rt ra rb)
  (xo-form 31 rt ra rb 1 235 0))

(define-instruction mullwo. (rt ra rb)
  (xo-form 31 rt ra rb 1 235 1))

;; multiply high word
(define-instruction mulhw (rt ra rb)
  (xo-form 31 rt ra rb 0 75 0))

(define-instruction mulhw. (rt ra rb)
  (xo-form 31 rt ra rb 0 75 1))

;; multiply high word unsigned
(define-instruction mulhwu (rt ra rb)
  (xo-form 31 rt ra rb 0 11 0))

(define-instruction mulhwu. (rt ra rb)
  (xo-form 31 rt ra rb 0 11 1))

;; divide word
(define-instruction divw (rt ra rb)
  (xo-form 31 rt ra rb 0 491 0))

(define-instruction divw. (rt ra rb)
  (xo-form 31 rt ra rb 0 491 1))

(define-instruction divwo (rt ra rb)
  (xo-form 31 rt ra rb 1 491 0))

(define-instruction divwo. (rt ra rb)
  (xo-form 31 rt ra rb 1 491 1))

;; divide word unsigned
(define-instruction divwu (rt ra rb)
  (xo-form 31 rt ra rb 0 459 0))

(define-instruction divwu. (rt ra rb)
  (xo-form 31 rt ra rb 0 459 1))

(define-instruction divwuo (rt ra rb)
  (xo-form 31 rt ra rb 1 459 0))

(define-instruction divwuo. (rt ra rb)
  (xo-form 31 rt ra rb 1 459 1))

;; divide word extended
(define-instruction divwe (rt ra rb)
  (xo-form 31 rt ra rb 0 427 0))

(define-instruction divwe. (rt ra rb)
  (xo-form 31 rt ra rb 0 427 1))

(define-instruction divweo (rt ra rb)
  (xo-form 31 rt ra rb 1 427 0))

(define-instruction divweo. (rt ra rb)
  (xo-form 31 rt ra rb 1 427 1))

;; divide word extended unsigned
(define-instruction divweu (rt ra rb)
  (xo-form 31 rt ra rb 0 395 0))

(define-instruction divweu. (rt ra rb)
  (xo-form 31 rt ra rb 0 395 1))

(define-instruction divweuo (rt ra rb)
  (xo-form 31 rt ra rb 1 395 0))

(define-instruction divweuo. (rt ra rb)
  (xo-form 31 rt ra rb 1 395 1))

;; modulo signed word
(define-instruction modsw (rt ra rb)
  (x-form 31 rt ra rb 779))

;; modulo unsigned word
(define-instruction moduw (rt ra rb)
  (x-form 31 rt ra rb 267))

;; deliver a random number
(define-instruction darn (rt l)
  (x-form 31 rt l 0 755))

;; multiply low doubleword
(define-instruction mulld (rt ra rb)
  (xo-form 31 rt ra rb 0 233 0))

(define-instruction mulld. (rt ra rb)
  (xo-form 31 rt ra rb 0 233 1))

(define-instruction mulldo (rt ra rb)
  (xo-form 31 rt ra rb 1 233 0))

(define-instruction mulldo. (rt ra rb)
  (xo-form 31 rt ra rb 1 233 1))

;; multiply high doubleword
(define-instruction mulhd (rt ra rb)
  (xo-form 31 rt ra rb 0 73 0))

(define-instruction mulhd. (rt ra rb)
  (xo-form 31 rt ra rb 0 73 1))

;; multiply high doubleword unsigned
(define-instruction mulhdu (rt ra rb)
  (xo-form 31 rt ra rb 0 9 0))

(define-instruction mulhdu. (rt ra rb)
  (xo-form 31 rt ra rb 0 9 1))

;; multiply-add high doubleword
(define-instruction maddhd (rt ra rb rc)
  (va-form 4 rt ra rb rc 48))

;; multiply-add high doubleword unsigned
(define-instruction maddhdu (rt ra rb rc)
  (va-form 4 rt ra rb rc 49))

;; multiply-add low doubleword
(define-instruction maddld (rt ra rb rc)
  (va-form 4 rt ra rb rc 51))

;; divide doubleword
(define-instruction divd (rt ra rb)
  (xo-form 31 rt ra rb 0 489 0))

(define-instruction divd. (rt ra rb)
  (xo-form 31 rt ra rb 0 489 1))

(define-instruction divdo (rt ra rb)
  (xo-form 31 rt ra rb 1 489 0))

(define-instruction divdo. (rt ra rb)
  (xo-form 31 rt ra rb 1 489 1))

;; divide doubleword unsigned
(define-instruction divdu (rt ra rb)
  (xo-form 31 rt ra rb 0 457 0))

(define-instruction divdu. (rt ra rb)
  (xo-form 31 rt ra rb 0 457 1))

(define-instruction divduo (rt ra rb)
  (xo-form 31 rt ra rb 1 457 0))

(define-instruction divduo. (rt ra rb)
  (xo-form 31 rt ra rb 1 457 1))

;; divide doubleword extended
(define-instruction divde (rt ra rb)
  (xo-form 31 rt ra rb 0 425 0))

(define-instruction divde. (rt ra rb)
  (xo-form 31 rt ra rb 0 425 1))

(define-instruction divdeo (rt ra rb)
  (xo-form 31 rt ra rb 1 425 0))

(define-instruction divdeo. (rt ra rb)
  (xo-form 31 rt ra rb 1 425 1))

;; divide doubleword extended unsigned
(define-instruction divdeu (rt ra rb)
  (xo-form 31 rt ra rb 0 393 0))

(define-instruction divdeu. (rt ra rb)
  (xo-form 31 rt ra rb 0 393 1))

(define-instruction divdeuo (rt ra rb)
  (xo-form 31 rt ra rb 1 393 0))

(define-instruction divdeuo. (rt ra rb)
  (xo-form 31 rt ra rb 1 393 1))

;; modulo signed doubleword
(define-instruction modsd (rt ra rb)
  (x-form 31 rt ra rb 777 0))

;; modulo unsigned doubleword
(define-instruction modud (rt ra rb)
  (x-form 31 rt ra rb 265 0))

;; compare immediate
(define-instruction cmpi (bf l ra si)
  (d-form 11 (logior (ash bf 2) l) ra si))

;; compare
(define-instruction cmp (bf l ra rb)
  (x-form 31 (logior (ash bf 2) l) ra rb 0))

;; compare logical immediate
(define-instruction cmpli (bf l ra ui)
  (d-form 10 (logior (ash bf 2) l) ra ui))

;; compare logical
(define-instruction cmpl (bf l ra rb)
  (x-form 31 (logior (ash bf 2) l) ra rb 32))

;; compare ranged byte
(define-instruction cmprb (bf l ra rb)
  (x-form 31 (logior (ash bf 2) l) ra rb 192))

;; compare equal byte
(define-instruction cmpeqb (bf ra rb)
  (x-form 31 (ash bf 2) ra rb 224))

;; trap word immediate
(define-instruction twi (to ra si)
  (d-form 3 to ra si))

;; trap word
(define-instruction tw (to ra rb)
  (x-form 31 to ra rb 4))

;; trap doubleword immediate
(define-instruction tdi (to ra si)
  (d-form 2 to ra si))

;; integer select
(define-instruction isel (rt ra rb bc)
  (a-form 31 rt ra rb bc 15 0))

;; trap doubleword
(define-instruction td (to ra rb)
  (x-form 31 to ra rb 68))

;; and immediate
(define-instruction andi. (ra rs ui)
  (d-form 28 rs ra ui))

;; and immediate shifted
(define-instruction andis. (ra rs ui)
  (d-form 29 rs ra ui))

;; or immediate
(define-instruction ori (ra rs ui)
  (d-form 24 rs ra ui))

;; or immediate shifted
(define-instruction oris (ra rs ui)
  (d-form 25 rs ra ui))

;; xor immediate
(define-instruction xori (ra rs ui)
  (d-form 26 rs ra ui))

;; xor immediate shifted
(define-instruction xoris (ra rs ui)
  (d-form 27 rs ra ui))

;; and
(define-instruction and (ra rs rb)
  (x-form 31 rs ra rb 28 0))

(define-instruction and. (ra rs rb)
  (x-form 31 rs ra rb 28 1))

;; xor
(define-instruction xor (ra rs rb)
  (x-form 31 rs ra rb 316 0))

(define-instruction xor. (ra rs rb)
  (x-form 31 rs ra rb 316 1))

;; nand
(define-instruction nand (ra rs rb)
  (x-form 31 rs ra rb 476 0))

(define-instruction nand. (ra rs rb)
  (x-form 31 rs ra rb 476 1))

;; or
(define-instruction or (ra rs rb)
  (x-form 31 rs ra rb 444 0))

(define-instruction or. (ra rs rb)
  (x-form 31 rs ra rb 444 1))

;; nor
(define-instruction nor (ra rs rb)
  (x-form 31 rs ra rb 124 0))

(define-instruction nor. (ra rs rb)
  (x-form 31 rs ra rb 124 1))

;; and with complement
(define-instruction andc (ra rs rb)
  (x-form 31 rs ra rb 60 0))

(define-instruction andc. (ra rs rb)
  (x-form 31 rs ra rb 60 1))

;; equivalent
(define-instruction eqv (ra rs rb)
  (x-form 31 rs ra rb 284 0))

(define-instruction eqv. (ra rs rb)
  (x-form 31 rs ra rb 284 1))

;; or with complement
(define-instruction orc (ra rs rb)
  (x-form 31 rs ra rb 412 0))

(define-instruction orc. (ra rs rb)
  (x-form 31 rs ra rb 412 1))

;; extend sign byte
(define-instruction extsb (ra rs)
  (x-form 31 rs ra 0 954 0))

(define-instruction extsb. (ra rs)
  (x-form 31 rs ra 0 954 1))

;; count leading zeros word
(define-instruction cntlzw (ra rs)
  (x-form 31 rs ra 0 26 0))

(define-instruction cntlzw. (ra rs)
  (x-form 31 rs ra 0 26 1))

;; extend sign halfword
(define-instruction extsh (ra rs)
  (x-form 31 rs ra 0 922 0))

(define-instruction extsh. (ra rs)
  (x-form 31 rs ra 0 922 1))

;; count trailing zeros word
(define-instruction cnttzw (ra rs)
  (x-form 31 rs ra 0 538 0))

(define-instruction cnttzw. (ra rs)
  (x-form 31 rs ra 0 538 1))

;; compare bytes
(define-instruction cmpb (ra rs rb)
  (x-form 31 rs ra rb 508))

;; population count bytes
(define-instruction popcntb (ra rs)
  (x-form 31 rs ra 0 122))

;; population count words
(define-instruction popcntw (ra rs)
  (x-form 31 rs ra 0 378))

;; parity doubleword
(define-instruction prtyd (ra rs)
  (x-form 31 rs ra 0 186))

;; parity word
(define-instruction prtyw (ra rs)
  (x-form 31 rs ra 0 154))

;; extend sign word
(define-instruction extsw (ra rs)
  (x-form 31 rs ra 0 986 0))

(define-instruction extsw. (ra rs)
  (x-form 31 rs ra 0 986 1))

;; count leading zeros doubleword
(define-instruction cntlzd (ra rs)
  (x-form 31 rs ra 0 58 0))

(define-instruction cntlzd. (ra rs)
  (x-form 31 rs ra 0 58 1))

;; population count doubleword
(define-instruction popcntd (ra rs)
  (x-form 31 rs ra 0 506 0))

;; count trailing zeros doubleword
(define-instruction cnttzd (ra rs)
  (x-form 31 rs ra 0 570 0))

(define-instruction cnttzd. (ra rs)
  (x-form 31 rs ra 0 570 1))

;; bit permute doubleword
(define-instruction bpermd (ra rs rb)
  (x-form 31 rs ra rb 252))

;; rotate left word immediate then and with mask
(define-instruction rlwinm (ra rs sh mb me)
  (m-form 21 rs ra sh mb me 0))

(define-instruction rlwinm. (ra rs sh mb me)
  (m-form 21 rs ra sh mb me 1))

;; rotate left word then and with mask
(define-instruction rlwnm (ra rs rb mb me)
  (m-form 23 rs ra rb mb me 0))

(define-instruction rlwnm. (ra rs rb mb me)
  (m-form 23 rs ra rb mb me 1))

;; rotate left word immediate then mask insert
(define-instruction rlwimi (ra rs sh mb me)
  (m-form 20 rs ra sh mb me 0))

(define-instruction rlwimi. (ra rs sh mb me)
  (m-form 20 rs ra sh mb me 1))

;; ;; rotate left doubleword immediate then clear left
;; (define-instruction rldicl (ra rs sh mb)
;;   (md-form 30 rs ra sh mb 0 0))

;; (define-instruction rldicl. (ra rs sh mb)
;;   (md-form 30 rs ra sh mb 0 1))

;; ;; rotate left doubleword immediate then clear right
;; (define-instruction rldicr (ra rs sh me)
;;   (md-form 30 rs ra sh me 1 0))

;; (define-instruction rldicr. (ra rs sh me)
;;   (md-form 30 rs ra sh me 1 1))

;; ;; rotate left doubleword immediate then clear
;; (define-instruction rldic (ra rs sh mb)
;;   (md-form 30 rs ra sh mb 2 0))

;; (define-instruction rldic. (ra rs sh mb)
;;   (md-form 30 rs ra sh mb 2 1))

;; rotate left doubleword then clear left
(define-instruction rldcl (ra rs rb mb)
  (mds-form 30 rs ra rb mb 8 0))

(define-instruction rldcl. (ra rs rb mb)
  (mds-form 30 rs ra rb mb 8 1))

;; rotate left doubleword then clear right
(define-instruction rldcr (ra rs rb me)
  (mds-form 30 rs ra rb me 9 0))

(define-instruction rldcr. (ra rs rb me)
  (mds-form 30 rs ra rb me 9 1))

;; ;; rotate left doubleword immediate then mask insert
;; (define-instruction rldimi (ra rs sh mb)
;;   (md-form 30 rs ra sh mb 3 0))

;; (define-instruction rldimi. (ra rs sh mb)
;;   (md-form 30 rs ra sh mb 3 1))

;; shift left word
(define-instruction slw (ra rs rb)
  (x-form 31 rs ra rb 24 0))

(define-instruction slw. (ra rs rb)
  (x-form 31 rs ra rb 24 1))

;; shift right word
(define-instruction srw (ra rs rb)
  (x-form 31 rs ra rb 536 0))

(define-instruction srw. (ra rs rb)
  (x-form 31 rs ra rb 536 1))

;; shift right algebraic word immediate
(define-instruction srawi (ra rs sh)
  (x-form 31 rs ra sh 824 0))

(define-instruction srawi. (ra rs sh)
  (x-form 31 rs ra sh 824 1))

;; shift right algebraic word
(define-instruction sraw (ra rs rb)
  (x-form 31 rs ra rb 792 0))

(define-instruction sraw. (ra rs rb)
  (x-form 31 rs ra rb 792 1))

;; shift left doubleword
(define-instruction sld (ra rs rb)
  (x-form 31 rs ra rb 27 0))

(define-instruction sld. (ra rs rb)
  (x-form 31 rs ra rb 27 1))

;; shift right doubleword
(define-instruction srd (ra rs rb)
  (x-form 31 rs ra rb 539 0))

(define-instruction srd. (ra rs rb)
  (x-form 31 rs ra rb 539 1))

;; ;; shift right algebraic doubleword immediate
;; (define-instruction sradi (ra rs sh)
;;   (xs-form 31 rs ra sh 413 0))

;; (define-instruction sradi. (ra rs sh)
;;   (xs-form 31 rs ra sh 413 1))

;; shift right algebraic doubleword
(define-instruction srad (ra rs rb)
  (x-form 31 rs ra rb 794 0))

(define-instruction srad. (ra rs rb)
  (x-form 31 rs ra rb 794 1))

;; ;; extend-sign word and shift left immediate
;; (define-instruction extswsli (ra rs sh)
;;   (xs-form 31 rs ra sh 445 0))

;; (define-instruction extswsli. (ra rs sh)
;;   (xs-form 31 rs ra sh 445 1))

;; convert declets to binary coded decimal
(define-instruction cdtbcd (ra rs)
  (x-form 31 rs ra 0 282))

;; convert binary coded decimal to declets
(define-instruction cbcdtd (ra rs)
  (x-form 31 rs ra 0 314))

;; add and generate sixes
(define-instruction addg6s (rt ra rb)
  (xo-form 31 rt ra rb 0 74 0))

;; TODO
;; move from vsr doubleword
;; (define-instruction mfvsrd (ra xs)
;;   (x-form 31 s ra 0 51 sx))

;; TODO
;; move from vsr lower doubleword
;; (define-instruction mfvsrld (ra xs)
;;   (x-form 31 s ra 0 307 sx))

;; TODO
;; move from vsr word and zero
;; (define-instruction mfvsrwz (ra xs)
;;   (x-form 31 s ra 0 115 sx))

;; TODO
;; move to vsr doubleword
;; (define-instruction mtvsrd (xt ra)
;;   (x-form 31 t ra 0 179 tx))

;; TODO
;; move to vsr word algebraic
;; (define-instruction mtvsrwa (xt ra)
;;   (x-form 31 t ra 0 211 tx))

;; TODO
;; move to vsr word and zero
;; (define-instruction mtvsrwz (xt ra)
;;   (x-form 31 t ra 0 243 tx))

;; TODO
;; move to vsr double doubleword
;; (define-instruction mtvsrdd (xt ra rb)
;;   (x-form 31 t ra rb 435 tx))

;; TODO
;; move to vsr word & splat
;; (define-instruction mtvsrws (xt ra)
;;   (x-form 31 t ra 0 403 tx))

;; move to special purpose register
(define-instruction mtspr (spr rs)
  (xfx-form 31 rs spr 467))

;; move from special purpose register
(define-instruction mfspr (rt spr)
  (xfx-form 31 rt spr 339))

;; move to cr from xer extended
(define-instruction mcrxrx (bf)
  (x-form 31 (ash bf 2) 0 0 576))

;; move to one condition register field
(define-instruction mtocrf (fxm rs)
  (xfx-form 31 rs (logior (ash 1 9) (ash fxm 1)) 144))

;; move to condition register fields
(define-instruction mtcrf (fxm rs)
  (xfx-form 31 rs (ash fxm 1) 144))

;; move from one condition register field
(define-instruction mfocrf (rt fxm)
  (xfx-form 31 rt (logior (ash 1 9) (ash fxm 1)) 19))

;; move from condition register
(define-instruction mfcr (rt)
  (xfx-form 31 rt 0 19))

;; set boolean
(define-instruction setb (rt bfa)
  (x-form 31 rt (ash bfa 2) 0 128))

;; load floating-point single
(define-instruction lfs (frt d ra)
  (d-form 48 frt ra d))

;; load floating-point single indexed
(define-instruction lfsx (frt ra rb)
  (x-form 31 frt ra rb 535))

;; load floating-point single with update
(define-instruction lfsu (frt d ra)
  (d-form 49 frt ra d))

;; load floating-point single with update indexed
(define-instruction lfsux (frt ra rb)
  (x-form 31 frt ra rb 567))

;; load floating-point double
(define-instruction lfd (frt d ra)
  (d-form 50 frt ra d))

;; load floating-point double indexed
(define-instruction lfdx (frt ra rb)
  (x-form 31 frt ra rb 599))

;; load floating-point double with update
(define-instruction lfdu (frt d ra)
  (d-form 51 frt ra d))

;; load floating-point double with update indexed
(define-instruction lfdux (frt ra rb)
  (x-form 31 frt ra rb 631))

;; load floating-point as integer word algebraic indexed
(define-instruction lfiwax (frt ra rb)
  (x-form 31 frt ra rb 887))

;; load floating-point as integer word and zero indexed
(define-instruction lfiwzx (frt ra rb)
  (x-form 31 frt ra rb 855))

;; store floating-point single
(define-instruction stfs (frs d ra)
  (d-form 52 frs ra d))

;; store floating-point single with update
(define-instruction stfsu (frs d ra)
  (d-form 53 frs ra d))

;; store floating-point single indexed
(define-instruction stfsx (frs ra rb)
  (x-form 31 frs ra rb 663))

;; store floating-point single with update indexed
(define-instruction stfsux (frs ra rb)
  (x-form 31 frs ra rb 695))

;; store floating-point double
(define-instruction stfd (frs d ra)
  (d-form 54 frs ra d))

;; store floating-point double with update
(define-instruction stfdu (frs d ra)
  (d-form 55 frs ra d))

;; store floating-point double indexed
(define-instruction stfdx (frs ra rb)
  (x-form 31 frs ra rb 727))

;; store floating-point double with update indexed
(define-instruction stfdux (frs ra rb)
  (x-form 31 frs ra rb 759))

;; store floating-point as integer word indexed
(define-instruction stfiwx (frs ra rb)
  (x-form 31 frs ra rb 983))

;; load floating-point double pair
(define-instruction lfdp (frtp ds ra)
  (ds-form 57 frtp ra ds 0))

;; load floating-point double pair indexed
(define-instruction lfdpx (frtp ra rb)
  (x-form 31 frtp ra rb 791))

;; store floating-point double pair
(define-instruction stfdp (frsp ds ra)
  (ds-form 61 frsp ra ds 0))

;; store floating-point double pair indexed
(define-instruction stfdpx (frsp ra rb)
  (x-form 31 frsp ra rb 919))

;; floating move register
(define-instruction fmr (frt frb)
  (x-form 63 frt 0 frb 72 0))

(define-instruction fmr. (frt frb)
  (x-form 63 frt 0 frb 72 1))

;; floating absolute value
(define-instruction fabs (frt frb)
  (x-form 63 frt 0 frb 264 0))

(define-instruction fabs. (frt frb)
  (x-form 63 frt 0 frb 264 1))

;; floating negative absolute value
(define-instruction fnabs (frt frb)
  (x-form 63 frt 0 frb 136 0))

(define-instruction fnabs. (frt frb)
  (x-form 63 frt 0 frb 136 1))

;; floating negate
(define-instruction fneg (frt frb)
  (x-form 63 frt 0 frb 40 0))

(define-instruction fneg. (frt frb)
  (x-form 63 frt 0 frb 40 1))

;; floating copy sign
(define-instruction fcpsgn (frt fra frb)
  (x-form 63 frt fra frb 8 0))

(define-instruction fcpsgn. (frt fra frb)
  (x-form 63 frt fra frb 8 1))

;; floating merge even word
(define-instruction fmrgew (frt fra frb)
  (x-form 63 frt fra frb 966))

;; floating merge odd word
(define-instruction fmrgow (frt fra frb)
  (x-form 63 frt fra frb 838))

;; floating add [single]
(define-instruction fadd (frt fra frb)
  (a-form 63 frt fra frb 0 21 0))

(define-instruction fadd. (frt fra frb)
  (a-form 63 frt fra frb 0 21 1))

(define-instruction fadds (frt fra frb)
  (a-form 59 frt fra frb 0 21 0))

(define-instruction fadds. (frt fra frb)
  (a-form 59 frt fra frb 0 21 1))

;; floating subtract [single]
(define-instruction fsub (frt fra frb)
  (a-form 63 frt fra frb 0 20 0))

(define-instruction fsub. (frt fra frb)
  (a-form 63 frt fra frb 0 20 1))

(define-instruction fsubs (frt fra frb)
  (a-form 59 frt fra frb 0 20 0))

(define-instruction fsubs. (frt fra frb)
  (a-form 59 frt fra frb 0 20 1))

;; floating multiply [single]
(define-instruction fmul (frt fra frc)
  (a-form 63 frt fra 0 frc 25 0))

(define-instruction fmul. (frt fra frc)
  (a-form 63 frt fra 0 frc 25 1))

(define-instruction fmuls (frt fra frc)
  (a-form 59 frt fra 0 frc 25 0))

(define-instruction fmuls. (frt fra frc)
  (a-form 59 frt fra 0 frc 25 1))

;; floating divide [single]
(define-instruction fdiv (frt fra frb)
  (a-form 63 frt fra frb 0 18 0))

(define-instruction fdiv. (frt fra frb)
  (a-form 63 frt fra frb 0 18 1))

(define-instruction fdivs (frt fra frb)
  (a-form 59 frt fra frb 0 18 0))

(define-instruction fdivs. (frt fra frb)
  (a-form 59 frt fra frb 0 18 1))

;; floating square root [single]
(define-instruction fsqrt (frt frb)
  (a-form 63 frt 0 frb 0 22 0))

(define-instruction fsqrt. (frt frb)
  (a-form 63 frt 0 frb 0 22 1))

(define-instruction fsqrts (frt frb)
  (a-form 59 frt 0 frb 0 22 0))

(define-instruction fsqrts. (frt frb)
  (a-form 59 frt 0 frb 0 22 1))

;; floating reciprocal estimate [single]
(define-instruction fre (frt frb)
  (a-form 63 frt 0 frb 0 24 0))

(define-instruction fre. (frt frb)
  (a-form 63 frt 0 frb 0 24 1))

(define-instruction fres (frt frb)
  (a-form 59 frt 0 frb 0 24 0))

(define-instruction fres. (frt frb)
  (a-form 59 frt 0 frb 0 24 1))

;; floating reciprocal square root estimate [single]
(define-instruction frsqrte (frt frb)
  (a-form 63 frt 0 frb 0 26 0))

(define-instruction frsqrte. (frt frb)
  (a-form 63 frt 0 frb 0 26 1))

(define-instruction frsqrtes (frt frb)
  (a-form 59 frt 0 frb 0 26 0))

(define-instruction frsqrtes. (frt frb)
  (a-form 59 frt 0 frb 0 26 1))

;; floating test for software divide
(define-instruction ftdiv (bf fra frb)
  (x-form 63 (ash bf 2) fra frb 128))

;; floating test for software square root
(define-instruction ftsqrt (bf frb)
  (x-form 63 (ash bf 2) 0 frb 160))

;; floating multiply-add [single]
(define-instruction fmadd (frt fra frc frb)
  (a-form 63 frt fra frb frc 29 0))

(define-instruction fmadd. (frt fra frc frb)
  (a-form 63 frt fra frb frc 29 1))

(define-instruction fmadds (frt fra frc frb)
  (a-form 59 frt fra frb frc 29 0))

(define-instruction fmadds. (frt fra frc frb)
  (a-form 59 frt fra frb frc 29 1))

;; floating multiply-subtract [single]
(define-instruction fmsub (frt fra frc frb)
  (a-form 63 frt fra frb frc 28 0))

(define-instruction fmsub. (frt fra frc frb)
  (a-form 63 frt fra frb frc 28 1))

(define-instruction fmsubs (frt fra frc frb)
  (a-form 59 frt fra frb frc 28 0))

(define-instruction fmsubs. (frt fra frc frb)
  (a-form 59 frt fra frb frc 28 1))

;; floating negative multiply-add [single]
(define-instruction fnmadd (frt fra frc frb)
  (a-form 63 frt fra frb frc 31 0))

(define-instruction fnmadd. (frt fra frc frb)
  (a-form 63 frt fra frb frc 31 1))

(define-instruction fnmadds (frt fra frc frb)
  (a-form 59 frt fra frb frc 31 0))

(define-instruction fnmadds. (frt fra frc frb)
  (a-form 59 frt fra frb frc 31 1))

;; floating negative multiply-subtract [single]
(define-instruction fnmsub (frt fra frc frb)
  (a-form 63 frt fra frb frc 30 0))

(define-instruction fnmsub. (frt fra frc frb)
  (a-form 63 frt fra frb frc 30 1))

(define-instruction fnmsubs (frt fra frc frb)
  (a-form 59 frt fra frb frc 30 0))

(define-instruction fnmsubs. (frt fra frc frb)
  (a-form 59 frt fra frb frc 30 1))

;; floating round to single-precision
(define-instruction frsp (frt frb)
  (x-form 63 frt 0 frb 12 0))

(define-instruction frsp. (frt frb)
  (x-form 63 frt 0 frb 12 1))

;; floating convert with round double-precision to signed doubleword format
(define-instruction fctid (frt frb)
  (x-form 63 frt 0 frb 814 0))

(define-instruction fctid. (frt frb)
  (x-form 63 frt 0 frb 814 1))

;; floating convert with truncate double-precision to signed doubleword format
(define-instruction fctidz (frt frb)
  (x-form 63 frt 0 frb 815 0))

(define-instruction fctidz. (frt frb)
  (x-form 63 frt 0 frb 815 1))

;; floating convert with round double-precision to unsigned doubleword format
(define-instruction fctidu (frt frb)
  (x-form 63 frt 0 frb 942 0))

(define-instruction fctidu. (frt frb)
  (x-form 63 frt 0 frb 942 1))

;; floating convert with truncate double-precision to unsigned doubleword format
(define-instruction fctiduz (frt frb)
  (x-form 63 frt 0 frb 943 0))

(define-instruction fctiduz. (frt frb)
  (x-form 63 frt 0 frb 943 1))

;; floating convert with round double-precision to signed word format
(define-instruction fctiw (frt frb)
  (x-form 63 frt 0 frb 14 0))

(define-instruction fctiw. (frt frb)
  (x-form 63 frt 0 frb 14 1))

;; floating convert with truncate double-precision to signed word fomat
(define-instruction fctiwz (frt frb)
  (x-form 63 frt 0 frb 15 0))

(define-instruction fctiwz. (frt frb)
  (x-form 63 frt 0 frb 15 1))

;; floating convert with round double-precision to unsigned word format
(define-instruction fctiwu (frt frb)
  (x-form 63 frt 0 frb 142 0))

(define-instruction fctiwu. (frt frb)
  (x-form 63 frt 0 frb 142 1))

;; floating convert with truncate double-precision to unsigned word format
(define-instruction fctiwuz (frt frb)
  (x-form 63 frt 0 frb 143 0))

(define-instruction fctiwuz. (frt frb)
  (x-form 63 frt 0 frb 143 1))

;; floating convert with round signed doubleword to double-precision format
(define-instruction fcfid (frt frb)
  (x-form 63 frt 0 frb 846 0))

(define-instruction fcfid. (frt frb)
  (x-form 63 frt 0 frb 846 1))

;; floating convert with round unsigned doubleword to double-precision format
(define-instruction fcfidu (frt frb)
  (x-form 63 frt 0 frb 974 0))

(define-instruction fcfidu. (frt frb)
  (x-form 63 frt 0 frb 974 1))

;; floating convert with round signed doubleword to single-precision format
(define-instruction fcfids (frt frb)
  (x-form 63 frt 0 frb 846 0))

(define-instruction fcfids. (frt frb)
  (x-form 63 frt 0 frb 846 1))

;; floating convert with round unsigned doubleword to single-precision format
(define-instruction fcfidus (frt frb)
  (x-form 63 frt 0 frb 974 0))

(define-instruction fcfidus. (frt frb)
  (x-form 63 frt 0 frb 974 1))

;; floating round to integer nearest
(define-instruction frin (frt frb)
  (x-form 63 frt 0 frb 392 0))

(define-instruction frin. (frt frb)
  (x-form 63 frt 0 frb 362 1))

;; floating round to integer toward zero
(define-instruction friz (frt frb)
  (x-form 63 frt 0 frb 424 0))

(define-instruction friz. (frt frb)
  (x-form 63 frt 0 frb 424 1))

;; floating round to integer plus
(define-instruction frip (frt frb)
  (x-form 63 frt 0 frb 456 0))

(define-instruction frip. (frt frb)
  (x-form 63 frt 0 frb 456 1))

;; floating round to integer minus
(define-instruction frim (frt frb)
  (x-form 63 frt 0 frb 488 0))

(define-instruction frim. (frt frb)
  (x-form 63 frt 0 frb 488 1))

;; floating compare unordered
(define-instruction fcmpu (bf fra frb)
  (x-form 63 (ash bf 2) fra frb 0))

;; floating compare ordered
(define-instruction fcmpo (bf fra frb)
  (x-form 63 (ash bf 2) fra frb 32))

;; floating select
(define-instruction fsel (frt fra frc frb)
  (a-form 63 frt fra frb frc 23 0))

(define-instruction fsel. (frt fra frc frb)
  (a-form 63 frt fra frb frc 23 1))

;; Move From FPSCR [& Clear Enables | Lightweight | Control [& Set (DRN|RN)[Immediate]]] X-form
(define-instruction mffs (frt)
  (x-form 63 frt 0 0 583 0))

(define-instruction mffs. (frt)
  (x-form 63 frt 0 0 583 1))

(define-instruction mffsce (frt)
  (x-form 63 frt 1 0 583 0))

(define-instruction mffscdrn (frt frb)
  (x-form 63 frt 20 frb 583 0))

(define-instruction mffscdrni (frt drm)
  (x-form 63 frt 21 drm 583 0))

(define-instruction mffscrn (frt frb)
  (x-form 63 frt 22 frb 583 0))

(define-instruction mffscrni (frt rm)
  (x-form 63 frt 23 rm 583 0))

(define-instruction mffsl (frt)
  (x-form 63 frt 24 0 583 0))

;; move to condition register from fpscr
(define-instruction mcrfs (bf bfa)
  (x-form 63 (ash bf 2) (ash bfa 2) 0 64))

;; move to fpscr field immediate
(define-instruction mtfsfi (bf u w)
  (x-form 63 (ash bf 2) w (ash u 1) 134 0))

(define-instruction mtfsfi. (bf u w)
  (x-form 63 (ash bf 2) w (ash u 1) 134 1))

;; move to fpscr fields
(define-instruction mtfsf (flm frb l w)
  (xfl-form 63 l flm w frb 711 0))

(define-instruction mtfsf. (flm frb l w)
  (xfl-form 63 l flm w frb 711 1))

;; move to fpscr bit 0
(define-instruction mtfsb0 (bt)
  (x-form 63 bt 0 0 70 0))

(define-instruction mtfsb0. (bt)
  (x-form 63 bt 0 0 70 1))

;; move to fpscr bit 1
(define-instruction mtfsb1 (bt)
  (x-form 63 bt 0 0 38 0))

(define-instruction mtfsb1. (bt)
  (x-form 63 bt 0 0 38 1))

;; dfp add [quad]
(define-instruction dadd (frt fra frb)
  (x-form 59 frt fra frb 2 0))

(define-instruction dadd. (frt fra frb)
  (x-form 59 frt fra frb 2 1))

(define-instruction daddq (frtp frap frbp)
  (x-form 63 frtp frap frbp 2 0))

(define-instruction daddq. (frtp frap frbp)
  (x-form 63 frtp frap frbp 2 1))

;; dfp subtract [quad]
(define-instruction dsub (frt fra frb)
  (x-form 59 frt fra frb 514 0))

(define-instruction dsub. (frt fra frb)
  (x-form 59 frt fra frb 514 1))

(define-instruction dsubq (frtp frap frbp)
  (x-form 63 frtp frap frbp 514 0))

(define-instruction dsubq. (frtp frap frbp)
  (x-form 63 frtp frap frbp 514 1))

;; dfp multiply [quad]
(define-instruction dmul (frt fra frb)
  (x-form 59 frt fra frb 34 0))

(define-instruction dmul. (frt fra frb)
  (x-form 59 frt fra frb 34 1))

(define-instruction dmulq (frtp frap frbp)
  (x-form 63 frtp frap frbp 34 0))

(define-instruction dmulq. (frtp frap frbp)
  (x-form 63 frtp frap frbp 34 1))

;; dfp divide [quad]
(define-instruction ddiv (frt fra frb)
  (x-form 59 frt fra frb 546 0))

(define-instruction ddiv. (frt fra frb)
  (x-form 59 frt fra frb 546 1))

(define-instruction ddivq (frtp frap frbp)
  (x-form 63 frtp frap frbp 546 0))

(define-instruction ddivq. (frtp frap frbp)
  (x-form 63 frtp frap frbp 546 1))

;; dfp compare unordered [quad]
(define-instruction dcmpu (bf fra frb)
  (x-form 59 (ash bf 2) fra frb 642))

(define-instruction dcmpuq (bf frap frbp)
  (x-form 63 (ash bf 2) frap frbp 642))

;; dfp compare ordered [quad]
(define-instruction dcmpo (bf fra frb)
  (x-form 59 (ash bf 2) fra frb 130))

(define-instruction dcmpoq (bf frap frbp)
  (x-form 63 (ash bf 2) frap frbp 130))

;; dfp test data class [quad]
(define-instruction dtstdc (bf fra dcm)
  (z22-form 59 (ash bf 2) fra dcm 194 0))

(define-instruction dtstdcq (bf frap dcm)
  (z22-form 63 (ash bf 2) frap dcm 194 0))

;; dfp test data group [quad]
(define-instruction dtstdg (bf fra dgm)
  (z22-form 59 (ash bf 2) fra dgm 226 0))

(define-instruction dtstdgq (bf frap dgm)
  (z22-form 63 (ash bf 2) frap dgm 226 0))

;; dfp test exponent [quad]
(define-instruction dtstex (bf fra frb)
  (x-form 59 (ash bf 2) fra frb 162))

(define-instruction dtstexq (bf frap frbp)
  (x-form 63 (ash bf 2) frap frbp 162))

;; dfp test significance [quad]
(define-instruction dtstsf (bf fra frb)
  (x-form 59 (ash bf 1) fra frb 674))

(define-instruction dtstsfq (bf fra frbp)
  (x-form 63 (ash bf 1) fra frbp 674))

;; dfp test significance immediate [quad]
(define-instruction dtstsfi (bf uim frb)
  (x-form 59 (ash bf 1) uim frb 675))

(define-instruction dtstsfiq (bf uim frbp)
  (x-form 63 (ash bf 1) uim frbp 675))

;; dfp quantize immediate [quad]
(define-instruction dquai (te frt frb rmc)
  (z23-form 59 frt te frb rmc 67 0))

(define-instruction dquai. (te frt frb rmc)
  (z23-form 59 frt te frb rmc 67 1))

(define-instruction dquaiq (te frtp frbp rmc)
  (z23-form 63 frtp te frbp rmc 67 0))

(define-instruction dquaiq. (te frtp frbp rmc)
  (z23-form 63 frtp te frbp rmc 67 1))

;; dfp quantize [quad]
(define-instruction dqua (frt fra frb rmc)
  (z23-form 59 frt fra frb rmc 3 0))

(define-instruction dqua. (frt fra frb rmc)
  (z23-form 59 frt fra frb rmc 3 1))

(define-instruction dquaq (frtp frap frbp rmc)
  (z23-form 63 frtp frap frbp rmc 3 0))

(define-instruction dquaq. (frtp frap frbp rmc)
  (z23-form 63 frtp frap frbp rmc 3 1))

;; dfp reround [quad]
(define-instruction drrnd (frt fra frb rmc)
  (z23-form 59 frt fra frb rmc 35 0))

(define-instruction drrnd. (frt fra frb rmc)
  (z23-form 59 frt fra frb rmc 35 1))

(define-instruction drrndq (frtp fra frbp rmc)
  (z23-form 63 frtp fra frbp rmc 35 0))

(define-instruction drrndq. (frtp fra frbp rmc)
  (z23-form 63 frtp fra frbp rmc 35 1))

;; dfp round to fp integer with inexact [quad]
(define-instruction drintx (r frt frb rmc)
  (z23-form 59 frt r frb rmc 99 0))

(define-instruction drintx. (r frt frb rmc)
  (z23-form 59 frt r frb rmc 99 1))

(define-instruction drintxq (r frtp frbp rmc)
  (z23-form 63 frtp r frbp rmc 99 0))

(define-instruction drintxq. (r frtp frbp rmc)
  (z23-form 63 frtp r frbp rmc 99 1))

;; dfp round to fp integer without inexact [quad]
(define-instruction drintn (r frt frb rmc)
  (z23-form 59 frt r frb rmc 227 0))

(define-instruction drintn. (r frt frb rmc)
  (z23-form 59 frt r frb rmc 227 1))

(define-instruction drintnq (r frtp frbp rmc)
  (z23-form 63 frtp r frbp rmc 227 0))

(define-instruction drintnq. (r frtp frbp rmc)
  (z23-form 63 frtp r frbp rmc 227 1))

;; dfp convert to dfp long
(define-instruction dctdp (frt frb)
  (x-form 59 frt 0 frb 258 0))

(define-instruction dctdp. (frt frb)
  (x-form 59 frt 0 frb 258 1))

;; dfp convert to dfp extended
(define-instruction dctqpq (frtp frb)
  (x-form 63 frtp 0 frb 258 0))

(define-instruction dctqpq. (frtp frb)
  (x-form 63 frtp 0 frb 258 1))

;; dfp round to dfp short
(define-instruction drsp (frt frb)
  (x-form 59 frt 0 frb 770 0))

(define-instruction drsp. (frt frb)
  (x-form 59 frt 0 frb 770 1))

;; dfp round to dfp long
(define-instruction drdpq (frtp frbp)
  (x-form 63 frtp 0 frbp 770 0))

(define-instruction drdpq. (frtp frbp)
  (x-form 63 frtp 0 frbp 770 0))

;; dfp convert from fixed
(define-instruction dcffix (frt frb)
  (x-form 59 frt 0 frb 802 0))

(define-instruction dcffix. (frt frb)
  (x-form 59 frt 0 frb 802 1))

;; dfp convert from fixed quad
(define-instruction dcffixq (frtp frb)
  (x-form 63 frtp 0 frb 802 0))

(define-instruction dcffixq. (frtp frb)
  (x-form 63 frtp 0 frb 802 1))

;; dfp convert to fixed [quad]
(define-instruction dctfix (frt frb)
  (x-form 59 frt 0 frb 290 0))

(define-instruction dctfix. (frt frb)
  (x-form 59 frt 0 frb 290 1))

(define-instruction dctfixq (frt frbp)
  (x-form 63 frt 0 frbp 290 0))

(define-instruction dctfixq. (frt frbp)
  (x-form 63 frt 0 frbp 290 1))

;; dfp decode dpd to bcd [quad]
(define-instruction ddedpd (sp frt frb)
  (x-form 59 frt (ash sp 3) frb 322 0))

(define-instruction ddedpd. (sp frt frb)
  (x-form 59 frt (ash sp 3) frb 322 1))

(define-instruction ddedpdq (sp frtp frbp)
  (x-form 63 frtp (ash sp 3) frbp 322 0))

(define-instruction ddedpdq. (sp frtp frbp)
  (x-form 63 frtp (ash sp 3) frbp 322 1))

;; dfp encode bcd to dpd [quad]
(define-instruction denbcd (s frt frb)
  (x-form 59 frt (ash s 3) frb 834 0))

(define-instruction denbcd. (s frt frb)
  (x-form 59 frt (ash s 3) frb 834 1))

(define-instruction denbcdq (s frtp frbp)
  (x-form 63 frtp (ash s 3) frbp 834 0))

(define-instruction denbcdq. (s frtp frbp)
  (x-form 63 frtp (ash s 3) frbp 834 1))

;; dfp extract biased exponent [quad]
(define-instruction dxex (frt frb)
  (x-form 59 frt 0 frb 354 0))

(define-instruction dxex. (frt frb)
  (x-form 59 frt 0 frb 354 1))

(define-instruction dxexq (frt frbp)
  (x-form 63 frt 0 frbp 354 0))

(define-instruction dxexq. (frt frbp)
  (x-form 63 frt 0 frbp 354 1))

;; dfp insert biased exponent [quad]
(define-instruction diex (frt fra frb)
  (x-form 59 frt fra frb 866 0))

(define-instruction diex. (frt fra frb)
  (x-form 59 frt fra frb 866 1))

(define-instruction diexq (frtp fra frbp)
  (x-form 63 frtp fra frbp 866 0))

(define-instruction diexq. (frtp fra frbp)
  (x-form 63 frtp fra frbp 866 1))

;; dfp shift significand left immediate [quad]
(define-instruction dscli (frt fra sh)
  (z22-form 59 frt fra sh 66 0))

(define-instruction dscli. (frt fra sh)
  (z22-form 59 frt fra sh 66 1))

(define-instruction dscliq (frtp frap sh)
  (z22-form 63 frtp frap sh 66 0))

(define-instruction dscliq. (frtp frap sh)
  (z22-form 63 frtp frap sh 66 1))

;; dfp shift significand right immediate [quad]
(define-instruction dscri (frt fra sh)
  (z22-form 59 frt fra sh 98 0))

(define-instruction dscri. (frt fra sh)
  (z22-form 59 frt fra sh 98 1))

(define-instruction dscriq (frtp frap sh)
  (z22-form 63 frtp frap sh 98 0))

(define-instruction dscriq. (frtp frap sh)
  (z22-form 63 frtp frap sh 98 1))

;; load vector element byte indexed
(define-instruction lvebx (vrt ra rb)
  (x-form 31 vrt ra rb 7 0))

;; load vector element halfword indexed
(define-instruction lvehx (vrt ra rb)
  (x-form 31 vrt ra rb 39 0))

;; load vector element word indexed
(define-instruction lvewx (vrt ra rb)
  (x-form 31 vrt ra rb 71 0))

;; load vector indexed
(define-instruction lvx (vrt ra rb)
  (x-form 31 vrt ra rb 103 0))

;; load vector indexed last
(define-instruction lvxl (vrt ra rb)
  (x-form 31 vrt ra rb 359 0))

;; store vector element byte indexed
(define-instruction stvebx (vrs ra rb)
  (x-form 31 vrs ra rb 135 0))

;; store vector element halfword indexed
(define-instruction stvehx (vrs ra rb)
  (x-form 31 vrs ra rb 167 0))

;; store vector element word indexed
(define-instruction stvewx (vrs ra rb)
  (x-form 31 vrs ra rb 199 0))

;; store vector indexed
(define-instruction stvx (vrs ra rb)
  (x-form 31 vrs ra rb 231 0))

;; store vector indexed last
(define-instruction stvxl (vrs ra rb)
  (x-form 31 vrs ra rb 487 0))

;; load vector for shift left indexed
(define-instruction lvsl (vrt ra rb)
  (x-form 31 vrt ra rb 6 0))

;; load vector for shift right indexed
(define-instruction lvsr (vrt ra rb)
  (x-form 31 vrt ra rb 38 0))

;; vector pack pixel
(define-instruction vpkpx (vrt vra vrb)
  (vx-form 4 vrt vra vrb 782))

;; vector pack signed doubleword signed saturate
(define-instruction vpksdss (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1486))

;; vector pack signed doubleword unsigned saturate
(define-instruction vpksdus (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1358))

;; vector pack signed halfword signed saturate
(define-instruction vpkshss (vrt vra vrb)
  (vx-form 4 vrt vra vrb 398))

;; vector pack signed halfword unsigned saturate
(define-instruction vpkshus (vrt vra vrb)
  (vx-form 4 vrt vra vrb 270))

;; vector pack signed word signed saturate
(define-instruction vpkswss (vrt vra vrb)
  (vx-form 4 vrt vra vrb 462))

;; vector pack signed word unsigned saturate
(define-instruction vpkswus (vrt vra vrb)
  (vx-form 4 vrt vra vrb 334))

;; vector pack unsigned doubleword unsigned modulo
(define-instruction vpkudum (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1102))

;; vector pack unsigned doubleword unsigned saturate
(define-instruction vpkudus (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1230))

;; vector pack unsigned halfword unsigned modulo
(define-instruction vpkuhum (vrt vra vrb)
  (vx-form 4 vrt vra vrb 14))

;; vector pack unsigned halfword unsigned saturate
(define-instruction vpkuhus (vrt vra vrb)
  (vx-form 4 vrt vra vrb 142))

;; vector pack unsigned word unsigned modulo
(define-instruction vpkuwum (vrt vra vrb)
  (vx-form 4 vrt vra vrb 78))

;; vector pack unsigned word unsigned saturate
(define-instruction vpkuwus (vrt vra vrb)
  (vx-form 4 vrt vra vrb 206))

;; vector unpack high pixel
(define-instruction vupkhpx (vrt vrb)
  (vx-form 4 vrt 0 vrb 846))

;; vector unpack low pixel
(define-instruction vupklpx (vrt vrb)
  (vx-form 4 vrt 0 vrb 974))

;; vector unpack high signed byte
(define-instruction vupkhsb (vrt vrb)
  (vx-form 4 vrt 0 vrb 526))

;; vector unpack high signed halfword
(define-instruction vupkhsh (vrt vrb)
  (vx-form 4 vrt 0 vrb 590))

;; vector unpack high signed word
(define-instruction vupkhsw (vrt vrb)
  (vx-form 4 vrt 0 vrb 1614))

;; vector unpack low signed byte
(define-instruction vupklsb (vrt vrb)
  (vx-form 4 vrt 0 vrb 654))

;; vector unpack low signed halfword
(define-instruction vupklsh (vrt vrb)
  (vx-form 4 vrt 0 vrb 718))

;; vector unpack low signed word
(define-instruction vupklsw (vrt vrb)
  (vx-form 4 vrt 0 vrb 1742))

;; vector merge high byte
(define-instruction vmrghb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 12))

;; vector merge high halfword
(define-instruction vmrghh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 76))

;; vector merge low byte
(define-instruction vmrglb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 268))

;; vector merge low halfword
(define-instruction vmrglh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 332))

;; vector merge high word
(define-instruction vmrghw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 140))

;; vector merge low word
(define-instruction vmrglw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 396))

;; vector merge even word
(define-instruction vmrgew (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1932))

;; vector merge odd word
(define-instruction vmrgow (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1676))

;; vector splat byte
(define-instruction vspltb (vrt vrb uim)
  (vx-form 4 vrt uim vrb 524))

;; vector splat halfword
(define-instruction vsplth (vrt vrb uim)
  (vx-form 4 vrt uim vrb 588))

;; vector splat word
(define-instruction vspltw (vrt vrb uim)
  (vx-form 4 vrt uim vrb 652))

;; vector splat immediate signed byte
(define-instruction vspltisb (vrt sim)
  (vx-form 4 vrt sim 0 780))

;; vector splat immediate signed halfword
(define-instruction vspltish (vrt sim)
  (vx-form 4 vrt sim 0 844))

;; vector splat immediate signed word
(define-instruction vspltisw (vrt sim)
  (vx-form 4 vrt sim 0 908))

;; vector permute
(define-instruction vperm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 43))

;; vector permute right-indexed
(define-instruction vpermr (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 59))

;; vector select
(define-instruction vsel (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 42))

;; vector shift left double by octet immediate
(define-instruction vsldoi (vrt vra vrb shb)
  (va-form 4 vrt vra vrb shb 44))

;; vector shift left
(define-instruction vsl (vrt vra vrb)
  (vx-form 4 vrt vra vrb 452))

;; vector shift left by octet
(define-instruction vslo (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1036))

;; vector shift right
(define-instruction vsr (vrt vra vrb)
  (vx-form 4 vrt vra vrb 708))

;; vector shift right by octet
(define-instruction vsro (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1100))

;; vector shift left variable
(define-instruction vslv (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1860))

;; vector shift right variable
(define-instruction vsrv (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1796))

;; vector extract unsigned byte
(define-instruction vextractub (vrt vrb uim)
  (vx-form 4 vrt uim vrb 525))

;; vector extract unsigned word
(define-instruction vextractuw (vrt vrb uim)
  (vx-form 4 vrt uim vrb 653))

;; vector extract unsigned halfword
(define-instruction vextractuh (vrt vrb uim)
  (vx-form 4 vrt uim vrb 589))

;; vector extract doubleword
(define-instruction vextractd (vrt vrb uim)
  (vx-form 4 vrt uim vrb 717))

;; vector insert byte
(define-instruction vinsertb (vrt vrb uim)
  (vx-form 4 vrt uim vrb 781))

;; vector insert word
(define-instruction vinsertw (vrt vrb uim)
  (vx-form 4 vrt uim vrb 909))

;; vector insert halfword
(define-instruction vinserth (vrt vrb uim)
  (vx-form 4 vrt uim vrb 845))

;; vector insert doubleword
(define-instruction vinsertd (vrt vrb uim)
  (vx-form 4 vrt uim vrb 973))

;; vector add and write carry-out unsigned word
(define-instruction vaddcuw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 384))

;; vector add signed byte saturate
(define-instruction vaddsbs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 768))

;; vector add signed halfword saturate
(define-instruction vaddshs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 832))

;; vector add signed word saturate
(define-instruction vaddsws (vrt vra vrb)
  (vx-form 4 vrt vra vrb 896))

;; vector add unsigned byte modulo
(define-instruction vaddubm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 0))

;; vector add unsigned doubleword modulo
(define-instruction vaddudm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 192))

;; vector add unsigned halfword modulo
(define-instruction vadduhm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 64))

;; vector add unsigned word modulo
(define-instruction vadduwm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 128))

;; vector add unsigned byte saturate
(define-instruction vaddubs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 512))

;; vector add unsigned halfword saturate
(define-instruction vadduhs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 576))

;; vector add unsigned word saturate
(define-instruction vadduws (vrt vra vrb)
  (vx-form 4 vrt vra vrb 640))

;; vector add unsigned quadword modulo
(define-instruction vadduqm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 256))

;; vector add extended unsigned quadword modulo
(define-instruction vaddeuqm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 60))

;; vector add & write carry unsigned quadword
(define-instruction vaddcuq (vrt vra vrb)
  (vx-form 4 vrt vra vrb 320))

;; vector add extended & write carry unsigned quadword
(define-instruction vaddecuq (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 61))

;; vector subtract and write carry-out unsigned word
(define-instruction vsubcuw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1408))

;; vector subtract signed byte saturate
(define-instruction vsubsbs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1792))

;; vector subtract signed halfword saturate
(define-instruction vsubshs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1856))

;; vector subtract signed word saturate
(define-instruction vsubsws (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1920))

;; vector subtract unsigned byte modulo
(define-instruction vsububm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1024))

;; vector subtract unsigned doubleword modulo
(define-instruction vsubudm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1216))

;; vector subtract unsigned halfword modulo
(define-instruction vsubuhm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1088))

;; vector subtract unsigned word modulo
(define-instruction vsubuwm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1152))

;; vector subtract unsigned byte saturate
(define-instruction vsububs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1536))

;; vector subtract unsigned halfword saturate
(define-instruction vsubuhs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1600))

;; vector subtract unsigned word saturate
(define-instruction vsubuws (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1664))

;; vector subtract unsigned quadword modulo
(define-instruction vsubuqm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1280))

;; vector subtract extended unsigned quadword modulo
(define-instruction vsubeuqm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 62))

;; vector subtract & write carry unsigned quadword
(define-instruction vsubcuq (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1344))

;; vector subtract extended & write carry unsigned quadword
(define-instruction vsubecuq (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 63))

;; vector multiply even signed byte
(define-instruction vmulesb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 776))

;; vector multiply even unsigned byte
(define-instruction vmuleub (vrt vra vrb)
  (vx-form 4 vrt vra vrb 520))

;; vector multiply odd signed byte
(define-instruction vmulosb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 264))

;; vector multiply odd unsigned byte
(define-instruction vmuloub (vrt vra vrb)
  (vx-form 4 vrt vra vrb 8))

;; vector multiply even signed halfword
(define-instruction vmulesh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 840))

;; vector multiply even unsigned halfword
(define-instruction vmuleuh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 584))

;; vector multiply odd signed halfword
(define-instruction vmulosh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 328))

;; vector multiply odd unsigned halfword
(define-instruction vmulouh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 72))

;; vector multiply even signed word
(define-instruction vmulesw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 904))

;; vector multiply even unsigned word
(define-instruction vmuleuw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 648))

;; vector multiply odd signed word
(define-instruction vmulosw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 392))

;; vector multiply odd unsigned word
(define-instruction vmulouw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 136))

;; vector multiply unsigned word modulo
(define-instruction vmuluwm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 137))

;; vector multiply-high-add signed halfword saturate
(define-instruction vmhaddshs (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 32))

;; vector multiply-high-round-add signed halfword saturate
(define-instruction vmhraddshs (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 33))

;; vector multiply-low-add unsigned halfword modulo
(define-instruction vmladduhm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 34))

;; vector multiply-sum unsigned byte modulo
(define-instruction vmsumubm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 36))

;; vector multiply-sum mixed byte modulo
(define-instruction vmsummbm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 37))

;; vector multiply-sum signed halfword modulo
(define-instruction vmsumshm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 40))

;; vector multiply-sum signed halfword saturate
(define-instruction vmsumshs (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 41))

;; vector multiply-sum unsigned halfword modulo
(define-instruction vmsumuhm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 38))

;; vector multiply-sum unsigned halfword saturate
(define-instruction vmsumuhs (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 39))

;; vector multiply-sum unsigned doubleword modulo
(define-instruction vmsumudm (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 35))

;; vector sum across signed word saturate
(define-instruction vsumsws (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1928))

;; vector sum across half signed word saturate
(define-instruction vsum2sws (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1672))

;; vector sum across quarter signed byte saturate
(define-instruction vsum4sbs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1800))

;; vector sum across quarter signed halfword saturate
(define-instruction vsum4shs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1608))

;; vector sum across quarter unsigned byte saturate
(define-instruction vsum4ubs (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1544))

;; vector negate word
(define-instruction vnegw (vrt vrb)
  (vx-form 4 vrt 6 vrb 1538))

;; vector negate doubleword
(define-instruction vnegd (vrt vrb)
  (vx-form 4 vrt 7 vrb 1538))

;; vector extend sign byte to word
(define-instruction vextsb2w (vrt vrb)
  (vx-form 4 vrt 16 vrb 1538))

;; vector extend sign halfword to word
(define-instruction vextsh2w (vrt vrb)
  (vx-form 4 vrt 17 vrb 1538))

;; vector extend sign word to doubleword
(define-instruction vextsw2d (vrt vrb)
  (vx-form 4 vrt 26 vrb 1538))

;; vector extend sign byte to doubleword
(define-instruction vextsb2d (vrt vrb)
  (vx-form 4 vrt 24 vrb 1538))

;; vector extend sign halfword to doubleword
(define-instruction vextsh2d (vrt vrb)
  (vx-form 4 vrt 25 vrb 1538))

;; vector average signed byte
(define-instruction vavgsb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1282))

;; vector average signed halfword
(define-instruction vavgsh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1346))

;; vector average signed word
(define-instruction vavgsw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1410))

;; vector average unsigned byte
(define-instruction vavgub (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1026))

;; vector average unsigned word
(define-instruction vavguw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1154))

;; vector average unsigned halfword
(define-instruction vavguh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1090))

;; vector absolute difference unsigned byte
(define-instruction vabsdub (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1027))

;; vector absolute difference unsigned halfword
(define-instruction vabsduh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1091))

;; vector absolute difference unsigned word
(define-instruction vabsduw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1155))

;; vector maximum signed byte
(define-instruction vmaxsb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 258))

;; vector maximum signed doubleword
(define-instruction vmaxsd (vrt vra vrb)
  (vx-form 4 vrt vra vrb 450))

;; vector maximum unsigned byte
(define-instruction vmaxub (vrt vra vrb)
  (vx-form 4 vrt vra vrb 2))

;; vector maximum unsigned doubleword
(define-instruction vmaxud (vrt vra vrb)
  (vx-form 4 vrt vra vrb 194))

;; vector maximum signed halfword
(define-instruction vmaxsh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 322))

;; vector maximum signed word
(define-instruction vmaxsw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 386))

;; vector maximum unsigned halfword
(define-instruction vmaxuh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 66))

;; vector maximum unsigned word
(define-instruction vmaxuw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 130))

;; vector minimum signed byte
(define-instruction vminsb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 770))

;; vector minimum signed doubleword
(define-instruction vminsd (vrt vra vrb)
  (vx-form 4 vrt vra vrb 962))

;; vector minimum unsigned byte
(define-instruction vminub (vrt vra vrb)
  (vx-form 4 vrt vra vrb 514))

;; vector minimum unsigned doubleword
(define-instruction vminud (vrt vra vrb)
  (vx-form 4 vrt vra vrb 706))

;; vector minimum signed halfword
(define-instruction vminsh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 834))

;; vector minimum signed word
(define-instruction vminsw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 898))

;; vector minimum unsigned halfword
(define-instruction vminuh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 578))

;; vector minimum unsigned word
(define-instruction vminuw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 642))

;; vector compare equal unsigned byte
(define-instruction vcmpequb (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 6))

(define-instruction vcmpequb. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 6))

;; vector compare equal unsigned halfword
(define-instruction vcmpequh (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 70))

(define-instruction vcmpequh. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 70))

;; vector compare equal unsigned word
(define-instruction vcmpequw (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 134))

(define-instruction vcmpequw. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 134))

;; vector compare equal unsigned doubleword
(define-instruction vcmpequd (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 199))

(define-instruction vcmpequd. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 199))

;; vector compare greater than signed byte
(define-instruction vcmpgtsb (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 774))

(define-instruction vcmpgtsb. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 774))

;; vector compare greater than signed doubleword
(define-instruction vcmpgtsd (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 967))

(define-instruction vcmpgtsd. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 967))

;; vector compare greater than signed halfword
(define-instruction vcmpgtsh (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 838))

(define-instruction vcmpgtsh. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 838))

;; vector compare greater than signed word
(define-instruction vcmpgtsw (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 902))

(define-instruction vcmpgtsw. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 902))

;; vector compare greater than unsigned byte
(define-instruction vcmpgtub (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 518))

(define-instruction vcmpgtub. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 518))

;; vector compare greater than unsigned doubleword
(define-instruction vcmpgtud (vrt vra vrb)
  (vx-form 4 vrt vra vrb (logior (ash 0 10) 711)))

(define-instruction vcmpgtud. (vrt vra vrb)
  (vx-form 4 vrt vra vrb (logior (ash 1 10) 711)))

;; vector compare greater than unsigned halfword
(define-instruction vcmpgtuh (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 582))

(define-instruction vcmpgtuh. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 582))

;; vector compare greater than unsigned word
(define-instruction vcmpgtuw (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 646))

(define-instruction vcmpgtuw. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 646))

;; vector compare not equal byte
(define-instruction vcmpneb (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 7))

(define-instruction vcmpneb. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 7))

;; vector compare not equal or zero byte
(define-instruction vcmpnezb (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 263))

(define-instruction vcmpnezb. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 263))

;; vector compare not equal halfword
(define-instruction vcmpneh (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 71))

(define-instruction vcmpneh. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 71))

;; vector compare not equal or zero halfword
(define-instruction vcmpnezh (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 327))

(define-instruction vcmpnezh. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 327))

;; vector compare not equal word
(define-instruction vcmpnew (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 135))

(define-instruction vcmpnew. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 135))

;; vector compare not equal or zero word
(define-instruction vcmpnezw (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 391))

(define-instruction vcmpnezw. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 391))

;; vector logical and
(define-instruction vand (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1028))

;; vector logical and with complement
(define-instruction vandc (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1092))

;; vector logical equivalence
(define-instruction veqv (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1668))

;; vector logical nand
(define-instruction vnand (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1412))

;; vector logical or with complement
(define-instruction vorc (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1348))

;; vector logical nor
(define-instruction vnor (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1284))

;; vector logical or
(define-instruction vor (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1156))

;; vector logical xor
(define-instruction vxor (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1220))

;; vector parity byte word
(define-instruction vprtybw (vrt vrb)
  (vx-form 4 vrt 8 vrb 1538))

;; vector parity byte doubleword
(define-instruction vprtybd (vrt vrb)
  (vx-form 4 vrt 9 vrb 1538))

;; vector parity byte quadword
(define-instruction vprtybq (vrt vrb)
  (vx-form 4 vrt 10 vrb 1538))

;; vector rotate left byte
(define-instruction vrlb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 4))

;; vector rotate left halfword
(define-instruction vrlh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 68))

;; vector rotate left word
(define-instruction vrlw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 132))

;; vector rotate left doubleword
(define-instruction vrld (vrt vra vrb)
  (vx-form 4 vrt vra vrb 196))

;; vector shift left byte
(define-instruction vslb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 260))

;; vector shift left halfword
(define-instruction vslh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 324))

;; vector shift left word
(define-instruction vslw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 388))

;; vector shift left doubleword
(define-instruction vsld (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1476))

;; vector shift right byte
(define-instruction vsrb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 516))

;; vector shift right halfword
(define-instruction vsrh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 580))

;; vector shift right word
(define-instruction vsrw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 644))

;; vector shift right doubleword
(define-instruction vsrd (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1732))

;; vector shift right algebraic byte
(define-instruction vsrab (vrt vra vrb)
  (vx-form 4 vrt vra vrb 772))

;; vector shift right algebraic halfword
(define-instruction vsrah (vrt vra vrb)
  (vx-form 4 vrt vra vrb 836))

;; vector shift right algebraic word
(define-instruction vsraw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 900))

;; vector shift right algebraic doubleword
(define-instruction vsrad (vrt vra vrb)
  (vx-form 4 vrt vra vrb 964))

;; vector rotate left word then and with mask
(define-instruction vrlwnm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 389))

;; vector rotate left word then mask insert
(define-instruction vrlwmi (vrt vra vrb)
  (vx-form 4 vrt vra vrb 133))

;; vector rotate left doubleword then and with mask
(define-instruction vrldnm (vrt vra vrb)
  (vx-form 4 vrt vra vrb 453))

;; vector rotate left doubleword then mask insert
(define-instruction vrldmi (vrt vra vrb)
  (vx-form 4 vrt vra vrb 197))

;; vector add floating-point
(define-instruction vaddfp (vrt vra vrb)
  (vx-form 4 vrt vra vrb 10))

;; vector subtract floating-point
(define-instruction vsubfp (vrt vra vrb)
  (vx-form 4 vrt vra vrb 74))

;; vector multiply-add floating-point
(define-instruction vmaddfp (vrt vra vrc vrb)
  (va-form 4 vrt vra vrc vrb 46))

;; vector negative multiply-subtract floating-point
(define-instruction vnmsubfp (vrt vra vrc vrb)
  (va-form 4 vrt vra vrc vrb 47))

;; vector maximum floating-point
(define-instruction vmaxfp (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1034))

;; vector minimum floating-point
(define-instruction vminfp (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1098))

;; vector convert with truncate floating-point to signed word format saturate
(define-instruction vctsxs (vrt vrb uim)
  (vx-form 4 vrt uim vrb 970))

;; vector convert with truncate floating-point to unsigned word format saturate
(define-instruction vctuxs (vrt vrb uim)
  (vx-form 4 vrt uim vrb 906))

;; vector convert with round to nearest signed word format
(define-instruction vcfsx (vrt vrb uim)
  (vx-form 4 vrt uim vrb 842))

;; vector convert with round to nearest unsigned word format
(define-instruction vcfux (vrt vrb uim)
  (vx-form 4 vrt uim vrb 778))

;; vector round to floating-point integer toward -infinity
(define-instruction vrfim (vrt vrb)
  (vx-form 4 vrt 0 vrb 714))

;; vector round to floating-point integer nearest
(define-instruction vrfin (vrt vrb)
  (vx-form 4 vrt 0 vrb 522))

;; vector round to floating-point integer toward +infinity
(define-instruction vrfip (vrt vrb)
  (vx-form 4 vrt 0 vrb 650))

;; vector round to floating-point integer toward zero
(define-instruction vrfiz (vrt vrb)
  (vx-form 4 vrt 0 vrb 586))

;; vector compare bounds floating-point
(define-instruction vcmpbfp (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 966))

(define-instruction vcmpbfp. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 966))

;; vector compare equal floating-point
(define-instruction vcmpeqfp (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 198))

(define-instruction vcmpeqfp. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 198))

;; vector compare greater than or equal floating-point
(define-instruction vcmpgefp (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 454))

(define-instruction vcmpgefp. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 454))

;; vector compare greater than floating-point
(define-instruction vcmpgtfp (vrt vra vrb)
  (vc-form 4 vrt vra vrb 0 710))

(define-instruction vcmpgtfp. (vrt vra vrb)
  (vc-form 4 vrt vra vrb 1 710))

;; vector 2 raised to the exponent estimate floating-point
(define-instruction vexptefp (vrt vrb)
  (vx-form 4 vrt 0 vrb 394))

;; vector log base 2 estimate floating-point
(define-instruction vlogefp (vrt vrb)
  (vx-form 4 vrt 0 vrb 458))

;; vector reciprocal estimate floating-point
(define-instruction vrefp (vrt vrb)
  (vx-form 4 vrt 0 vrb 266))

;; vector reciprocal square root estimate floating-point
(define-instruction vrsqrtefp (vrt vrb)
  (vx-form 4 vrt 0 vrb 330))

;; vector aes cipher
(define-instruction vcipher (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1288))

;; vector aes cipher last
(define-instruction vcipherlast (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1289))

;; vector aes inverse cipher
(define-instruction vncipher (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1352))

;; vector aes inverse cipher last
(define-instruction vncipherlast (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1353))

;; vector aes subbytes
(define-instruction vsbox (vrt vra)
  (vx-form 4 vrt vra 0 1480))

;; vector sha-512 sigma doubleword
(define-instruction vshasigmad (vrt vra st six)
  (vx-form 4 vrt vra (logior (ash st 4) six) 1730))

;; vector sha-256 sigma word
(define-instruction vshasigmaw (vrt vra st six)
  (vx-form 4 vrt vra (logior (ash st 4) six) 1666))

;; vector polynomial multiply-sum byte
(define-instruction vpmsumb (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1032))

;; vector polynomial multiply-sum doubleword
(define-instruction vpmsumd (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1224))

;; vector polynomial multiply-sum halfword
(define-instruction vpmsumh (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1096))

;; vector polynomial multiply-sum word
(define-instruction vpmsumw (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1160))

;; vector permute and exclusive-or
(define-instruction vpermxor (vrt vra vrb vrc)
  (va-form 4 vrt vra vrb vrc 45))

;; vector gather bits by bytes by doubleword
(define-instruction vgbbd (vrt vrb)
  (vx-form 4 vrt 0 vrb 1292))

;; vector count leading zeros byte
(define-instruction vclzb (vrt vrb)
  (vx-form 4 vrt 0 vrb 1794))

;; vector count leading zeros halfword
(define-instruction vclzh (vrt vrb)
  (vx-form 4 vrt 0 vrb 1858))

;; vector count leading zeros word
(define-instruction vclzw (vrt vrb)
  (vx-form 4 vrt 0 vrb 1922))

;; vector count leading zeros doubleword
(define-instruction vclzd (vrt vrb)
  (vx-form 4 vrt 0 vrb 1986))

;; vector count trailing zeros byte
(define-instruction vctzb (vrt vrb)
  (vx-form 4 vrt 28 vrb 1538))

;; vector count trailing zeros halfword
(define-instruction vctzh (vrt vrb)
  (vx-form 4 vrt 29 vrb 1538))

;; vector count trailing zeros word
(define-instruction vctzw (vrt vrb)
  (vx-form 4 vrt 30 vrb 1538))

;; vector count trailing zeros doubleword
(define-instruction vctzd (vrt vrb)
  (vx-form 4 vrt 31 vrb 1538))

;; vector count leading zero least-significant bits byte
(define-instruction vclzlsbb (rt vrb)
  (vx-form 4 rt 0 vrb 1538))

;; vector count trailing zero least-significant bits byte
(define-instruction vctzlsbb (rt vrb)
  (vx-form 4 rt 1 vrb 1538))

;; vector extract unsigned byte left-indexed
(define-instruction vextublx (rt ra vrb)
  (vx-form 4 rt ra vrb 1549))

;; vector extract unsigned halfword left-indexed
(define-instruction vextuhlx (rt ra vrb)
  (vx-form 4 rt ra vrb 1613))

;; vector extract unsigned byte right-indexed
(define-instruction vextubrx (rt ra vrb)
  (vx-form 4 rt ra vrb 1805))

;; vector extract unsigned halfword right-indexed
(define-instruction vextuhrx (rt ra vrb)
  (vx-form 4 rt ra vrb 1869))

;; vector extract unsigned word left-indexed
(define-instruction vextuwlx (rt ra vrb)
  (vx-form 4 rt ra vrb 1677))

;; vector extract unsigned word right-indexed
(define-instruction vextuwrx (rt ra vrb)
  (vx-form 4 rt ra vrb 1933))

;; vector population count byte
(define-instruction vpopcntb (vrt vrb)
  (vx-form 4 vrt 0 vrb 1795))

;; vector population count doubleword
(define-instruction vpopcntd (vrt vrb)
  (vx-form 4 vrt 0 vrb 1987))

;; vector population count halfword
(define-instruction vpopcnth (vrt vrb)
  (vx-form 4 vrt 0 vrb 1859))

;; vector population count word
(define-instruction vpopcntw (vrt vrb)
  (vx-form 4 vrt 0 vrb 1923))

;; vector bit permute doubleword
(define-instruction vbpermd (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1484))

;; vector bit permute quadword
(define-instruction vbpermq (vrt vra vrb)
  (vx-form 4 vrt vra vrb 1356))

;; decimal add modulo
(define-instruction bcdadd. (vrt vra vrb ps)
  (vx-form 4 vrt vra vrb (logior (ash 1 9) (ash ps 8) 1)))

;; decimal subtract modulo
(define-instruction bcdsub. (vrt vra vrb ps)
  (vx-form 4 vrt vra vrb (logior (ash 1 9) (ash ps 8) 65)))

;; decimal convert from national
(define-instruction bcdcfn. (vrt vrb ps)
  (vx-form 4 vrt 7 vrb (logior (ash 1 9) (ash ps 8) 385)))

;; decimal convert from zoned
(define-instruction bcdcfz. (vrt vrb ps)
  (vx-form 4 vrt 6 vrb (logior (ash 1 9) (ash ps 8) 385)))

;; decimal convert to national
(define-instruction bcdctn. (vrt vrb)
  (vx-form 4 vrt 5 vrb (logior (ash 1 9) 385)))

;; decimal convert to zoned
(define-instruction bcdctz. (vrt vrb ps)
  (vx-form 4 vrt 4 vrb (logior (ash 1 9) (ash ps 8) 385)))

;; decimal convert from signed quadword
(define-instruction bcdcfsq. (vrt vrb ps)
  (vx-form 4 vrt 2 vrb (logior (ash 1 9) (ash ps 8) 385)))

;; decimal convert to signed quadword
(define-instruction bcdctsq. (vrt vrb)
  (vx-form 4 vrt 0 vrb (logior (ash 1 9) 385)))

;; vector multiply-by-10 unsigned quadword
(define-instruction vmul10uq (vrt vra)
  (vx-form 4 vrt vra 0 513))

;; vector multiply-by-10 & write carry unsigned quadword
(define-instruction vmul10cuq (vrt vra)
  (vx-form 4 vrt vra 0 1))

;; vector multiply-by-10 extended unsigned quadword
(define-instruction vmul10euq (vrt vra vrb)
  (vx-form 4 vrt vra vrb 577))

;; vector multiply-by-10 extended & write carry unsigned quadword
(define-instruction vmul10ecuq (vrt vra vrb)
  (vx-form 4 vrt vra vrb 65))

;; decimal copy sign
(define-instruction bcdcpsgn. (vrt vra vrb)
  (vx-form 4 vrt vra vrb 833))

;; decimal set sign
(define-instruction bcdsetsgn. (vrt vrb ps)
  (vx-form 4 vrt 31 vrb (logior (ash 1 9) (ash ps 8) 385)))

;; decimal shift
(define-instruction bcds. (vrt vra vrb ps)
  (vx-form 4 vrt vra vrb (logior (ash 1 9) (ash ps 8) 193)))

;; decimal unsigned shift
(define-instruction bcdus. (vrt vra vrb)
  (vx-form 4 vrt vra vrb (logior (ash 1 9) 129)))

;; decimal shift and round
(define-instruction bcdsr. (vrt vra vrb ps)
  (vx-form 4 vrt vra vrb (logior (ash 1 9) (ash ps 8) 449)))

;; decimal truncate
(define-instruction bcdtrunc. (vrt vra vrb ps)
  (vx-form 4 vrt vra vrb (logior (ash 1 9) (ash ps 8) 257)))

;; decimal unsigned truncate
(define-instruction bcdutrunc. (vrt vra vrb)
  (vx-form 4 vrt vra vrb (logior (ash 1 9) 321)))

;; move to vector status and control register
(define-instruction mtvscr (vrb)
  (vx-form 4 0 0 vrb 1604))

;; move from vector status and control register
(define-instruction mfvscr (vrt)
  (vx-form 4 vrt 0 0 1540))
