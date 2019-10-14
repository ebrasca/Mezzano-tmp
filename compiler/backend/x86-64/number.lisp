;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.x86-64)

;;; FIXNUM/INTEGER operations.

(define-builtin sys.int::fixnump ((object) :z)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:test64
                       :operands (list object sys.int::+fixnum-tag-mask+)
                       :inputs (list object)
                       :outputs '())))

(define-builtin mezzano.runtime::%fixnum-+ ((lhs rhs) result)
  (let ((out (make-instance 'ir:label :phis (list result)))
        (no-overflow (make-instance 'ir:label :name :+-no-overflow))
        (overflow (make-instance 'ir:label :name :+-overflow))
        (fixnum-result (make-instance 'ir:virtual-register))
        (bignum-result (make-instance 'ir:virtual-register)))
    (cond ((constant-value-p rhs '(eql 0))
           (emit (make-instance 'ir:move-instruction
                                :source lhs
                                :destination result))
           (finish))
          ((constant-value-p lhs '(eql 0))
           (emit (make-instance 'ir:move-instruction
                                :source rhs
                                :destination result))
           (finish))
          ((constant-value-p rhs '(signed-byte 31))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:add64
                                :result fixnum-result
                                :lhs lhs
                                :rhs (ash (fetch-constant-value rhs)
                                          sys.int::+n-fixnum-bits+))))
          (t
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:add64
                                :result fixnum-result
                                :lhs lhs
                                :rhs rhs))))
    (emit (make-instance 'x86-branch-instruction
                         :opcode 'lap:jo
                         :true-target overflow
                         :false-target no-overflow))
    (emit no-overflow)
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list fixnum-result)))
    ;; Build a bignum on overflow.
    ;; Recover the full value using the carry bit.
    (emit overflow)
    (emit (make-instance 'ir:move-instruction
                         :source fixnum-result
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:rcr64
                         :operands (list :rax 1)
                         :inputs (list :rax)
                         :outputs (list :rax)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :r13 '(:function sys.int::%%make-bignum-64-rax))
                         :inputs '()
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:call
                         :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                         :inputs '(:r13 :rax)
                         :outputs (list :r8)
                         :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                     :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                     :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                     :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
    (emit (make-instance 'ir:move-instruction
                         :destination bignum-result
                         :source :r8))
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list bignum-result)))
    (emit out)))

(define-builtin mezzano.compiler::%fast-fixnum-+ ((lhs rhs) result)
  (cond ((constant-value-p rhs '(eql 0))
         (emit (make-instance 'ir:move-instruction
                              :source lhs
                              :destination result))
         (finish))
        ((constant-value-p lhs '(eql 0))
         (emit (make-instance 'ir:move-instruction
                              :source rhs
                              :destination result))
         (finish))
        ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:add64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:add64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin mezzano.runtime::%fixnum-- ((lhs rhs) result)
  (let ((out (make-instance 'ir:label :phis (list result)))
        (no-overflow (make-instance 'ir:label :name :--no-overflow))
        (overflow (make-instance 'ir:label :name :--overflow))
        (fixnum-result (make-instance 'ir:virtual-register))
        (bignum-result (make-instance 'ir:virtual-register)))
    (cond ((constant-value-p rhs '(eql 0))
           (emit (make-instance 'ir:move-instruction
                                :source lhs
                                :destination result))
           (finish))
          ((constant-value-p rhs '(signed-byte 31))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sub64
                                :result fixnum-result
                                :lhs lhs
                                :rhs (ash (fetch-constant-value rhs)
                                          sys.int::+n-fixnum-bits+))))
          (t
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sub64
                                :result fixnum-result
                                :lhs lhs
                                :rhs rhs))))
    (emit (make-instance 'x86-branch-instruction
                         :opcode 'lap:jo
                         :true-target overflow
                         :false-target no-overflow))
    (emit no-overflow)
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list fixnum-result)))
    ;; Build a bignum on overflow.
    ;; Recover the full value using the carry bit.
    (emit overflow)
    (emit (make-instance 'ir:move-instruction
                         :source fixnum-result
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmc
                         :operands (list)
                         :inputs (list)
                         :outputs (list)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:rcr64
                         :operands (list :rax 1)
                         :inputs (list :rax)
                         :outputs (list :rax)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :r13 '(:function sys.int::%%make-bignum-64-rax))
                         :inputs '()
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:call
                         :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                         :inputs '(:r13 :rax)
                         :outputs (list :r8)
                         :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                     :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                     :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                     :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
    (emit (make-instance 'ir:move-instruction
                         :destination bignum-result
                         :source :r8))
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list bignum-result)))
    (emit out)))

(define-builtin mezzano.compiler::%fast-fixnum-- ((lhs rhs) result)
  (cond ((constant-value-p rhs '(eql 0))
           (emit (make-instance 'ir:move-instruction
                                :source lhs
                                :destination result))
         (finish))
        ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:sub64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:sub64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin mezzano.runtime::%fixnum-* ((lhs rhs) result)
  (cond ((or (constant-value-p lhs '(eql 0))
             (constant-value-p rhs '(eql 0)))
         (emit (make-instance 'ir:constant-instruction
                              :value 0
                              :destination result))
         (finish))
        ((constant-value-p rhs '(eql 1))
         (emit (make-instance 'ir:move-instruction
                              :source lhs
                              :destination result))
         (finish))
        ((constant-value-p lhs '(eql 1))
         (emit (make-instance 'ir:move-instruction
                              :source rhs
                              :destination result))
         (finish)))
  (let ((out (make-instance 'ir:label :phis (list result)))
        (low-half (make-instance 'ir:virtual-register :kind :integer))
        (high-half (make-instance 'ir:virtual-register :kind :integer))
        (no-overflow (make-instance 'ir:label :name :*-no-overflow))
        (overflow (make-instance 'ir:label :name :*-overflow))
        (overflow-temp (make-instance 'ir:virtual-register :kind :integer))
        (fixnum-result (make-instance 'ir:virtual-register))
        (bignum-result (make-instance 'ir:virtual-register))
        (lhs-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    ;; Convert the lhs to a raw integer, leaving the rhs as a fixnum.
    ;; This will cause the result to be a fixnum.
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source lhs
                         :destination lhs-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source lhs-unboxed
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:imul64
                         :operands (list rhs)
                         :inputs (list :rax rhs)
                         :outputs (list :rax :rdx)))
    ;; Avoid keeping rax/rdx live over a branch.
    (emit (make-instance 'ir:move-instruction
                         :destination low-half
                         :source :rax))
    (emit (make-instance 'ir:move-instruction
                         :destination high-half
                         :source :rdx))
    (emit (make-instance 'x86-branch-instruction
                         :opcode 'lap:jo
                         :true-target overflow
                         :false-target no-overflow))
    (emit no-overflow)
    (emit (make-instance 'ir:move-instruction
                         :source low-half
                         :destination fixnum-result))
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list fixnum-result)))
    ;; Build a bignum on overflow.
    ;; 128-bit result in rdx:rax.
    (emit overflow)
    ;; Unbox the result.
    (emit (make-instance 'ir:move-instruction
                         :destination :rax
                         :source low-half))
    (emit (make-instance 'ir:move-instruction
                         :destination :rdx
                         :source high-half))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:shrd64
                         :operands (list :rax :rdx sys.int::+n-fixnum-bits+)
                         :inputs (list :rax :rdx)
                         :outputs (list :rax)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sar64
                         :operands (list :rdx sys.int::+n-fixnum-bits+)
                         :inputs (list :rdx)
                         :outputs (list :rdx)))
    ;; Check if the result will fit in 64 bits.
    ;; Save the high bits.
    (emit (make-instance 'ir:move-instruction
                         :destination overflow-temp
                         :source :rdx))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cqo
                         :operands (list)
                         :inputs (list :rax)
                         :outputs (list :rdx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmp64
                         :operands (list overflow-temp :rdx)
                         :inputs (list overflow-temp :rdx)
                         :outputs (list)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :rdx overflow-temp)
                         :inputs (list overflow-temp)
                         :outputs (list :rdx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :r13 '(:function sys.int::%%make-bignum-128-rdx-rax))
                         :inputs (list)
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmov64e
                         :operands (list :r13 '(:function sys.int::%%make-bignum-64-rax))
                         :inputs (list :r13)
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:call
                         :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                         :inputs (list :r13 :rax :rdx)
                         :outputs (list :r8)
                         :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                     :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                     :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                     :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
    (emit (make-instance 'ir:move-instruction
                         :destination bignum-result
                         :source :r8))
    (emit (make-instance 'ir:jump-instruction
                         :target out
                         :values (list bignum-result)))
    (emit out)))

(define-builtin mezzano.compiler::%fast-fixnum-* ((lhs rhs) result)
  (cond ((or (constant-value-p lhs '(eql 0))
             (constant-value-p rhs '(eql 0)))
         (emit (make-instance 'ir:constant-instruction
                              :value 0
                              :destination result))
         (finish))
        ((constant-value-p rhs '(eql 1))
         (emit (make-instance 'ir:move-instruction
                              :source lhs
                              :destination result))
         (finish))
        ((constant-value-p lhs '(eql 1))
         (emit (make-instance 'ir:move-instruction
                              :source rhs
                              :destination result))
         (finish)))
  ;; Convert the lhs to a raw integer, leaving the rhs as a fixnum.
  ;; This will cause the result to be a fixnum.
  (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source lhs
                         :destination lhs-unboxed))
    (emit (make-instance 'ir:move-instruction
                         :source lhs-unboxed
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:imul64
                         :operands (list rhs)
                         :inputs (list :rax rhs)
                         :outputs (list :rax :rdx)))
    (emit (make-instance 'ir:move-instruction
                         :destination result
                         :source :rax))))

(define-builtin mezzano.runtime::%fixnum-truncate ((lhs rhs) (quot rem))
  (let ((quot-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:move-instruction
                         :source lhs
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cqo
                         :operands (list)
                         :inputs (list :rax)
                         :outputs (list :rdx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:idiv64
                         :operands (list rhs)
                         :inputs (list :rax :rdx rhs)
                         :outputs (list :rax :rdx)))
    ;; :rax holds the dividend as a integer.
    ;; :rdx holds the remainder as a fixnum.
    (emit (make-instance 'ir:move-instruction
                         :source :rax
                         :destination quot-unboxed))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source quot-unboxed
                         :destination quot))
    (emit (make-instance 'ir:move-instruction
                         :source :rdx
                         :destination rem))))

(define-builtin mezzano.runtime::%fixnum-logand ((lhs rhs) result)
  (cond ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:and64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:and64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin sys.c::%fast-fixnum-logand ((lhs rhs) result)
  (cond ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:and64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:and64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin mezzano.runtime::%fixnum-logior ((lhs rhs) result)
  (cond ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:or64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:or64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin sys.c::%fast-fixnum-logior ((lhs rhs) result)
  (cond ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:or64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:or64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin mezzano.runtime::%fixnum-logxor ((lhs rhs) result)
  (cond ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:xor64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:xor64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin sys.c::%fast-fixnum-logxor ((lhs rhs) result)
  (cond ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:xor64
                              :result result
                              :lhs lhs
                              :rhs (ash (fetch-constant-value rhs)
                                        sys.int::+n-fixnum-bits+))))
        (t
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:xor64
                              :result result
                              :lhs lhs
                              :rhs rhs)))))

(define-builtin mezzano.runtime::%fixnum-< ((lhs rhs) :l)
  (cond ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs (ash (fetch-constant-value rhs)
                                                       sys.int::+n-fixnum-bits+))
                              :inputs (list lhs)
                              :outputs '())))
        (t
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs rhs)
                              :inputs (list lhs rhs)
                              :outputs '())))))

(define-builtin mezzano.runtime::%fixnum-right-shift ((integer count) result)
  (cond ((constant-value-p count '(integer 0))
         (let ((count-value (fetch-constant-value count)))
           (cond ((>= count-value (- 64 sys.int::+n-fixnum-bits+))
                  ;; All bits shifted out.
                  ;; Turn INTEGER into 0 or -1.
                  (emit (make-instance 'ir:move-instruction
                                       :destination :rax
                                       :source integer))
                  (emit (make-instance 'x86-instruction
                                       :opcode 'lap:cqo
                                       :operands '()
                                       :inputs '(:rax)
                                       :outputs '(:rdx)))
                  (emit (make-instance 'x86-instruction
                                       :opcode 'lap:and64
                                       :operands `(:rdx ,(sys.c::fixnum-to-raw -1))
                                       :inputs '(:rdx)
                                       :outputs '(:rdx)))
                  (emit (make-instance 'ir:move-instruction
                                       :destination result
                                       :source :rdx)))
                 ((zerop count-value)
                  (emit (make-instance 'ir:move-instruction
                                       :destination result
                                       :source integer)))
                 (t
                  (let ((temp1 (make-instance 'ir:virtual-register :kind :integer))
                        (temp2 (make-instance 'ir:virtual-register :kind :integer)))
                    (emit (make-instance 'x86-fake-three-operand-instruction
                                         :opcode 'lap:sar64
                                         :result temp1
                                         :lhs integer
                                         :rhs count-value))
                    (emit (make-instance 'x86-fake-three-operand-instruction
                                         :opcode 'lap:and64
                                         :result temp2
                                         :lhs temp1
                                         :rhs (sys.c::fixnum-to-raw -1)))
                  (emit (make-instance 'ir:move-instruction
                                       :destination result
                                       :source temp2)))))))
        (t
         (let ((done (make-instance 'ir:label
                                    :name :right-shift-done
                                    :phis (list result)))
               (sign-extend (make-instance 'ir:label :name :right-shift-sign-extend))
               (do-shift (make-instance 'ir:label :name :right-shift-do-shift))
               (count-unboxed (make-instance 'ir:virtual-register :kind :integer))
               (temp0 (make-instance 'ir:virtual-register :kind :integer))
               (temp1 (make-instance 'ir:virtual-register :kind :integer))
               (temp2 (make-instance 'ir:virtual-register :kind :integer))
               (temp3 (make-instance 'ir:virtual-register :kind :integer)))
           (emit (make-instance 'ir:move-instruction
                                :destination temp0
                                :source integer))
           ;; x86 masks the shift count to 6 bits, test if all the bits were shifted out.
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:cmp64
                                :operands (list count (sys.c::fixnum-to-raw 64))
                                :inputs (list count)
                                :outputs (list)))
           (emit (make-instance 'x86-branch-instruction
                                :opcode 'lap:jae
                                :true-target sign-extend
                                :false-target do-shift))
           (emit do-shift)
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :source count
                                :destination count-unboxed))
           (emit (make-instance 'ir:move-instruction
                                :source count-unboxed
                                :destination :rcx))
           ;; Do the shift.
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sar64
                                :result temp1
                                :lhs temp0
                                :rhs :cl))
           ;; Make sure the result really is a fixnum.
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:and64
                                :result temp2
                                :lhs temp1
                                :rhs (sys.c::fixnum-to-raw -1)))
           ;; Done
           (emit (make-instance 'ir:jump-instruction
                                :target done
                                :values (list temp2)))
           (emit sign-extend)
           (emit (make-instance 'ir:move-instruction
                                :destination :rax
                                :source temp0))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:cqo
                                :operands '()
                                :inputs '(:rax)
                                :outputs '(:rdx)))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:and64
                                :operands `(:rdx ,(sys.c::fixnum-to-raw -1))
                                :inputs '(:rdx)
                                :outputs '(:rdx)))
           (emit (make-instance 'ir:move-instruction
                                :destination temp3
                                :source :rdx))
           (emit (make-instance 'ir:jump-instruction
                                :target done
                                :values (list temp3)))
           (emit done)))))

(define-builtin mezzano.compiler::%fast-fixnum-left-shift ((integer count) result)
  (cond ((constant-value-p count '(integer 0))
         (let ((count-value (fetch-constant-value count)))
           (cond ((>= count-value (- 64 sys.int::+n-fixnum-bits+))
                  ;; All bits shifted out.
                  ;; Turn INTEGER into 0.
                  (emit (make-instance 'ir:constant-instruction
                                       :destination result
                                       :value 0)))
                 ((zerop count-value)
                  (emit (make-instance 'ir:move-instruction
                                       :destination result
                                       :source integer)))
                 (t
                  (emit (make-instance 'x86-fake-three-operand-instruction
                                       :opcode 'lap:shl64
                                       :result result
                                       :lhs integer
                                       :rhs count-value))))))
        (t
         ;; No need to test for overlong shift counts here. The result must fit
         ;; in a fixnum, so obviously the count is within range or the input
         ;; is 0.
         (let ((count-value (make-instance 'ir:virtual-register :kind :integer)))
           (emit (make-instance 'ir:unbox-fixnum-instruction
                                :source count
                                :destination count-value))
           (emit (make-instance 'ir:move-instruction
                                :source count-value
                                :destination :rcx))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:shl64
                                :result result
                                :lhs integer
                                :rhs :rcx))))))

;;; SINGLE-FLOAT operations.

(define-builtin sys.int::%single-float-as-integer ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-single-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin sys.int::%integer-as-single-float ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-single-float-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-fixnum-to-single-float ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (result-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtsi2ss64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-double-float-to-single-float ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :double-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
    (emit (make-instance 'ir:unbox-double-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtsd2ss64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%%single-float-< ((lhs rhs) :b)
  (cond ((constant-value-p rhs 'single-float)
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
           (emit (make-instance 'ir:unbox-single-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomiss
                                :operands (list lhs-unboxed `(:literal ,(sys.int::%single-float-as-integer (fetch-constant-value rhs))))
                                :inputs (list lhs-unboxed)
                                :outputs '()))))
        (t
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :single-float))
               (rhs-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
           (emit (make-instance 'ir:unbox-single-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'ir:unbox-single-float-instruction
                                :source rhs
                                :destination rhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomiss
                                :operands (list lhs-unboxed rhs-unboxed)
                                :inputs (list lhs-unboxed rhs-unboxed)
                                :outputs '()))))))

;; TODO: This needs to check two conditions (P & NE), which the
;; compiler can't currently do efficiently.
(define-builtin sys.int::%%single-float-= ((lhs rhs) result)
  (cond ((constant-value-p rhs 'single-float)
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :single-float))
               (temp-result1 (make-instance 'ir:virtual-register))
               (temp-result2 (make-instance 'ir:virtual-register)))
           (emit (make-instance 'ir:unbox-single-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomiss
                                :operands (list lhs-unboxed `(:literal ,(sys.int::%single-float-as-integer (fetch-constant-value rhs))))
                                :inputs (list lhs-unboxed)
                                :outputs '()))
           (emit (make-instance 'ir:constant-instruction
                                :destination temp-result1
                                :value t))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64p
                                :result temp-result2
                                :lhs temp-result1
                                :rhs `(:constant nil)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64ne
                                :result result
                                :lhs temp-result2
                                :rhs `(:constant nil)))))
        (t
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :single-float))
               (rhs-unboxed (make-instance 'ir:virtual-register :kind :single-float))
               (temp-result1 (make-instance 'ir:virtual-register))
               (temp-result2 (make-instance 'ir:virtual-register)))
           (emit (make-instance 'ir:unbox-single-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'ir:unbox-single-float-instruction
                                :source rhs
                                :destination rhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomiss
                                :operands (list lhs-unboxed rhs-unboxed)
                                :inputs (list lhs-unboxed rhs-unboxed)
                                :outputs '()))
           (emit (make-instance 'ir:constant-instruction
                                :destination temp-result1
                                :value t))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64p
                                :result temp-result2
                                :lhs temp-result1
                                :rhs `(:constant nil)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64ne
                                :result result
                                :lhs temp-result2
                                :rhs `(:constant nil)))))))

(define-builtin sys.int::%%truncate-single-float ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :single-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-single-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvttss2si64
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(macrolet ((frob (name instruction)
             `(define-builtin ,name ((lhs rhs) result)
                (cond ((constant-value-p rhs 'single-float)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :single-float))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
                         (emit (make-instance 'ir:unbox-single-float-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',instruction
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal ,(sys.int::%single-float-as-integer (fetch-constant-value rhs)))))
                         (emit (make-instance 'ir:box-single-float-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :single-float))
                             (rhs-unboxed (make-instance 'ir:virtual-register :kind :single-float))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
                         (emit (make-instance 'ir:unbox-single-float-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'ir:unbox-single-float-instruction
                                              :source rhs
                                              :destination rhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',instruction
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs rhs-unboxed))
                         (emit (make-instance 'ir:box-single-float-instruction
                                              :source result-unboxed
                                              :destination result))))))))
  (frob sys.int::%%single-float-/ lap:divss)
  (frob sys.int::%%single-float-+ lap:addss)
  (frob sys.int::%%single-float-- lap:subss)
  (frob sys.int::%%single-float-* lap:mulss))

(define-builtin sys.int::%%single-float-sqrt ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :single-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
    (emit (make-instance 'ir:unbox-single-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sqrtss
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%%single-float-abs ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :single-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :single-float)))
    (emit (make-instance 'ir:unbox-single-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:andps
                         :result result-unboxed
                         :lhs value-unboxed
                         :rhs '(:literal/128 #x7FFFFFFF)))
    (emit (make-instance 'ir:box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

;;; DOUBLE-FLOAT operations.

(define-builtin sys.int::%double-float-as-integer ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-double-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin sys.int::%integer-as-double-float ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'ir:box-double-float-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-fixnum-to-double-float ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer))
        (result-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtsi2sd64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-double-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-single-float-to-double-float ((value) result)
  (let ((temp (make-instance 'ir:virtual-register :kind :single-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
    (emit (make-instance 'ir:unbox-single-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtss2sd64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-double-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%%double-float-< ((lhs rhs) :b)
  (cond ((constant-value-p rhs 'double-float)
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
           (emit (make-instance 'ir:unbox-double-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomisd
                                :operands (list lhs-unboxed `(:literal ,(sys.int::%double-float-as-integer (fetch-constant-value rhs))))
                                :inputs (list lhs-unboxed)
                                :outputs '()))))
        (t
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :double-float))
               (rhs-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
           (emit (make-instance 'ir:unbox-double-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'ir:unbox-double-float-instruction
                                :source rhs
                                :destination rhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomisd
                                :operands (list lhs-unboxed rhs-unboxed)
                                :inputs (list lhs-unboxed rhs-unboxed)
                                :outputs '()))))))

;; TODO: This needs to check two conditions (P & NE), which the
;; compiler can't currently do efficiently.
(define-builtin sys.int::%%double-float-= ((lhs rhs) result)
  (cond ((constant-value-p rhs 'double-float)
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :double-float))
               (temp-result1 (make-instance 'ir:virtual-register))
               (temp-result2 (make-instance 'ir:virtual-register)))
           (emit (make-instance 'ir:unbox-double-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomisd
                                :operands (list lhs-unboxed `(:literal ,(sys.int::%double-float-as-integer (fetch-constant-value rhs))))
                                :inputs (list lhs-unboxed)
                                :outputs '()))
           (emit (make-instance 'ir:constant-instruction
                                :destination temp-result1
                                :value t))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64p
                                :result temp-result2
                                :lhs temp-result1
                                :rhs `(:constant nil)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64ne
                                :result result
                                :lhs temp-result2
                                :rhs `(:constant nil)))))
        (t
         (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :double-float))
               (rhs-unboxed (make-instance 'ir:virtual-register :kind :double-float))
               (temp-result1 (make-instance 'ir:virtual-register))
               (temp-result2 (make-instance 'ir:virtual-register)))
           (emit (make-instance 'ir:unbox-double-float-instruction
                                :source lhs
                                :destination lhs-unboxed))
           (emit (make-instance 'ir:unbox-double-float-instruction
                                :source rhs
                                :destination rhs-unboxed))
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:ucomisd
                                :operands (list lhs-unboxed rhs-unboxed)
                                :inputs (list lhs-unboxed rhs-unboxed)
                                :outputs '()))
           (emit (make-instance 'ir:constant-instruction
                                :destination temp-result1
                                :value t))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64p
                                :result temp-result2
                                :lhs temp-result1
                                :rhs `(:constant nil)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:cmov64ne
                                :result result
                                :lhs temp-result2
                                :rhs `(:constant nil)))))))

(define-builtin sys.int::%%truncate-double-float ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :double-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-double-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvttsd2si64
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(macrolet ((frob (name instruction)
             `(define-builtin ,name ((lhs rhs) result)
                (cond ((constant-value-p rhs 'double-float)
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :double-float))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
                         (emit (make-instance 'ir:unbox-double-float-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',instruction
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs `(:literal ,(sys.int::%double-float-as-integer (fetch-constant-value rhs)))))
                         (emit (make-instance 'ir:box-double-float-instruction
                                              :source result-unboxed
                                              :destination result))))
                      (t
                       (let ((lhs-unboxed (make-instance 'ir:virtual-register :kind :double-float))
                             (rhs-unboxed (make-instance 'ir:virtual-register :kind :double-float))
                             (result-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
                         (emit (make-instance 'ir:unbox-double-float-instruction
                                              :source lhs
                                              :destination lhs-unboxed))
                         (emit (make-instance 'ir:unbox-double-float-instruction
                                              :source rhs
                                              :destination rhs-unboxed))
                         (emit (make-instance 'x86-fake-three-operand-instruction
                                              :opcode ',instruction
                                              :result result-unboxed
                                              :lhs lhs-unboxed
                                              :rhs rhs-unboxed))
                         (emit (make-instance 'ir:box-double-float-instruction
                                              :source result-unboxed
                                              :destination result))))))))
  (frob sys.int::%%double-float-/ lap:divsd)
  (frob sys.int::%%double-float-+ lap:addsd)
  (frob sys.int::%%double-float-- lap:subsd)
  (frob sys.int::%%double-float-* lap:mulsd))

(define-builtin sys.int::%%double-float-sqrt ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :double-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
    (emit (make-instance 'ir:unbox-double-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sqrtsd
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'ir:box-double-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%%double-float-abs ((value) result)
  (let ((value-unboxed (make-instance 'ir:virtual-register :kind :double-float))
        (result-unboxed (make-instance 'ir:virtual-register :kind :double-float)))
    (emit (make-instance 'ir:unbox-double-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:andpd
                         :result result-unboxed
                         :lhs value-unboxed
                         :rhs '(:literal/128 #x7FFFFFFFFFFFFFFF)))
    (emit (make-instance 'ir:box-double-float-instruction
                         :source result-unboxed
                         :destination result))))
