;;;; Copyright (c) 2019-2020 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.ppc64le)

(defmethod ra:architectural-physical-registers ((architecture sys.c:ppc64le-target))
  '(:gpr0 :gpr1 :gpr2 :gpr3 :gpr4 :gpr5 :gpr6 :gpr7
    :gpr8 :gpr9 :gpr10 :gpr11 :gpr12 :gpr13 :gpr14 :gpr15
    :gpr16 :gpr17 :gpr18 :gpr19 :gpr20 :gpr21 :gpr22 :gpr23
    :gpr24 :gpr25 :gpr26 :gpr27 :gpr28 :gpr29 :gpr30 :gpr31
    :fpr0 :fpr1 :fpr2 :fpr3 :fpr4 :fpr5 :fpr6 :fpr7
    :fpr8 :fpr9 :fpr10 :fpr11 :fpr12 :fpr13 :fpr14 :fpr15
    :fpr16 :fpr17 :fpr18 :fpr19 :fpr20 :fpr21 :fpr22 :fpr23
    :fpr24 :fpr25 :fpr26 :fpr27 :fpr28 :fpr29 :fpr30 :fpr31
    :vr0 :vr1 :vr2 :vr3 :vr4 :vr5 :vr6 :vr7
    :vr8 :vr9 :vr10 :vr11 :vr12 :vr13 :vr14 :vr15
    :vr16 :vr17 :vr18 :vr19 :vr20 :vr21 :vr22 :vr23
    :vr24 :vr25 :vr26 :vr27 :vr28 :vr29 :vr30 :vr31
    :vsr0 :vsr1 :vsr2 :vsr3 :vsr4 :vsr5 :vsr6 :vsr7
    :vsr8 :vsr9 :vsr10 :vsr11 :vsr12 :vsr13 :vsr14 :vsr15
    :vsr16 :vsr17 :vsr18 :vsr19 :vsr20 :vsr21 :vsr22 :vsr23
    :vsr24 :vsr25 :vsr26 :vsr27 :vsr28 :vsr29 :vsr30 :vsr31
    :vsr32 :vsr33 :vsr34 :vsr35 :vsr36 :vsr37 :vsr38 :vsr39
    :vsr40 :vsr41 :vsr42 :vsr43 :vsr44 :vsr45 :vsr46 :vsr47
    :vsr48 :vsr49 :vsr50 :vsr51 :vsr52 :vsr53 :vsr54 :vsr55
    :vsr56 :vsr57 :vsr58 :vsr59 :vsr60 :vsr61 :vsr62 :vsr63))

(defmethod ra:target-argument-registers ((target sys.c:ppc64le-target))
  '(:gpr0 :gpr1 :gpr2 :gpr3 :gpr4))

(defmethod ra:target-return-register ((target sys.c:ppc64le-target))
  :gpr0)

(defmethod ra:target-funcall-register ((target sys.c:ppc64le-target))
  :gpr6)

(defmethod ra:target-fref-register ((target sys.c:ppc64le-target))
  :gpr7)

(defmethod ra:target-count-register ((target sys.c:ppc64le-target))
  :gpr5)

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :value)) (architecture sys.c:ppc64le-target))
  '(:gpr0 :gpr1 :gpr2 :gpr3 :gpr4 :gpr6 :gpr7))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :integer)) (architecture sys.c:ppc64le-target))
  '(:gpr5 :gpr9 :gpr10 :gpr11))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :single-float)) (architecture sys.c:ppc64le-target))
  '(:fpr0 :fpr1 :fpr2 :fpr3 :fpr4 :fpr5 :fpr6 :fpr7
    :fpr8 :fpr9 :fpr10 :fpr11 :fpr12 :fpr13 :fpr14 :fpr15
    :fpr16 :fpr17 :fpr18 :fpr19 :fpr20 :fpr21 :fpr22 :fpr23
    :fpr24 :fpr25 :fpr26 :fpr27 :fpr28 :fpr29 :fpr30 :fpr31))

(defmethod ra:valid-physical-registers-for-kind ((kind (eql :double-float)) (architecture sys.c:ppc64le-target))
  '(:fpr0 :fpr1 :fpr2 :fpr3 :fpr4 :fpr5 :fpr6 :fpr7
    :fpr8 :fpr9 :fpr10 :fpr11 :fpr12 :fpr13 :fpr14 :fpr15
    :fpr16 :fpr17 :fpr18 :fpr19 :fpr20 :fpr21 :fpr22 :fpr23
    :fpr24 :fpr25 :fpr26 :fpr27 :fpr28 :fpr29 :fpr30 :fpr31))

(defmethod ra:spill/fill-register-kinds-compatible (kind1 kind2 (architecture sys.c:ppc64le-target))
  (or (eql kind1 kind2)
      ;; These register kinds are all mutually compatible as they are at most 64 bits wide.
      (and (member kind1 '(:value :integer :single-float :double-float))
           (member kind2 '(:value :integer :single-float :double-float)))))
