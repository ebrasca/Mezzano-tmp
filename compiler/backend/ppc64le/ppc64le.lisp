;;;; Copyright (c) 2019-2020 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.ppc64le)

;;; Wrapper around an arbitrary ppc64le instruction.
(defclass ppc64le-instruction (ir:backend-instruction)
  ((%inputs :initarg :inputs :reader ir:instruction-inputs)
   (%outputs :initarg :outputs :reader ir:instruction-outputs)
   (%opcode :initarg :opcode :reader ppc64le-instruction-opcode)
   (%operands :initarg :operands :reader ppc64le-instruction-operands)
   (%clobbers :initarg :clobbers :reader ppc64le-instruction-clobbers)
   (%early-clobber :initarg :early-clobber :reader ppc64le-instruction-early-clobber)
   (%prefix :initarg :prefix :reader ppc64le-instruction-prefix))
  (:default-initargs :clobbers '() :early-clobber nil :prefix nil))

(defmethod ra:instruction-clobbers ((instruction ppc64le-instruction) (architecture sys.c:ppc64le-target))
  (ppc64le-instruction-clobbers instruction))

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction ppc64le-instruction)
                                                                (architecture sys.c:ppc64le-target))
  (not (ppc64le-instruction-early-clobber instruction)))

(defmethod ir:replace-all-registers ((instruction ppc64le-instruction) substitution-function)
  (setf (slot-value instruction '%inputs) (mapcar substitution-function (slot-value instruction '%inputs)))
  (setf (slot-value instruction '%outputs) (mapcar substitution-function (slot-value instruction '%outputs)))
  (setf (slot-value instruction '%operands)
        (loop :for operand :in (slot-value instruction '%operands)
              :collect (cond ((typep operand 'ir:virtual-register)
                              (funcall substitution-function operand))
                             ((and (consp operand)
                                   (not (member (first operand) '(:constant :function))))
                              (mapcar substitution-function operand))
                             (t operand)))))

(defmethod ir:print-instruction ((instruction ppc64le-instruction))
  (format t "   ~S~%"
          `(:ppc64le ,(ppc64le-instruction-opcode instruction) ,(ppc64le-instruction-operands instruction))))

;;; Wrapper around ppc64le branch instructions.
(defclass ppc64le-branch-instruction (ir:terminator-instruction)
  ((%opcode :initarg :opcode :accessor ppc64le-instruction-opcode)
   (%true-target :initarg :true-target :accessor ppc64le-branch-true-target)
   (%false-target :initarg :false-target :accessor ppc64le-branch-false-target)))

(defmethod ir:successors (function (instruction ppc64le-branch-instruction))
  (list (ppc64le-branch-true-target instruction)
        (ppc64le-branch-false-target instruction)))

(defmethod ir:instruction-inputs ((instruction ppc64le-branch-instruction))
  '())

(defmethod ir:instruction-outputs ((instruction ppc64le-branch-instruction))
  '())

(defmethod ir:replace-all-registers ((instruction ppc64le-branch-instruction) substitution-function)
  )

(defmethod ir:print-instruction ((instruction ppc64le-branch-instruction))
  (format t "   ~S~%"
          `(:ppc64le-branch ,(ppc64le-instruction-opcode instruction)
                            ,(ppc64le-branch-true-target instruction)
                            ,(ppc64le-branch-false-target instruction))))

(defun lower-complicated-box-instructions (backend-function)
  (do* ((inst (ir:first-instruction backend-function) next-inst)
        (next-inst (ir:next-instruction backend-function inst)
                   (if inst
                       (ir:next-instruction backend-function inst)
                       nil)))
       ((null inst))
    (multiple-value-bind (box-function box-register)
        (typecase inst
          (ir:box-unsigned-byte-64-instruction
           (values 'mezzano.runtime::%%make-unsigned-byte-64-gpr10 :gpr10))
          (ir:box-signed-byte-64-instruction
           (values 'mezzano.runtime::%%make-signed-byte-64-gpr10 :gpr10))
          (ir:box-double-float-instruction
           (values 'sys.int::%%make-double-float-gpr10 :gpr10)))
      (when box-function
        (let* ((value (ir:box-source inst))
               (result (ir:box-destination inst)))
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:move-instruction
                          :destination box-register
                          :source value))
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:move-instruction
                          :destination result
                          :source :gpr0))
          (ir:remove-instruction backend-function inst))))))

(defmethod ir:perform-target-lowering (backend-function (target sys.c:ppc64le-target))
  (lower-builtins backend-function))

(defmethod ir:perform-target-lowering-post-ssa (backend-function (target sys.c:ppc64le-target))
  (lower-complicated-box-instructions backend-function))
