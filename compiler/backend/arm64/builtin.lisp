;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.arm64)

(defmacro define-builtin (name (lambda-list results) &body body)
  (when (not (listp results))
    (setf results (list results)))
  (let ((backend-function (gensym))
        (insertion-point (gensym))
        (the-block (gensym))
        (real-lambda-list (loop
                             for arg in lambda-list
                             collect (if (symbolp arg)
                                         arg
                                         (gensym))))
        (defs (gensym)))
    (loop
       for arg in lambda-list
       for real-arg in real-lambda-list
       when (consp arg)
       do
         (assert (eql (first arg) :constant))
         (destructuring-bind (name &optional (predicate t))
             (rest arg)
           (setf body `((let ((,name (let ((arg-defs (gethash ,real-arg ,defs)))
                                       (cond ((and arg-defs
                                                   (endp (rest arg-defs))
                                                   (typep (first arg-defs) 'ir:constant-instruction))
                                              (ir:constant-value (first arg-defs)))
                                             (t (give-up))))))
                          (when (not ,predicate)
                            (give-up))
                          ,@body)))))
    `(%defbuiltin ',name
                  ',real-lambda-list
                  ',results
                  (lambda (,backend-function ,insertion-point ,defs ,@real-lambda-list ,@(remove-if #'keywordp results))
                    (declare (ignorable ,defs ,@real-lambda-list ,@(remove-if #'keywordp results)))
                    (block ,the-block
                      (flet ((emit (inst)
                               (ir:insert-before ,backend-function ,insertion-point inst))
                             (give-up ()
                               (return-from ,the-block nil))
                             (constant-value-p (value &optional (type 't))
                               (and (typep (first (gethash value ,defs)) 'ir:constant-instruction)
                                    (typep (ir:constant-value (first (gethash value ,defs))) type)))
                             (fetch-constant-value (value)
                               (ir:constant-value (first (gethash value ,defs)))))
                        (declare (ignorable #'emit #'give-up #'constant-value-p #'fetch-constant-value))
                        ,@body
                        t))))))

(defclass builtin ()
  ((%name :initarg :name :reader builtin-name)
   (%lambda-list :initarg :lambda-list :reader builtin-lambda-list)
   (%result-list :initarg :result-list :reader builtin-result-list)
   (%generator :initarg :generator :reader builtin-generator)))

(defvar *builtins* (make-hash-table :test 'equal))

(defun match-builtin (name n-arguments)
  (let ((builtin (gethash name *builtins*)))
    (if (and builtin
             (eql (length (builtin-lambda-list builtin)) n-arguments))
        builtin
        nil)))

(defun %defbuiltin (name lambda-list result-list generator)
  (setf (gethash name *builtins*)
        (make-instance 'builtin
                       :name name
                       :lambda-list lambda-list
                       :result-list result-list
                       :generator generator))
  name)

(defun consumed-by-p (definition consumer uses defs)
  "Return true if all DEFINITION's outputs are only used by CONSUMER."
  (dolist (out (ir:instruction-outputs definition)
           t)
    (when (typep out 'ir:virtual-register)
      (let ((out-defs (gethash out defs))
            (out-uses (gethash out uses)))
        ;(format t "Out: ~S  defs: ~S  uses: ~S~%" out out-defs out-uses)
        ;; Must have one definition.
        (when (not (and out-defs
                        (eql (first out-defs) definition)
                        (endp (rest out-defs))))
          (return nil))
        ;; Must be used only by the consumer.
        (when (or (endp out-uses)
                  (not (endp (rest out-uses)))
                  (not (eql (first out-uses) consumer)))
          (return nil))))))

(defstruct predicate-instruction
  inverse jump-instruction cmov-instruction)

(defparameter *predicate-instructions-1*
  '((:eq  :ne lap:b.eq lap:csel.eq)
    (:ne  :eq lap:b.ne lap:csel.ne)
    (:cs  :cc lap:b.cs lap:csel.cs)
    (:cc  :cs lap:b.cc lap:csel.cc)
    (:mi  :pl lap:b.mi lap:csel.mi)
    (:pl  :mi lap:b.pl lap:csel.pl)
    (:vs  :vc lap:b.vs lap:csel.vs)
    (:vc  :vs lap:b.vc lap:csel.vc)
    (:hi  :ls lap:b.hi lap:csel.hi)
    (:ls  :hi lap:b.ls lap:csel.ls)
    (:ge  :lt lap:b.ge lap:csel.ge)
    (:lt  :ge lap:b.lt lap:csel.lt)
    (:gt  :le lap:b.gt lap:csel.gt)
    (:le  :ge lap:b.le lap:csel.le)))

(defparameter *predicate-instructions*
  (let ((ht (make-hash-table :test 'eq)))
    (mapc (lambda (i)
            (setf (gethash (first i) ht)
                  (make-predicate-instruction
                   :inverse (second i)
                   :jump-instruction (third i)
                   :cmov-instruction (fourth i))))
          *predicate-instructions-1*)
    ht))

(defun predicate-info (pred)
  (or (gethash pred *predicate-instructions*)
      (error "Unknown predicate ~S." pred)))

(defun invert-predicate (pred)
  (predicate-instruction-inverse (predicate-info pred)))

(defun reify-predicate (predicate result emitter)
  (let ((tmp (make-instance 'ir:virtual-register)))
    (funcall emitter (make-instance 'ir:constant-instruction
                                    :destination tmp
                                    :value t))
    (funcall emitter (make-instance 'arm64-instruction
                                    :opcode (predicate-instruction-cmov-instruction
                                             (predicate-info predicate))
                                    :operands (list result tmp :x26)
                                    :inputs (list tmp)
                                    :outputs (list result)))))

;; Lower (branch (call foo ...) target) when FOO produces a predicate result.
(defun lower-predicate-builtin (backend-function inst uses defs)
  (let ((next-inst (ir:next-instruction backend-function inst)))
    (when (and (typep inst 'ir:call-instruction)
               (typep next-inst 'ir:branch-instruction)
               (consumed-by-p inst next-inst uses defs))
      (let ((builtin (match-builtin (ir:call-function inst)
                                    (length (ir:call-arguments inst)))))
        (when (and builtin
                   ;; Predicate result.
                   ;; FIXME: This should work when the result consumed by the branch is a predicate and other results are ignored.
                   (eql (length (builtin-result-list builtin)) 1)
                   (keywordp (first (builtin-result-list builtin))))
          (when (not (apply (builtin-generator builtin)
                            backend-function inst
                            defs
                            (ir:call-arguments inst)))
            (return-from lower-predicate-builtin nil))
          (let ((pred (first (builtin-result-list builtin))))
            (ir:insert-before
             backend-function inst
             (make-instance 'arm64-branch-instruction
                            :opcode (predicate-instruction-jump-instruction
                                     (predicate-info pred))
                            :true-target (ir:branch-true-target next-inst)
                            :false-target (ir:branch-false-target next-inst)))
            (let ((advance (ir:next-instruction backend-function next-inst)))
              (ir:remove-instruction backend-function inst)
              (ir:remove-instruction backend-function next-inst)
              advance)))))))

(defun lower-builtin (backend-function inst defs)
  (let ((builtin (and (typep inst '(or
                                    ir:call-instruction
                                    ir:call-multiple-instruction))
                      (match-builtin (ir:call-function inst)
                                     (length (ir:call-arguments inst))))))
    (when builtin
      (let* ((result-regs (if (typep inst 'ir:call-instruction)
                              (list* (ir:call-result inst)
                                     (loop
                                        for r in (rest (builtin-result-list builtin))
                                        collect (make-instance 'ir:virtual-register)))
                              (loop
                                 for r in (builtin-result-list builtin)
                                 collect (make-instance 'ir:virtual-register))))
             (results (loop
                         for result in (builtin-result-list builtin)
                         for reg in result-regs
                         when (not (keywordp result))
                         collect reg)))
        (when (not (apply (builtin-generator builtin)
                          backend-function inst
                          defs
                          (append (ir:call-arguments inst)
                                  results)))
          (return-from lower-builtin nil))
        (cond ((and result-regs
                    (endp (builtin-result-list builtin)))
               ;; Builtin produces no results, but one value expected.
               (assert (endp (rest result-regs)))
               (when (typep inst 'ir:call-instruction)
                 (ir:insert-before
                  backend-function inst
                  (make-instance 'ir:constant-instruction
                                 :destination (first result-regs)
                                 :value nil))))
              (t
               ;; Convert predicate results to NIL/T.
               (loop
                  for result in (builtin-result-list builtin)
                  for reg in result-regs
                  when (keywordp result)
                  do (reify-predicate result reg
                                      (lambda (new-inst)
                                        (ir:insert-before
                                         backend-function inst new-inst))))))
        ;; Fix up multiple values.
        (when (typep inst 'ir:call-multiple-instruction)
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:values-instruction
                          :values (if (endp (builtin-result-list builtin))
                                      '()
                                      result-regs))))
        (let ((advance (ir:next-instruction backend-function inst)))
          (ir:remove-instruction backend-function inst)
          advance)))))

(defun lower-builtins (backend-function)
  (multiple-value-bind (uses defs)
      (ir::build-use/def-maps backend-function)
    (do* ((inst (ir:first-instruction backend-function) next-inst)
          (next-inst (ir:next-instruction backend-function inst) (if inst (ir:next-instruction backend-function inst))))
         ((null inst))
      (let ((next (or (lower-predicate-builtin backend-function inst uses defs)
                      (lower-builtin backend-function inst defs))))
        (when next
          (setf next-inst next))))))
