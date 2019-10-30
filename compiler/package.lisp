;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Various packages.

(in-package :sys.int)

(defpackage :mezzano.compiler
  (:nicknames :sys.c :system.compiler)
  (:export #:compile
           #:compiler-macro-function
           #:*macroexpand-hook*
           #:macroexpand
           #:macroexpand-1
           #:macro-function
           #:constantp
           #:fixnump

           #:target
           #:x86-64-target
           #:arm64-target
           #:ppc64le-target

           #:quoted-form-p
           #:lambda-information
           #:lambda-information-p
           #:lambda-information-name
           #:lambda-information-docstring
           #:lambda-information-lambda-list
           #:lambda-information-body
           #:lambda-information-required-args
           #:lambda-information-optional-args
           #:lambda-information-rest-arg
           #:lambda-information-enable-keys
           #:lambda-information-key-args
           #:lambda-information-allow-other-keys
           #:lambda-information-environment-arg
           #:lambda-information-environment-layout
           #:lambda-information-fref-arg
           #:lambda-information-closure-arg
           #:lambda-information-count-arg
           #:lambda-information-plist
           #:lexical-variable
           #:lexical-variable-p
           #:lexical-variable-name
           #:lexical-variable-definition-point
           #:lexical-variable-ignore
           #:lexical-variable-dynamic-extent
           #:lexical-variable-use-count
           #:lexical-variable-write-count
           #:lexical-variable-used-in
           #:lexical-variable-plist
           #:localp
           #:special-variable
           #:block-information
           #:block-information-env-var
           #:block-information-count
           #:block-information-return-mode
           #:tagbody-information
           #:tagbody-information-go-tags
           #:go-tag
           #:go-tag-p
           #:go-tag-name
           #:go-tag-tagbody
           #:go-tag-use-count
           #:go-tag-used-in

           #:ast-block
           #:ast-function
           #:ast-go
           #:ast-if
           #:ast-let
           #:ast-multiple-value-bind
           #:ast-multiple-value-call
           #:ast-multiple-value-prog1
           #:ast-progn
           #:ast-quote
           #:ast-return-from
           #:ast-setq
           #:ast-tagbody
           #:ast-the
           #:ast-unwind-protect
           #:ast-call
           #:ast-jump-table

           #:ast-info
           #:ast-body
           #:ast-name
           #:ast-target
           #:ast-go-info
           #:ast-test
           #:ast-if-then
           #:ast-if-else
           #:ast-bindings
           #:ast-value-form
           #:ast-function-form
           #:ast-forms
           #:ast-value
           #:ast-setq-variable
           #:ast-statements
           #:ast-the-type
           #:ast-protected-form
           #:ast-cleanup-function
           #:ast-arguments
           #:ast-targets)
  (:use :cl))

(defpackage :mezzano.lap.arm64
  (:documentation "arm64 assembler for LAP.")
  (:use :cl))

(defpackage :mezzano.lap.ppc64le
  (:documentation "ppc64le assembler for LAP.")
  (:use :cl))

(defpackage :system.internals
  (:nicknames :sys.int)
  (:use :cl))

(defpackage :mezzano.clos
  (:use :cl)
  (:export #:defclass #:defgeneric #:defmethod

           #:find-class
           #:find-class-in-reference
           #:class-reference
           #:class-reference-class

           #:class-of
           #:call-next-method #:next-method-p
           #:slot-value #:slot-boundp #:slot-exists-p #:slot-makunbound
           #:make-instance #:change-class
           #:initialize-instance #:reinitialize-instance #:shared-initialize
           #:update-instance-for-different-class
           #:update-instance-for-redefined-class
           #:print-object
           #:set-funcallable-instance-function

           #:standard-object #:funcallable-standard-object
           #:standard-class #:funcallable-standard-class
           #:standard-generic-function #:standard-method
           #:standard-slot-definition
           #:standard-effective-slot-definition
           #:standard-direct-slot-definition

           #:class-name
           #:class-direct-superclasses #:class-direct-slots
           #:class-precedence-list #:class-slots #:class-direct-subclasses
           #:class-direct-methods
           #:class-finalized-p
           #:class-prototype
           #:class-default-initargs #:class-direct-default-initargs
           #:class-layout #:class-sealed #:class-allocation-area
           #:generic-function-name #:generic-function-lambda-list
           #:generic-function-methods #:generic-function-discriminating-function
           #:generic-function-method-class
           #:generic-function-method-combination
           #:generic-function-argument-precedence-order
           #:method-lambda-list #:method-qualifiers #:method-specializers
           #:method-generic-function #:method-function
           #:slot-definition-name #:slot-definition-initfunction
           #:slot-definition-initform #:slot-definition-initargs
           #:slot-definition-readers #:slot-definition-writers
           #:slot-definition-allocation
           #:slot-definition-documentation
           #:slot-definition-type
           #:slot-definition-location
           ;;
           ;; Class-related metaobject protocol
           ;;
           #:compute-class-precedence-list #:compute-slots
           #:compute-effective-slot-definition
           #:finalize-inheritance #:allocate-instance
           #:slot-value-using-class #:slot-boundp-using-class
           #:slot-exists-p-using-class #:slot-makunbound-using-class
           #:compute-default-initargs
           ;;
           ;; Generic function related metaobject protocol
           ;;
           #:compute-discriminating-function
           #:compute-applicable-methods-using-classes #:method-more-specific-p
           #:compute-applicable-methods
           #:compute-effective-method-function
           #:compute-effective-method
           #:compute-method-function
           #:apply-methods #:apply-method

           #:metaobject #:specializer #:class
           #:structure-class #:structure-object
           #:intern-eql-specializer #:eql-specializer #:eql-specializer-object

           #:slot-unbound #:no-applicable-method

           #:with-slots #:with-accessors

           #:extract-lambda-list
           #:extract-specializer-names

           #:validate-superclass

           #:ensure-class
           #:ensure-class-using-class

           #:structure-slot-definition
           #:structure-effective-slot-definition
           #:structure-direct-slot-definition
           #:structure-slot-definition-read-only
           #:structure-slot-definition-fixed-vector
           #:structure-slot-definition-align

           #:+slot-unbound+

           #:standard-instance-access
           #:funcallable-standard-instance-access
           ))

;;; Supervisor manages the hardware, doing paging and memory management.
(defpackage :mezzano.supervisor
  (:use :cl)
  (:export #:current-thread
           #:with-symbol-spinlock
           #:with-pseudo-atomic
           #:with-snapshot-inhibited
           #:with-device-access
           #:without-interrupts
           #:with-world-stopped
           #:make-thread
           #:thread
           #:threadp
           #:thread-name
           #:thread-state
           #:thread-lock
           #:thread-stack
           #:thread-wait-item
           #:thread-special-stack-pointer
           #:thread-full-save-p
           #:thread-%next
           #:thread-%prev
           #:thread-pending-footholds
           #:thread-mutex-stack
           #:thread-global-next
           #:thread-global-prev
           #:thread-priority
           #:thread-pager-argument-1
           #:thread-pager-argument-2
           #:thread-pager-argument-3
           #:thread-unsleep-helper
           #:thread-unsleep-helper-argument
           #:thread-state-r15
           #:thread-state-r14
           #:thread-state-r13
           #:thread-state-r13-value
           #:thread-state-r12
           #:thread-state-r12-value
           #:thread-state-r11
           #:thread-state-r11-value
           #:thread-state-r10
           #:thread-state-r10-value
           #:thread-state-r9
           #:thread-state-r9-value
           #:thread-state-r8
           #:thread-state-r8-value
           #:thread-state-rdi
           #:thread-state-rsi
           #:thread-state-rbx
           #:thread-state-rbx-value
           #:thread-state-rdx
           #:thread-state-rdx-value
           #:thread-state-rcx
           #:thread-state-rcx-value
           #:thread-state-rax
           #:thread-state-rax-value
           #:thread-state-rbp
           #:thread-frame-pointer
           #:thread-state-rip
           #:thread-state-cs
           #:thread-state-rflags
           #:thread-state-rsp
           #:thread-stack-pointer
           #:thread-state-ss
           #:thread-yield
           #:all-threads
           #:without-footholds
           #:establish-thread-foothold
           #:terminate-thread
           #:thread-join
           #:mutex
           #:mutex-p
           #:make-mutex
           #:with-mutex
           #:mutex-held-p
           #:acquire-mutex
           #:release-mutex
           #:condition-variable
           #:condition-variable-p
           #:make-condition-variable
           #:condition-wait
           #:condition-wait-for
           #:condition-notify
           #:latch
           #:latch-p
           #:make-latch
           #:latch-reset
           #:latch-wait
           #:latch-trigger
           #:snapshot
           #:wait-for-snapshot-completion
           #:allocate-memory-range
           #:protect-memory-range
           #:release-memory-range
           #:update-wired-dirty-bits
           #:debug-print-line
           #:panic
           #:fifo
           #:fifo-p
           #:make-fifo
           #:fifo-push
           #:fifo-pop
           #:fifo-reset
           #:fifo-size
           #:fifo-element-type
           #:irq-fifo
           #:irq-fifo-p
           #:make-irq-fifo
           #:irq-fifo-name
           #:irq-fifo-push
           #:irq-fifo-pop
           #:irq-fifo-reset
           #:irq-fifo-size
           #:irq-fifo-element-type
           #:add-boot-hook
           #:remove-boot-hook
           #:store-statistics
           #:physical-memory-statistics
           #:reboot
           #:current-boot-id
           #:ensure
           #:safe-without-interrupts
           #:with-symbol-spinlock
           #:map-physical-memory
           #:add-deferred-boot-action
           #:logical-core-count

           #:boot-uuid
           #:boot-field
           #:+boot-information-boot-uuid-offset+
           #:+boot-information-32-bit-physical-buddy-bins-offset+
           #:+boot-information-64-bit-physical-buddy-bins-offset+
           #:+boot-information-video+
           #:+boot-information-framebuffer-physical-address+
           #:+boot-information-framebuffer-width+
           #:+boot-information-framebuffer-pitch+
           #:+boot-information-framebuffer-height+
           #:+boot-information-framebuffer-layout+
           #:+boot-information-acpi-rsdp+
           #:+boot-information-options+
           #:+boot-information-n-memory-map-entries+
           #:+boot-information-memory-map+
           #:+boot-information-efi-system-table+
           #:+boot-information-fdt-address+
           #:+boot-information-block-map+
           #:boot-option
           #:+boot-option-force-read-only+
           #:+boot-option-freestanding+
           #:+boot-option-video-console+
           #:+boot-option-no-detect+

           ;; Temporary drivers.
           #:ps/2-key-read
           #:ps/2-aux-read
           #:current-framebuffer
           #:framebuffer-blit
           #:framebuffer-width
           #:framebuffer-height
           #:framebuffer-device
           #:framebuffer-boot-id
           #:safe-sleep
           #:read-rtc-time
           #:all-disks
           #:disk
           #:disk-writable-p
           #:disk-n-sectors
           #:disk-sector-size
           #:disk-read
           #:disk-write
           #:disk-flush
           #:disk-name
           #:make-disk-request
           #:disk-submit-request
           #:disk-cancel-request
           #:disk-await-request
           #:disk-request-complete-p
           #:register-disk
           #:start-profiling
           #:stop-profiling
           #:virtualbox-read-event
           #:virtualbox-graphics-update-framebuffer

           #:platform-irq
           #:irq-attach
           #:irq-eoi

           #:simple-irq
           #:make-simple-irq
           #:simple-irq-irq
           #:simple-irq-unmask
           #:simple-irq-mask
           #:simple-irq-attach
           #:simple-irq-pending-p
           #:simple-irq-masked-p

           #:wait-for-objects
           #:get-object-event
           #:watcher
           #:watcher-p
           #:with-watcher
           #:make-watcher
           #:watcher-name
           #:watcher-destroy
           #:watcher-add-object
           #:watcher-remove
           #:watcher-objects
           #:watcher-wait

           #:event
           #:event-p
           #:make-event
           #:event-name
           #:event-state
           #:event-wait
           #:event-wait-for

           #:timer
           #:timer-p
           #:make-timer
           #:timer-name
           #:timer-arm
           #:timer-arm-absolute
           #:timer-disarm
           #:timer-remaining
           #:timer-deadline
           #:timer-wait
           #:timer-expired-p
           #:with-timer
           #:timer-sleep

           #:thread-thread-pool
           #:thread-pool-block
           #:inhibit-thread-pool-blocking-hijack
           ))

;;; Runtime contains a bunch of low-level and common functions required to
;;; run the supervisor and the rest of the CL system.
(defpackage :mezzano.runtime
  (:use :cl))

(defpackage :sys.lap
  (:documentation "The system assembler.")
  (:use :cl)
  (:export #:perform-assembly-using-target
           #:perform-assembly
           #:emit
           #:emit-relocation
           #:immediatep
           #:resolve-immediate
           #:*current-address*
           #:note-fixup
           #:note-variably-sized-instruction
           #:*function-reference-resolver*
           #:label
           #:make-label
           #:label-name))

(defpackage :sys.lap-x86
  (:documentation "x86 assembler for LAP.")
  (:use :cl :sys.lap))

(defpackage :mezzano.compiler.backend
  (:use :cl :mezzano.compiler)
  (:export #:virtual-register
           #:virtual-register-kind
           #:backend-function
           #:backend-function-name

           #:first-instruction
           #:last-instruction
           #:next-instruction
           #:prev-instruction
           #:insert-before
           #:insert-after
           #:append-instruction
           #:remove-instruction

           #:do-instructions
           #:do-reversed-instructions

           #:backend-instruction
           #:terminator-instruction

           #:print-instruction
           #:instruction-inputs
           #:instruction-outputs
           #:produces-multiple-p
           #:consumes-multiple-p
           #:instruction-pure-p
           #:successors
           #:replace-all-registers

           #:label
           #:label-name
           #:label-phis

           #:argument-setup-instruction
           #:argument-setup-fref
           #:argument-setup-closure
           #:argument-setup-count
           #:argument-setup-required
           #:argument-setup-optional
           #:argument-setup-rest

           #:bind-local-instruction
           #:bind-local-ast
           #:bind-local-value

           #:unbind-local-instruction
           #:unbind-local-local

           #:load-local-instruction
           #:load-local-destination
           #:load-local-local

           #:store-local-instruction
           #:store-local-value
           #:store-local-local

           #:move-instruction
           #:move-destination
           #:move-source

           #:swap-instruction
           #:swap-lhs
           #:swap-rhs

           #:spill-instruction
           #:spill-destination
           #:spill-source

           #:fill-instruction
           #:fill-destination
           #:fill-source

           #:constant-instruction
           #:constant-destination
           #:constant-value

           #:values-instruction
           #:values-values

           #:multiple-value-bind-instruction
           #:multiple-value-bind-values

           #:save-multiple-instruction
           #:restore-multiple-instruction
           #:restore-multiple-context
           #:forget-multiple-instruction
           #:forget-multiple-context

           #:jump-instruction
           #:jump-target

           #:branch-instruction
           #:branch-value
           #:branch-true-target
           #:branch-false-target

           #:switch-instruction
           #:switch-value
           #:switch-targets

           #:call-instruction
           #:call-multiple-instruction
           #:tail-call-instruction
           #:funcall-instruction
           #:funcall-multiple-instruction
           #:tail-funcall-instruction
           #:multiple-value-funcall-instruction
           #:multiple-value-funcall-multiple-instruction
           #:call-result
           #:call-function
           #:call-arguments

           #:return-instruction
           #:return-value
           #:return-multiple-instruction

           #:unreachable-instruction

           #:nlx-region
           #:nlx-context
           #:begin-nlx-instruction
           #:begin-nlx-targets
           #:finish-nlx-instruction
           #:invoke-nlx-instruction
           #:invoke-nlx-multiple-instruction
           #:invoke-nlx-index
           #:invoke-nlx-value
           #:nlx-entry-instruction
           #:nlx-entry-multiple-instruction
           #:nlx-entry-value

           #:push-special-stack-instruction
           #:push-special-stack-a-value
           #:push-special-stack-b-value
           #:push-special-stack-frame
           #:push-special-stack-tag

           #:flush-binding-cache-entry-instruction
           #:flush-binding-cache-entry-symbol
           #:flush-binding-cache-entry-new-value

           #:unbind-instruction
           #:disestablish-block-or-tagbody-instruction
           #:disestablish-unwind-protect-instruction

           #:make-dx-simple-vector-instruction
           #:make-dx-simple-vector-result
           #:make-dx-simple-vector-size

           #:make-dx-cons-instruction
           #:make-dx-cons-result

           #:make-dx-closure-instruction
           #:make-dx-closure-result
           #:make-dx-closure-function
           #:make-dx-closure-environment

           #:box-type
           #:box-instruction
           #:box-destination
           #:box-source
           #:box-fixnum-instruction
           #:box-unsigned-byte-64-instruction
           #:box-signed-byte-64-instruction
           #:box-single-float-instruction
           #:box-double-float-instruction

           #:unbox-instruction
           #:unbox-destination
           #:unbox-source
           #:unbox-fixnum-instruction
           #:unbox-unsigned-byte-64-instruction
           #:unbox-signed-byte-64-instruction
           #:unbox-single-float-instruction
           #:unbox-double-float-instruction

           #:debug-instruction
           #:debug-bind-variable-instruction
           #:debug-unbind-variable-instruction
           #:debug-update-variable-instruction
           #:debug-variable
           #:debug-value

           #:spice-instruction
           #:spice-value

           #:compile-backend-function

           #:perform-target-lowering
           #:perform-target-lowering-post-ssa
           #:perform-target-lap-generation
))

(defpackage :mezzano.compiler.backend.dominance
  (:use :cl :mezzano.compiler.backend)
  (:export #:compute-dominance
           #:dominatep
           #:dominator-tree-parent
           #:dominator-tree-children
           #:dominance-frontier))

(defpackage :mezzano.compiler.backend.ast-convert
  (:use :cl :mezzano.compiler :mezzano.compiler.backend)
  (:export #:convert))

(defpackage :mezzano.compiler.backend.register-allocator
  (:use :cl)
  (:local-nicknames (:ir :mezzano.compiler.backend))
  (:export #:target-argument-registers
           #:target-return-register
           #:target-funcall-register
           #:target-fref-register
           #:target-count-register
           #:architectural-physical-registers
           #:valid-physical-registers-for-kind
           #:spill/fill-register-kinds-compatible
           #:instruction-clobbers
           #:instruction-inputs-read-before-outputs-written-p
           #:allow-memory-operand-p))

(defpackage :mezzano.compiler.backend.x86-64
  (:use :cl)
  (:local-nicknames (:lap :sys.lap-x86)
                    (:ir :mezzano.compiler.backend)
                    (:ra :mezzano.compiler.backend.register-allocator)))

(defpackage :mezzano.compiler.backend.arm64
  (:use :cl)
  (:local-nicknames (:lap :mezzano.lap.arm64)
                    (:ir :mezzano.compiler.backend)
                    (:ra :mezzano.compiler.backend.register-allocator)))

(defpackage :mezzano.compiler.backend.ppc64le
  (:use :cl)
  (:local-nicknames (:lap :mezzano.lap.ppc64le)
                    (:ir :mezzano.compiler.backend)
                    (:ra :mezzano.compiler.backend.register-allocator)))

(defpackage :mezzano.simd
  (:use :cl)
  (:export #:make-mmx-vector
           #:mmx-vector-value
           #:mmx-vector
           #:mmx-vector-p
           #:make-sse-vector
           #:sse-vector-value
           #:sse-vector-ref
           #:make-sse-vector-single-float
           #:sse-vector-single-float-element
           #:sse-vector-single-float-ref
           #:sse-vector-single-float-1-ref
           #:sse-vector-single-float-2-ref
           #:sse-vector-single-float-4-ref
           #:make-sse-vector-double-float
           #:sse-vector-double-float-element
           #:sse-vector-double-float-ref
           #:sse-vector-double-float-1-ref
           #:sse-vector-double-float-2-ref
           #:sse-vector
           #:sse-vector-p))

(defpackage :mezzano.delimited-continuations
  (:use :cl)
  (:export #:delimited-continuation-p
           #:delimited-continuation
           #:make-prompt-tag
           #:prompt-tag
           #:prompt-tag-p
           #:*default-continuation-stack-size*
           #:call-with-prompt
           #:abort-to-prompt
           #:resumable-p
           #:call-with-continuation-barrier
           #:with-continuation-barrier
           #:suspendable-continuation-p
           #:consumed-continuation-resumed
           #:consumed-continuation-resumed-continuation
           #:consumed-continuation-resumed-arguments
           #:barrier-present
           #:barrier-present-tag
           #:barrier-present-barrier
           #:unknown-prompt-tag
           #:unknown-prompt-tag-tag)
  (:local-nicknames (:lap :sys.lap-x86)))
