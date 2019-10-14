;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Low-level support functions for instances.

(in-package :mezzano.runtime)

;;; Location encoding.

(defconstant +location-type+ (byte 4 0))
(defconstant +location-type-t+                0)
(defconstant +location-type-unsigned-byte-8+  1)
(defconstant +location-type-unsigned-byte-16+ 2)
(defconstant +location-type-unsigned-byte-32+ 3)
(defconstant +location-type-unsigned-byte-64+ 4)
(defconstant +location-type-signed-byte-8+    5)
(defconstant +location-type-signed-byte-16+   6)
(defconstant +location-type-signed-byte-32+   7)
(defconstant +location-type-signed-byte-64+   8)
(defconstant +location-type-single-float+     9)
(defconstant +location-type-double-float+     10)

(defun sys.int::%instance-layout (object)
  "Return an instance's direct layout."
  (sys.int::%instance-layout object))

(defun sys.int::%fast-instance-layout-eq-p (object instance-header)
  "Test if an instance's direct layout & tag match instance-header."
  (sys.int::%fast-instance-layout-eq-p object instance-header))

(declaim (inline sys.int::instance-or-funcallable-instance-p))
(defun sys.int::instance-or-funcallable-instance-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (sys.int::%instance-or-funcallable-instance-p object)))

(defun sys.int::%instance-or-funcallable-instance-p (object)
  (sys.int::%instance-or-funcallable-instance-p object))

(declaim (inline sys.int::instance-p sys.int::funcallable-instance-p))

(defun sys.int::instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-instance+))

(defun sys.int::funcallable-instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-funcallable-instance+))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun location-types ()
  '(#.+location-type-t+
    #.+location-type-unsigned-byte-8+
    #.+location-type-unsigned-byte-16+
    #.+location-type-unsigned-byte-32+
    #.+location-type-unsigned-byte-64+
    #.+location-type-signed-byte-8+
    #.+location-type-signed-byte-16+
    #.+location-type-signed-byte-32+
    #.+location-type-signed-byte-64+
    #.+location-type-single-float+
    #.+location-type-double-float+))

(defun make-location (type offset)
  (logior (dpb type +location-type+ 0)
          (ash offset 4)))

(defun location-type (location)
  (ldb +location-type+ location))

(declaim (inline location-offset-t))
(defun location-offset-t (location)
  "Return the location offset scaled appropriately for %object-ref-t"
  (ash location -7))

(defun location-offset (location)
  (ash location -4))

(defun location-type-scale (location-type)
  (ecase location-type
    (#.+location-type-t+                1)
    (#.+location-type-unsigned-byte-8+  1)
    (#.+location-type-unsigned-byte-16+ 2)
    (#.+location-type-unsigned-byte-32+ 4)
    (#.+location-type-unsigned-byte-64+ 8)
    (#.+location-type-signed-byte-8+    1)
    (#.+location-type-signed-byte-16+   2)
    (#.+location-type-signed-byte-32+   4)
    (#.+location-type-signed-byte-64+   8)
    (#.+location-type-single-float+     4)
    (#.+location-type-double-float+     8)))

(defun location-type-accessor (location-type)
  (ecase location-type
    (#.+location-type-t+
     'sys.int::%object-ref-t)
    (#.+location-type-unsigned-byte-8+
     'sys.int::%object-ref-unsigned-byte-8-unscaled)
    (#.+location-type-unsigned-byte-16+
     'sys.int::%object-ref-unsigned-byte-16-unscaled)
    (#.+location-type-unsigned-byte-32+
     'sys.int::%object-ref-unsigned-byte-32-unscaled)
    (#.+location-type-unsigned-byte-64+
     'sys.int::%object-ref-unsigned-byte-64-unscaled)
    (#.+location-type-signed-byte-8+
     'sys.int::%object-ref-signed-byte-8-unscaled)
    (#.+location-type-signed-byte-16+
     'sys.int::%object-ref-signed-byte-16-unscaled)
    (#.+location-type-signed-byte-32+
     'sys.int::%object-ref-signed-byte-32-unscaled)
    (#.+location-type-signed-byte-64+
     'sys.int::%object-ref-signed-byte-64-unscaled)
    (#.+location-type-single-float+
     'sys.int::%object-ref-single-float-unscaled)
    (#.+location-type-double-float+
     'sys.int::%object-ref-double-float-unscaled)))
)

(macrolet ((def (name-base args)
             `(defun ,(if name-base
                          (list name-base 'instance-access)
                          'instance-access)
                  (,@args object location &optional (index 0))
                (ecase (location-type location)
                  (#.+location-type-t+
                   (funcall
                    #',(if name-base (list name-base 'sys.int::%object-ref-t) 'sys.int::%object-ref-t)
                    ,@args
                    object (+ (location-offset-t location) index)))
                  ,@(loop
                       for type in (location-types)
                       when (not (eql type +location-type-t+))
                       collect `(,type (funcall #',(if name-base
                                                       (list name-base (location-type-accessor type))
                                                       (location-type-accessor type))
                                                ,@args
                                                object
                                                (+ (location-offset location)
                                                   (* index ,(location-type-scale type))))))))))
  (def nil ())
  (def setf (value))
  (def sys.int::cas (new old)))

(defun slot-location-in-layout (layout slot-name)
  (let ((instance-slots (sys.int::layout-instance-slots layout)))
    (loop
       for i below (sys.int::%object-header-data instance-slots) by 2 ; avoid calling length on the slot vector, it's a simple-vector.
       when (eql slot-name (sys.int::%object-ref-t instance-slots i))
       do (return (sys.int::%object-ref-t instance-slots (1+ i)))
       finally (error "Instance slot ~S missing from layout ~S" slot-name layout))))

(defun instance-slot-location (object slot-name)
  (slot-location-in-layout (sys.int::%instance-layout object)
                           slot-name))

(defun instance-access-by-name (object slot-name &optional (index 0))
  (instance-access object (instance-slot-location object slot-name) index))

(defun (setf instance-access-by-name) (value object slot-name &optional (index 0))
  (setf (instance-access object (instance-slot-location object slot-name) index) value))

(defun (sys.int::cas instance-access-by-name) (old new object slot-name &optional (index 0))
  (sys.int::cas (instance-access object (instance-slot-location object slot-name) index) old new))

(deftype sys.int::instance-header ()
  `(satisfies sys.int::instance-header-p))

(defun sys.int::instance-header-p (object)
  (sys.int::%value-has-tag-p object sys.int::+tag-instance-header+))

(defun %make-instance-header (layout &optional (tag sys.int::+object-tag-instance+))
  (check-type layout sys.int::layout)
  (with-live-objects (layout)
    (sys.int::%%assemble-value
     (logior (ash (sys.int::lisp-object-address layout)
                  sys.int::+object-data-shift+)
             (ash tag
                  sys.int::+object-type-shift+))
     sys.int::+tag-instance-header+)))

(defun %unpack-instance-header (instance-header)
  (sys.int::%%assemble-value
   (ash (sys.int::lisp-object-address instance-header)
        (- sys.int::+object-data-shift+))
   0))

(defstruct (obsolete-instance-layout
             ;; Pinned, as the GC needs to read it.
             ;; Don't make it wired to avoid thrashing the wired area.
             (:area :pinned))
  new-instance
  old-layout)

(defstruct (wired-obsolete-instance-layout
             (:include obsolete-instance-layout)
             (:area :wired))
  ;; No new slots needed here, just changing the allocation area.
  )

(defun supersede-instance (old-instance replacement)
  (let ((layout (sys.int::%instance-layout old-instance)))
    (cond ((sys.int::layout-p layout)
           ;; This really is a layout, not a superseded instance
           (let ((new-layout (if (eql (sys.int::layout-area layout) :wired)
                                 (make-wired-obsolete-instance-layout
                                  :old-layout layout
                                  :new-instance replacement)
                                 (make-obsolete-instance-layout
                                  :old-layout layout
                                  :new-instance replacement))))
             (with-live-objects (new-layout)
               ;; ###: Should this be a CAS?
               ;; FIXME: This needs to keep the GC bits intact.
               ;; Not a problem for objects on the dynamic heap, but will
               ;; be an issue when dealing wired/pinned objects.
               (setf (sys.int::%object-ref-unsigned-byte-64 old-instance -1)
                     ;; Construct a new obsolete-instance header containing
                     ;; our new obsolete layout.
                     (logior (ash (sys.int::lisp-object-address new-layout)
                                  sys.int::+object-data-shift+)
                             (if (sys.int::funcallable-instance-p old-instance)
                                 (ash sys.int::+object-tag-funcallable-instance+
                                      sys.int::+object-type-shift+)
                                 (ash sys.int::+object-tag-instance+
                                      sys.int::+object-type-shift+)))))))
          (t
           ;; This instance has already been superseded, replace it in-place.
           ;; FIXME: Can race with the GC. It can snap the old instance away
           ;; from underneath us, losing the replacement.
           ;; Check if the old instance's layout matches after this?
           (setf (obsolete-instance-layout-new-instance layout)
                 replacement))))
  (values))

(in-package :sys.int)

(defstruct (layout
             (:area :wired)
             :sealed)
  (class nil :read-only t)
  (obsolete nil)
  (heap-size nil)
  (heap-layout nil)
  (area nil)
  (instance-slots nil))
