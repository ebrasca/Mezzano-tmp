;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; 25. Environment

(in-package :sys.int)

(defvar *site-info* nil
  "Site information, returned by SHORT- and LONG-SITE-NAME.
A list of two elements, the short & long name." )

(defvar *machine-info* nil "Value returned by MACHINE-INSTANCE.")

;;; 25.1.2 Debugging Utilities.

(defgeneric documentation (object doc-type)
  (:argument-precedence-order doc-type object))

(when (and (fboundp '(setf documentation))
           (not (typep (fdefinition '(setf documentation))
                       'standard-generic-function)))
  (fmakunbound '(setf documentation)))
(defgeneric (setf documentation) (new-value object doc-type)
  (:argument-precedence-order new-value doc-type object))

;; DOCUMENTATION on function objects.
;; FUNCTION-DOCUMENTATION is the fundamental function that
;; retrieves documentation from the various kinds of functions.

(defgeneric function-documentation (function))
(defgeneric (setf function-documentation) (value function))

(defmethod function-documentation ((function function))
  nil)

(defmethod (setf function-documentation) (new-value (function function))
  new-value)

(defmethod function-documentation ((function compiled-function))
  (debug-info-docstring (function-debug-info function)))

(defmethod (setf function-documentation) (value (function compiled-function))
  (setf (debug-info-docstring (function-debug-info function)) value))

(defmethod function-documentation ((function standard-generic-function))
  (slot-value function 'documentation))

(defmethod (setf function-documentation) (value (function standard-generic-function))
  (setf (slot-value function 'documentation) value))

(defmethod documentation ((x function) (doc-type (eql 't)))
  (function-documentation x))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 't)))
  (setf (function-documentation x) new-value))

(defmethod documentation ((x function) (doc-type (eql 'function)))
  (function-documentation x))

(defmethod (setf documentation) (new-value (x function) (doc-type (eql 'function)))
  (setf (function-documentation x) new-value))

;; DOCUMENTATION on function names.

(defvar *function-documentation*)

(defmethod documentation ((x list) (doc-type (eql 'function)))
  (assert (valid-function-name-p x) (x))
  (values (gethash x *function-documentation*)))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (check-type new-value (or string null))
  (assert (valid-function-name-p x) (x))
  (if new-value
      (setf (gethash x *function-documentation*) new-value)
      (remhash x *function-documentation*))
  new-value)

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (assert (valid-function-name-p x) (x))
  (values (gethash x *function-documentation*)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'function)))
  (check-type new-value (or string null))
  (assert (valid-function-name-p x) (x))
  (if new-value
      (setf (gethash x *function-documentation*) new-value)
      (remhash x *function-documentation*))
  new-value)

;; DOCUMENTATION on compiler macros.

(defvar *compiler-macro-documentation*)

(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  (assert (valid-function-name-p x) (x))
  (values (gethash x *compiler-macro-documentation*)))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  (check-type new-value (or string null))
  (assert (valid-function-name-p x) (x))
  (if new-value
      (setf (gethash x *compiler-macro-documentation*) new-value)
      (remhash x *compiler-macro-documentation*))
  new-value)

(defmethod documentation ((x symbol) (doc-type (eql 'compiler-macro)))
  (assert (valid-function-name-p x) (x))
  (values (gethash x *compiler-macro-documentation*)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'compiler-macro)))
  (check-type new-value (or string null))
  (assert (valid-function-name-p x) (x))
  (if new-value
      (setf (gethash x *compiler-macro-documentation*) new-value)
      (remhash x *compiler-macro-documentation*))
  new-value)

;; DOCUMENTATION on SETF expanders.

(defvar *setf-documentation*)

(defmethod documentation ((x symbol) (doc-type (eql 'setf)))
  (values (gethash x *setf-documentation*)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'setf)))
  (check-type new-value (or string null))
  (if new-value
      (setf (gethash x *setf-documentation*) new-value)
      (remhash x *setf-documentation*))
  new-value)

;; DOCUMENTATION on method combinations.

(defmethod documentation ((x method-combination) (doc-type (eql 't)))
  (documentation x 'method-combination))

(defmethod (setf documentation) (new-value (x method-combination) (doc-type (eql 't)))
  (setf (documentation x 'method-combination) new-value))

(defmethod documentation ((x method-combination) (doc-type (eql 'method-combination)))
  (slot-value x 'documentation))

(defmethod (setf documentation) (new-value (x method-combination) (doc-type (eql 'method-combination)))
  (check-type new-value (or string null))
  (setf (slot-value x 'documentation) new-value))

(defmethod documentation ((x symbol) (doc-type (eql 'method-combination)))
  (let ((mc (gethash x mezzano.clos::*method-combinations*)))
    (when mc
      (documentation mc 'method-combination))))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'method-combination)))
  (let ((mc (gethash x mezzano.clos::*method-combinations*)))
    (when mc
      (setf (documentation mc 'method-combination) new-value)))
  new-value)

;; DOCUMENTATION on methods.

(defmethod documentation ((x method) (doc-type (eql 't)))
  (slot-value x 'documentation))

(defmethod (setf documentation) (new-value (x method) (doc-type (eql 't)))
  (check-type new-value (or string null))
  (setf (slot-value x 'documentation) new-value))

;; DOCUMENTATION on packages.

(defmethod documentation ((x package) (doc-type (eql 't)))
  (package-documentation x))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (check-type new-value (or string null))
  (setf (package-documentation x) new-value))

;; DOCUMENTATION on types, classes, and structure names.

(defmethod documentation ((x mezzano.clos::clos-class) (doc-type (eql 't)))
  (slot-value x 'documentation))

(defmethod (setf documentation) (new-value (x mezzano.clos::clos-class) (doc-type (eql 't)))
  (check-type new-value (or string null))
  (setf (slot-value x 'documentation) new-value))

(defmethod documentation ((x mezzano.clos::clos-class) (doc-type (eql 'type)))
  (slot-value x 'documentation))

(defmethod (setf documentation) (new-value (x mezzano.clos::clos-class) (doc-type (eql 'type)))
  (check-type new-value (or string null))
  (setf (slot-value x 'documentation) new-value))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (let ((class (find-class x nil)))
    (if (typep class 'structure-class)
        (documentation class 'type)
        nil)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'structure)))
  (check-type new-value (or string null))
  (let ((class (find-class x nil)))
    (when (typep class 'structure-class)
      (setf (documentation class 'type) new-value)))
  new-value)

(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (let ((class (find-class x nil)))
    (if class
        (documentation class 'type)
        (let ((info (type-info-for x nil)))
          (if info
              (type-info-docstring info)
              nil)))))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'type)))
  (check-type new-value (or string null))
  (let ((class (find-class x nil)))
    (cond (class
           (setf (documentation class 'type) new-value))
          (t
           (setf (type-info-docstring (type-info-for x))
                 new-value))))
  new-value)

;; DOCUMENTATION on variables.

(defvar *variable-documentation*)

(defmethod documentation ((x symbol) (doc-type (eql 'variable)))
  (values (gethash x *variable-documentation*)))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'variable)))
  (check-type new-value (or string null))
  (if new-value
      (setf (gethash x *variable-documentation*) new-value)
      (remhash x *variable-documentation*))
  new-value)

(defun map-apropos (fn string package)
  (setf string (string string))
  (cond (package
         (do-symbols (sym package)
           (when (search string (symbol-name sym) :test #'char-equal)
             (funcall fn sym))))
        (t
         (do-all-symbols (sym)
           (when (search string (symbol-name sym) :test #'char-equal)
             (funcall fn sym))))))

(defun apropos (string &optional package)
  (map-apropos (lambda (sym)
                 (let ((info '()))
                   (when (boundp sym) (push "bound" info))
                   (when (fboundp sym) (push "fbound" info))
                   (if info
                       (format t "~S ~A~%" sym info)
                       (format t "~S~%" sym))))
               string package)
  (values))

(defun apropos-list (string &optional package)
  (let ((syms '()))
    (map-apropos (lambda (sym)
                   (pushnew sym syms))
                 string package)
    syms))

(defun top-level-form-position (pathname tlf)
  (ignore-errors
    (with-open-file (s pathname)
      (loop
         repeat tlf
         do (with-standard-io-syntax
              (let ((*read-suppress* t)
                    (*read-eval* nil))
                (read s nil))))
      (1+ (file-position s)))))

(defparameter *ed-hook* nil)

(defun ed (&optional x)
  "ED is the standard editor."
  (assert *ed-hook* (*ed-hook*) "No editor configured")
  (when (typep x 'function-name)
    (setf x (fdefinition x)))
  (etypecase x
    (null
     (funcall *ed-hook*))
    ((or pathname string)
     (funcall *ed-hook* :initial-pathname (pathname x)))
    (function
     (let* ((info (function-debug-info x))
            (pathname (debug-info-source-pathname info))
            (tlf (debug-info-source-top-level-form-number info)))
       (funcall *ed-hook*
                :initial-pathname (if pathname
                                      (translate-logical-pathname pathname)
                                      nil)
                :initial-position (top-level-form-position pathname tlf)))))
  (values))

(defparameter *inspect-hook* nil)

(defun inspect (object)
  (assert *inspect-hook* (*inspect-hook*) "No inspector configured")
  (funcall *inspect-hook* object))

(defun dribble (&optional pathname)
  (if pathname
      (with-open-stream (stream (open pathname :direction :io :if-exists :new-version))
        (with-simple-restart (finish-dribble "Exit DRIBBLE")
          (let ((*standard-output* (make-broadcast-stream stream *standard-output*))
                (*standard-input* (make-echo-stream *standard-input* stream)))
            (repl))))
      (let ((restart (find-restart 'finish-dribble)))
        (when restart
          (invoke-restart restart)))))

;;; 25.1.1 Top Level Loop.

(declaim (special * ** ***
                  + ++ +++
                  / // ///
                  -))

(defun repl ()
  (let ((* nil) (** nil) (*** nil)
        (/ nil) (// nil) (/// nil)
        (+ nil) (++ nil) (+++ nil)
        (- nil))
    (loop
       (with-simple-restart (abort "Return to READ-EVAL-PRINT loop.")
         (fresh-line)
         (format t "~A> " (package-shortest-name *package*))
         (let ((form (read)))
           (fresh-line)
           (let ((result (multiple-value-list (let ((- form))
                                                (eval form)))))
             (shiftf *** ** * (first result))
             (shiftf /// // / result)
             (shiftf +++ ++ + form)
             (when result
               (dolist (v result)
                 (fresh-line)
                 (write v)))))))))

;;; 25.1.3 Environment Inquiry.

(defun lisp-implementation-type ()
  "Mezzano")

(defvar *git-revision*) ; Set by the cold generator.
(defvar *lisp-implementation-version* "devel")

(defun lisp-implementation-version ()
  (if *git-revision*
      (format nil "~A ~A" *lisp-implementation-version* *git-revision*)
      *lisp-implementation-version*))

(defun short-site-name () (first *site-info*))
(defun long-site-name () (second *site-info*))

;; (instance)
(defun machine-instance () *machine-info*)
(defun machine-type ()
  #+x86-64 "x86-64"
  #+arm64 "arm64")
(defun machine-version ()
  (multiple-value-bind (cpuid-max vendor-1 vendor-3 vendor-2)
      (cpuid 0)
    (declare (ignore cpuid-max))
    (decode-cpuid-vendor vendor-1 vendor-2 vendor-3)))

;;; Mezzano uses no supporting software.
(defun software-type () nil)
(defun software-version () nil)

(defun uptime ()
  "Print the current uptime."
  (let* ((uptime (truncate (get-internal-run-time) internal-time-units-per-second)))
    (multiple-value-bind (x-minutes seconds)
        (truncate uptime 60)
      (multiple-value-bind (x-hours minutes)
          (truncate x-minutes 60)
        (multiple-value-bind (days hours)
            (truncate x-hours 24)
          (format t "Up ~D day~:P, ~D hour~:P, ~D minute~:P, ~D second~:P.~%"
                  days hours minutes seconds)
          (values))))))
