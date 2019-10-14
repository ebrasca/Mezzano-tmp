;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(declaim (inline integerp))
(defun integerp (object)
  (or (sys.int::fixnump object)
      (sys.int::bignump object)))

(declaim (inline sys.int::single-float-p sys.int::double-float-p))
(defun sys.int::single-float-p (object)
  (sys.int::%value-has-immediate-tag-p object sys.int::+immediate-tag-single-float+))

(defun sys.int::double-float-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-double-float+))

(declaim (inline floatp))
(defun floatp (object)
  (or (sys.int::single-float-p object)
      (sys.int::%object-of-type-range-p
       object
       sys.int::+first-float-object-tag+
       sys.int::+last-float-object-tag+)))

(defun rationalp (object)
  (or (sys.int::fixnump object)
      (sys.int::%object-of-type-range-p
       object
       sys.int::+first-rational-object-tag+
       sys.int::+last-rational-object-tag+)))

(defun realp (object)
  (or (sys.int::fixnump object)
      (sys.int::single-float-p object)
      (sys.int::%object-of-type-range-p
       object
       sys.int::+first-real-object-tag+
       sys.int::+last-real-object-tag+)))

(defun numberp (object)
  (or (sys.int::fixnump object)
      (sys.int::single-float-p object)
      (sys.int::%object-of-type-range-p
       object
       sys.int::+first-numeric-object-tag+
       sys.int::+last-numeric-object-tag+)))

(defun float (number &optional prototype)
  (etypecase number
    (single-float
     (etypecase prototype
       ((or null single-float)
        number)
       (double-float
        (%%coerce-single-float-to-double-float number))))
    (double-float
     (etypecase prototype
       (single-float
        (%%coerce-double-float-to-single-float number))
       ((or null double-float)
        number)))
    (fixnum
     (etypecase prototype
       ((or null single-float)
        (%%coerce-fixnum-to-single-float number))
       (double-float
        (%%coerce-fixnum-to-double-float number))))
    (bignum
     (etypecase prototype
       ((or null single-float)
        (%%coerce-bignum-to-single-float number))
       (double-float
        (%%coerce-bignum-to-double-float number))))
    (ratio
     (sys.int::ratio-to-float number (etypecase prototype
                                       ((or null single-float) 'single-float)
                                       (double-float 'double-float))))))

(defun sys.int::float-nan-p (float)
  (etypecase float
    (single-float
     (let* ((bits (sys.int::%single-float-as-integer float))
            (exp (ldb (byte 8 23) bits))
            (sig (ldb (byte 23 0) bits)))
       (and (eql exp #xFF)
            (not (zerop sig)))))
    (double-float
     (let* ((bits (sys.int::%double-float-as-integer float))
            (exp (ldb (byte 11 52) bits))
            (sig (ldb (byte 52 0) bits)))
       (and (eql exp #x7FF)
            (not (zerop sig)))))))

(defun sys.int::float-trapping-nan-p (float)
  (etypecase float
    (single-float
     (let* ((bits (sys.int::%single-float-as-integer float))
            (exp (ldb (byte 8 23) bits))
            (sig (ldb (byte 23 0) bits)))
       (and (eql exp #xFF)
            (not (zerop sig))
            (not (logbitp 22 sig)))))
    (double-float
     (let* ((bits (sys.int::%double-float-as-integer float))
            (exp (ldb (byte 11 52) bits))
            (sig (ldb (byte 52 0) bits)))
       (and (eql exp #x7FF)
            (not (zerop sig))
            (not (logbitp 51 sig)))))))

(defun sys.int::float-infinity-p (float)
  (etypecase float
    (single-float
     (let* ((bits (sys.int::%single-float-as-integer float))
            (exp (ldb (byte 8 23) bits))
            (sig (ldb (byte 23 0) bits)))
       (and (eql exp #xFF)
            (zerop sig))))
    (double-float
     (let* ((bits (sys.int::%double-float-as-integer float))
            (exp (ldb (byte 11 52) bits))
            (sig (ldb (byte 52 0) bits)))
       (and (eql exp #x7FF)
            (zerop sig))))))

(defmacro number-dispatch (value &body body)
  (let ((object-tags (make-array 64 :initial-element nil))
        (value-sym (gensym))
        (targets '())
        (fixnum-target nil)
        (single-float-target nil)
        (error-target (gensym "ERROR"))
        (block-name (gensym)))
    (destructuring-bind (value &key (expected-type 'number))
        (if (consp value)
            value
            (list value))
      (loop
         for (keys . forms) in body
         for sym = (gensym)
         do
           (when (not (listp keys))
             (setf keys (list keys)))
           (dolist (key keys)
             (cond ((eql key 'fixnum)
                    (when fixnum-target
                      (error "Duplicate key ~S" key))
                    (setf fixnum-target sym))
                   ((eql key 'single-float)
                    (when single-float-target
                      (error "Duplicate key ~S" key))
                    (setf single-float-target sym))
                   (t
                    (let ((tag (ecase key
                                 ((fixnum single-float) (error "impossible"))
                                 (bignum sys.int::+object-tag-bignum+)
                                 (ratio sys.int::+object-tag-ratio+)
                                 (double-float sys.int::+object-tag-double-float+)
                                 (sys.int::complex-rational sys.int::+object-tag-complex-rational+)
                                 (sys.int::complex-single-float sys.int::+object-tag-complex-single-float+)
                                 (sys.int::complex-double-float sys.int::+object-tag-complex-double-float+))))
                      (when (aref object-tags tag)
                        (error "Duplicate key ~S~%" key))
                      (setf (aref object-tags tag) sym)))))
           (push sym targets)
           (if (equal forms '(:error))
               (push `(go ,error-target) targets)
               (push `(return-from ,block-name (progn ,@forms)) targets)))
      `(let ((,value-sym ,value))
         (block ,block-name
           (tagbody
              (cond ((sys.int::fixnump ,value-sym)
                     (go ,(or fixnum-target error-target)))
                    ((sys.int::single-float-p ,value-sym)
                     (go ,(or single-float-target error-target)))
                    ((sys.int::%value-has-tag-p ,value-sym sys.int::+tag-object+)
                     (sys.int::%jump-table (sys.int::%object-tag ,value-sym)
                                           ,@(loop
                                                for target across object-tags
                                                collect `(go ,(or target error-target)))))
                    (t (go ,error-target)))
              ,@(reverse targets)
              ,error-target
              (error 'type-error :datum ,value-sym :expected-type ',expected-type)))))))

(declaim (inline ratio-<))
(defun ratio-< (x y)
  (< (* (numerator x) (denominator y))
     (* (numerator y) (denominator x))))

(defmacro fixnum-float-compare (the-fixnum the-float float-type swap-args)
  (multiple-value-bind (float< float= truncate-float fix-to-float float-zero m-p-f-float m-n-f-float)
      (ecase float-type
        (single-float
         (values 'sys.int::%%single-float-<
                 'sys.int::%%single-float-=
                 'sys.int::%%truncate-single-float
                 '%%coerce-fixnum-to-single-float
                 0.0f0
                 sys.int::most-positive-fixnum-single-float
                 sys.int::most-negative-fixnum-single-float))
        (double-float
         (values 'sys.int::%%double-float-<
                 'sys.int::%%double-float-=
                 'sys.int::%%truncate-double-float
                 '%%coerce-fixnum-to-double-float
                 0.0d0
                 sys.int::most-positive-fixnum-double-float
                 sys.int::most-negative-fixnum-double-float)))
    (let ((fix (gensym "FIX")))
      ;; Go through great pains to get the effect of (< fixnum (rational float))
      ;; but relatively quickly and with minimal consing.
      ;; Be careful with NaNs too.
      `(cond
         ;; These two tests catch infinities as well.
         ;; Anything smaller than this cannot be represented as a fixnum.
         ;; M-N-F is exactly representable with a float.
         ((,float< ,the-float ,m-n-f-float)
          ;; Smaller than every possible fixnum
          ,(if swap-args t nil))
         ;; Watch out. Boundary condition:
         ;; m-p-f = (1- (expt 2 62))
         ;; (f m-p-f) = (expt 2 62)
         ;; We want the float that is one less (in float units) than (expt 2 62)
         ;; because this is an inclusive test.
         ((,float< ,m-p-f-float ,the-float)
          ;; Larger than every possible fixnum
          ,(if swap-args nil t))
         ((sys.int::float-nan-p ,the-float) nil)
         (t
          ;; The integer part must fit in the fixnum range.
          (let ((,fix (,truncate-float ,the-float)))
            (cond ((%fixnum-< ,the-fixnum ,fix) ,(if swap-args nil t))
                  ((%fixnum-< ,fix ,the-fixnum) ,(if swap-args t nil))
                  ;; Integer parts are equal.
                  ((,float= (,fix-to-float ,fix) ,the-float)
                   ;; No fractional part, the two are equal.
                   nil)
                  ((,float< ,the-float ,float-zero)
                   ;; Float is less than zero and has a fractional part.
                   ;; This makes it more negative than the fixnum.
                   ,(if swap-args t nil))
                  (t
                   ;; Float must be positive and have a fractional part.
                   ;; This makes it more positive.
                   ,(if swap-args nil t)))))))))

;; TODO: These should directly create a bignum, blat the value directly in,
;; and then canonicalize the result.
(defun %%truncate-single-float-to-integer (single-float)
  (let* ((bits (sys.int::%single-float-as-integer single-float))
         (sig (logior (ldb (byte 23 0) bits)
                      (ash 1 23)))
         (exp (- (ldb (byte 8 23) bits) 127))
         (r (ash sig (- exp 23))))
    (if (logbitp 31 bits)
        (- r)
        r)))

(defun %%truncate-double-float-to-integer (double-float)
  (let* ((bits (sys.int::%double-float-as-integer double-float))
         (sig (logior (ldb (byte 52 0) bits)
                      (ash 1 52)))
         (exp (- (ldb (byte 11 52) bits) 1023))
         (r (ash sig (- exp 52))))
    (if (logbitp 63 bits)
        (- r)
        r)))

(defmacro bignum-float-compare (the-bignum the-float float-type swap-args)
  (multiple-value-bind (float< float= truncate-float m-p-f-float m-n-f-float p-inf n-inf)
      (ecase float-type
        (single-float
         (values 'sys.int::%%single-float-<
                 'sys.int::%%single-float-=
                 '%%truncate-single-float-to-integer
                 sys.int::most-positive-fixnum-single-float
                 sys.int::most-negative-fixnum-single-float))
        (double-float
         (values 'sys.int::%%double-float-<
                 'sys.int::%%double-float-=
                 '%%truncate-double-float-to-integer
                 sys.int::most-positive-fixnum-double-float
                 sys.int::most-negative-fixnum-double-float)))
    `(cond ((sys.int::float-nan-p ,the-float) nil)
           ((sys.int::%bignum-negative-p ,the-bignum)
            (cond ((,float< ,the-float ,m-n-f-float)
                   (if (,float= ,the-float ,n-inf)
                       ,(if swap-args t nil)
                       (,(if swap-args '> '<) ,the-bignum (,truncate-float ,the-float))))
                  (t ,(if swap-args nil t))))
           (t
            (cond ((,float< ,m-p-f-float ,the-float)
                   (if (,float= ,the-float ,p-inf)
                       ,(if swap-args nil t)
                       (,(if swap-args '> '<) ,the-bignum (,truncate-float ,the-float))))
                  (t ,(if swap-args t nil)))))))

(defun sys.int::generic-< (x y)
  (number-dispatch (x :expected-type 'real)
    (fixnum
     (number-dispatch (y :expected-type 'real)
       (fixnum (%fixnum-< x y))
       (bignum (not (sys.int::%bignum-negative-p y)))
       (ratio (ratio-< x y))
       (single-float
        (fixnum-float-compare x y single-float nil))
       (double-float
        (fixnum-float-compare x y double-float nil))))
    (bignum
     (number-dispatch (y :expected-type 'real)
       (fixnum (sys.int::%bignum-negative-p x))
       (bignum (sys.int::%%bignum-< x y))
       (ratio (ratio-< x y))
       (single-float
        (bignum-float-compare x y single-float nil))
       (double-float
        (bignum-float-compare x y double-float nil))))
    (ratio
     (number-dispatch (y :expected-type 'real)
       ((fixnum bignum ratio) (ratio-< x y))
       ((single-float double-float)
        (ratio-< x (rational y)))))
    (single-float
     (number-dispatch (y :expected-type 'real)
       (fixnum
        (fixnum-float-compare y x single-float t))
       (bignum
        (bignum-float-compare y x single-float t))
       (ratio
        (ratio-< (rational x) y))
       (single-float
        (sys.int::%%single-float-< x y))
       (double-float
        (sys.int::%%double-float-< (float x 0.0d0) y))))
    (double-float
     (number-dispatch (y :expected-type 'real)
       (fixnum
        (fixnum-float-compare y x double-float t))
       (bignum
        (bignum-float-compare y x double-float t))
       (ratio
        (ratio-< (rational x) y))
       (single-float
        (sys.int::%%double-float-< x (float y 0.0d0)))
       (double-float
        (sys.int::%%double-float-< x y))))))

;; Implement these in terms of the other directions.
;; FIXME: One of these should have a complete implementation so that floating point
;; NaN values are correctly ordered.
(defun sys.int::generic-> (x y)
  (sys.int::generic-< y x))

(defun sys.int::generic->= (x y)
  (not (sys.int::generic-< x y)))

(defun sys.int::generic-<= (x y)
  (not (sys.int::generic-< y x)))

(declaim (inline ratio-= complex-=))
(defun ratio-= (x y)
  (and (= (numerator x) (numerator y))
       (= (denominator x) (denominator y))))

(defun complex-= (x y)
  (and (= (realpart x) (realpart y))
       (= (imagpart x) (imagpart y))))

(declaim (inline fixnum-fits-in-single-float-p
                 fixnum-fits-in-double-float-p))
(defun fixnum-fits-in-single-float-p (fixnum)
  (eq (sys.int::%%truncate-single-float
       (%%coerce-fixnum-to-single-float fixnum))
      fixnum))

(defun fixnum-fits-in-double-float-p (fixnum)
  ;; Do a comparison here to avoid creating a boxed double float for really
  ;; large values.
  (or (typep fixnum '(signed-byte 54))
      (eq (sys.int::%%truncate-double-float
           (%%coerce-fixnum-to-double-float fixnum))
          fixnum)))

(defun sys.int::generic-= (x y)
  (number-dispatch x
    (fixnum
     (number-dispatch y
       (fixnum (eq x y))
       (bignum nil)
       (ratio nil)
       (single-float
        (and (fixnum-fits-in-single-float-p x)
             (sys.int::%%single-float-= (float x 0.0f0) y)))
       (double-float
        (and (fixnum-fits-in-double-float-p x)
             (sys.int::%%double-float-= (float x 0.0d0) y)))
       (sys.int::complex-rational nil)
       ((sys.int::complex-single-float
         sys.int::complex-double-float)
        ;; Float complexes may have 0 in their imaginary part.
        (complex-= x y))))
    (bignum
     (number-dispatch y
       (fixnum nil)
       (bignum (sys.int::%%bignum-= x y))
       (ratio nil)
       (single-float
        ;; Check if the value falls outside the fixnum range.
        ;; These tests will always be false for NaNs
        (if (and (or (sys.int::%%single-float-< sys.int::most-negative-fixnum-single-float y)
                     (sys.int::%%single-float-< y sys.int::most-positive-fixnum-single-float))
                 (not (sys.int::float-infinity-p y)))
            (= x (%%truncate-single-float-to-integer y))
            nil))
       (double-float
        (if (and (or (sys.int::%%double-float-< sys.int::most-negative-fixnum-double-float y)
                     (sys.int::%%double-float-< y sys.int::most-positive-fixnum-double-float))
                 (not (sys.int::float-infinity-p y)))
            (= x (%%truncate-double-float-to-integer y))
            nil))
       (sys.int::complex-rational nil)
       ((sys.int::complex-single-float
         sys.int::complex-double-float)
        ;; Float complexes may have 0 in their imaginary part.
        (complex-= x y))))
    (ratio
     (number-dispatch y
       (fixnum nil)
       (bignum nil)
       (ratio (ratio-= x y))
       ((single-float double-float)
        (ratio-= x (rational y)))
       (sys.int::complex-rational nil)
       ((sys.int::complex-single-float
         sys.int::complex-double-float)
        ;; Float complexes may have 0 in their imaginary part.
        (complex-= x y))))
    (single-float
     (number-dispatch y
       (fixnum
        (and (fixnum-fits-in-single-float-p y)
             (sys.int::%%single-float-= x (float y 0.0f0))))
       (bignum
        (if (and (or (sys.int::%%single-float-< sys.int::most-negative-fixnum-single-float x)
                     (sys.int::%%single-float-< x sys.int::most-positive-fixnum-single-float))
                 (not (sys.int::float-infinity-p x)))
            (= (%%truncate-single-float-to-integer x) y)
            nil))
       (ratio
        (ratio-= (rational x) y))
       (single-float
        (sys.int::%%single-float-= x y))
       (double-float
        (sys.int::%%double-float-= (float x 0.0d0) y))
       (sys.int::complex-rational nil)
       ((sys.int::complex-single-float
         sys.int::complex-double-float)
        ;; Float complexes may have 0 in their imaginary part.
        (complex-= x y))))
    (double-float
     (number-dispatch y
       (fixnum
        (and (fixnum-fits-in-double-float-p y)
             (sys.int::%%double-float-= x (float y 0.0d0))))
       (bignum
        (if (and (or (sys.int::%%double-float-< sys.int::most-negative-fixnum-double-float x)
                     (sys.int::%%double-float-< x sys.int::most-positive-fixnum-double-float))
                 (not (sys.int::float-infinity-p x)))
            (= (%%truncate-double-float-to-integer x) y)
            nil))
       (ratio
        (ratio-= (rational x) y))
       (single-float
        (sys.int::%%double-float-= x (float y 0.0d0)))
       (double-float
        (sys.int::%%double-float-= x y))
       (sys.int::complex-rational nil)
       ((sys.int::complex-single-float
         sys.int::complex-double-float)
        ;; Float complexes may have 0 in their imaginary part.
        (complex-= x y))))
    (sys.int::complex-rational
     (number-dispatch y
       ((fixnum bignum ratio single-float double-float)
        nil)
       ((sys.int::complex-rational
         sys.int::complex-single-float
         sys.int::complex-double-float)
        (complex-= x y))))
    ((sys.int::complex-single-float
      sys.int::complex-double-float)
     (complex-= x y))))

(defun sys.int::generic-truncate (number divisor)
  (assert (/= divisor 0) (number divisor) 'division-by-zero)
  (cond
    ((and (sys.int::fixnump number)
          (eq divisor -1))
     ;; This is needed as most-negative-fixnum / -1 will overflow and produce
     ;; a bignum.
     (values (- number)
             0))
    ((or (and (sys.int::single-float-p number)
              (sys.int::fixnump divisor))
         (and (sys.int::fixnump number)
              (sys.int::single-float-p divisor))
         (and (sys.int::single-float-p number)
              (sys.int::single-float-p divisor)))
     (let* ((val (/ number divisor))
            (integer-part (if (<= sys.int::most-negative-fixnum-single-float
                                  val
                                  sys.int::most-positive-fixnum-single-float)
                              ;; Fits in a fixnum, convert quickly.
                              (sys.int::%%truncate-single-float val)
                              ;; Grovel inside the float
                              (multiple-value-bind (significand exponent sign)
                                  (integer-decode-float val)
                                (* (ash significand exponent) sign)))))
       (values integer-part (* (- val integer-part) divisor))))
    (t (sys.int::full-truncate number divisor))))

;;; From SBCL 1.0.55
(defun ceiling (number &optional (divisor 1))
  ;; If the numbers do not divide exactly and the result of
  ;; (/ NUMBER DIVISOR) would be positive then increment the quotient
  ;; and decrement the remainder by the divisor.
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (minusp number)
                 (plusp number)))
        (values (+ tru 1) (- rem divisor))
        (values tru rem))))

;;; From SBCL 1.0.55
(defun floor (number &optional (divisor 1))
  ;; If the numbers do not divide exactly and the result of
  ;; (/ NUMBER DIVISOR) would be negative then decrement the quotient
  ;; and augment the remainder by the divisor.
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
             (if (minusp divisor)
                 (plusp number)
                 (minusp number)))
        (values (1- tru) (+ rem divisor))
        (values tru rem))))

;; From SBCL 1.0.55
(defun round (number &optional (divisor 1))
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (zerop rem)
        (values tru rem)
        (let ((thresh (/ (abs divisor) 2)))
          (cond ((or (> rem thresh)
                     (and (= rem thresh) (oddp tru)))
                 (if (minusp divisor)
                     (values (- tru 1) (+ rem divisor))
                     (values (+ tru 1) (- rem divisor))))
                ((let ((-thresh (- thresh)))
                   (or (< rem -thresh)
                       (and (= rem -thresh) (oddp tru))))
                 (if (minusp divisor)
                     (values (+ tru 1) (- rem divisor))
                     (values (- tru 1) (+ rem divisor))))
                (t (values tru rem)))))))

(defun sys.int::generic-rem (number divisor)
  (nth-value 1 (sys.int::generic-truncate number divisor)))

(defun mod (number divisor)
  (nth-value 1 (floor number divisor)))

(defun sys.int::binary-/ (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         (sys.int::%%single-float-/ (float x) (float y)))
        (t (sys.int::full-/ x y))))

(defun sys.int::generic-+ (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (sys.int::single-float-p y)
                       (float x y)
                       x))
               (y* (if (sys.int::single-float-p x)
                       (float y x)
                       y)))
           (sys.int::%%single-float-+ x* y*)))
        (t (sys.int::full-+ x y))))

(defun sys.int::generic-- (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (sys.int::single-float-p y)
                       (float x y)
                       x))
               (y* (if (sys.int::single-float-p x)
                       (float y x)
                       y)))
           (sys.int::%%single-float-- x* y*)))
        (t (sys.int::full-- x y))))

(defun sys.int::generic-* (x y)
  (cond ((or (and (sys.int::single-float-p x)
                  (sys.int::fixnump y))
             (and (sys.int::fixnump x)
                  (sys.int::single-float-p y))
             (and (sys.int::single-float-p x)
                  (sys.int::single-float-p y)))
         ;; Convert both arguments to the same kind of float.
         (let ((x* (if (sys.int::single-float-p y)
                       (float x y)
                       x))
               (y* (if (sys.int::single-float-p x)
                       (float y x)
                       y)))
           (sys.int::%%single-float-* x* y*)))
        (t (sys.int::full-* x y))))

(defun %fixnum-integer-length (integer)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type fixnum integer))
  (do ((len 0 (1+ len)))
      ((or (eq integer 0) (eq integer -1))
       len)
    (declare (type fixnum len))
    (setf integer (ash integer -1))))

(defun integer-length (integer)
  (cond ((sys.int::fixnump integer)
         (%fixnum-integer-length integer))
        ((sys.int::bignump integer)
         (sys.int::%bignum-integer-length integer))
        (t (error 'type-error :expected-type 'integer :datum integer))))

(declaim (inline left-shift right-shift))

(defun left-shift (integer count)
  (cond ((and (sys.int::fixnump integer)
              (sys.int::fixnump count))
         (%fixnum-left-shift integer count))
        (t
         (generic-left-shift integer count))))

(defun right-shift (integer count)
  (cond ((and (sys.int::fixnump integer)
              (sys.int::fixnump count))
         (%fixnum-right-shift integer count))
        (t
         (generic-right-shift integer count))))

(defun generic-left-shift (integer count)
  (cond ((not (sys.int::fixnump count))
         (check-type integer integer)
         (check-type count integer)
         ;; The result of shifting any non-zero value left by a
         ;; bignum won't fit in the machine. Don't even bother trying.
         (if (zerop integer)
             0
             (error 'storage-condition)))
        ((sys.int::fixnump integer)
         (%fixnum-left-shift integer count))
        ((sys.int::bignump integer)
         (sys.int::%bignum-left-shift integer count))
        (t
         (check-type integer integer))))

(defun generic-right-shift (integer count)
  (cond ((not (sys.int::fixnump count))
         (check-type count integer)
         (check-type integer integer)
         (if (minusp integer)
             -1
             0))
        ((sys.int::fixnump integer)
         (%fixnum-right-shift integer count))
        ((sys.int::bignump integer)
         (sys.int::%bignum-right-shift integer count))
        (t
         (check-type integer integer))))

(defun ash (integer count)
  (cond
    ((> count 0)
     (left-shift integer count))
    ((< count 0)
     (right-shift integer (- count)))
    (t
     (check-type integer integer)
     (check-type count integer)
     (assert (eql count 0))
     integer)))

(declaim (inline sys.int::binary-=
                 sys.int::binary-< sys.int::binary-<=
                 sys.int::binary-> sys.int::binary->=))

(defun sys.int::binary-= (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (eq lhs rhs)
      (sys.int::generic-= lhs rhs)))

(defun sys.int::binary-< (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-< lhs rhs)
      (sys.int::generic-< lhs rhs)))

(defun sys.int::binary-<= (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (not (%fixnum-< rhs lhs))
      (sys.int::generic-<= lhs rhs)))

(defun sys.int::binary-> (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-< rhs lhs)
      (sys.int::generic-> lhs rhs)))

(defun sys.int::binary->= (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (not (%fixnum-< lhs rhs))
      (sys.int::generic->= lhs rhs)))

(declaim (inline sys.int::binary-+ sys.int::binary--
                 sys.int::binary-* sys.int::%truncate
                 rem
                 sys.int::binary-logand
                 sys.int::binary-logior
                 sys.int::binary-logxor
                 lognot))
(defun sys.int::binary-+ (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-+ lhs rhs)
      (sys.int::generic-+ lhs rhs)))

(defun sys.int::binary-- (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-- lhs rhs)
      (sys.int::generic-- lhs rhs)))

(defun sys.int::binary-* (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-* lhs rhs)
      (sys.int::generic-* lhs rhs)))

(defun sys.int::%truncate (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs)
           ;; Division by 0 is handled by generic-truncate.
           (not (eql rhs 0))
           ;; Division by -1 is the only case that can produce a bignum
           ;; and is not implemented by %FIXNUM-TRUNCATE.
           (not (eql rhs -1)))
      (%fixnum-truncate lhs rhs)
      (sys.int::generic-truncate lhs rhs)))

(defun rem (number divisor)
  (multiple-value-bind (quot rem)
      (truncate number divisor)
    (declare (ignore quot))
    rem))

(defun sys.int::binary-logand (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-logand lhs rhs)
      (sys.int::generic-logand lhs rhs)))

(defun sys.int::binary-logior (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-logior lhs rhs)
      (sys.int::generic-logior lhs rhs)))

(defun sys.int::binary-logxor (lhs rhs)
  (if (and (sys.int::fixnump lhs)
           (sys.int::fixnump rhs))
      (%fixnum-logxor lhs rhs)
      (sys.int::generic-logxor lhs rhs)))

(defun lognot (integer)
  (if (sys.int::fixnump integer)
      (%fixnum-logxor integer -1)
      (sys.int::generic-lognot integer)))
