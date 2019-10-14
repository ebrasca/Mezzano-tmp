;;;; Bignum arithmetic.

(in-package :sys.int)

(declaim (inline bignump
                 %n-bignum-fragments
                 %bignum-fragment
                 (setf %bignum-fragment)))

(defun bignump (object)
  (%object-of-type-p object +object-tag-bignum+))

(defun %n-bignum-fragments (bignum)
  (%object-header-data bignum))

;; Watch out - this can create another bignum to hold the fragment.
(defun %bignum-fragment (bignum n)
  (%object-ref-unsigned-byte-64 bignum n))

(defun (setf %bignum-fragment) (value bignum n)
  (setf (%object-ref-unsigned-byte-64 bignum n) value))

(declaim (inline %bignum-negative-p))
(defun %bignum-negative-p (bignum)
  ;; Use a UB32 access to avoid creating an intermediate bignum.
  (let ((sign-word (%object-ref-unsigned-byte-32
                    bignum
                    (1- (* (%object-header-data bignum) 2)))))
    (logtest #x80000000 sign-word)))

(defun bignum-to-float (bignum float-zero digits)
  (let* ((negative (minusp bignum))
         (bignum (if negative (- bignum) bignum))
         (length (integer-length bignum))
         (sig (ldb (byte digits (- length digits)) bignum))
         (exp (expt (float 2 float-zero) (- length digits))))
    (* (float sig float-zero) exp (if negative -1 1))))

(defun mezzano.runtime::%%coerce-bignum-to-single-float (bignum)
  (bignum-to-float bignum 0.0f0 24))

(defun mezzano.runtime::%%coerce-bignum-to-double-float (bignum)
  (bignum-to-float bignum 0.0d0 53))

(defun %%bignum-truncate (a b)
  "Divide two integers.
Implements the dumb mp_div algorithm from BigNum Math."
  (when (eql b 0)
    (error 'division-by-zero
           :operands (list a b)
           :operation 'truncate))
  (let ((ta (abs a))
        (tb (abs b))
        (tq 1)
        (q 0)
        (n nil))
    ;; Check for the easy case. |a| < |b| => 0, a
    (when (< ta tb)
      (return-from %%bignum-truncate
        (values 0 a)))
    (setf n (- (integer-length ta) (integer-length tb)))
    (setf tb (ash tb n))
    (setf tq (ash tq n))
    ;; Divide bit-by-bit.
    (dotimes (i (1+ n))
      (when (not (> tb ta))
        (setf ta (- ta tb))
        (setf q (+ tq q)))
      (setf tb (ash tb -1)
            tq (ash tq -1)))
    ;; Quotient in Q, remainder in TA.
    ;; Correct sign.
    (when (not (eql (minusp a) (minusp b)))
      (setf q (- q)))
    (when (minusp a)
      (setf ta (- ta)))
    (values q ta)))

(defun %%bignum-multiply-unsigned (a b)
  (assert (bignump a))
  (assert (bignump b))
  (let* ((digs (+ (%n-bignum-fragments a)
                  (%n-bignum-fragments b)
                  1))
         (c (%make-bignum-of-length digs)))
    (dotimes (i digs)
      (setf (%bignum-fragment c i) 0))
    (loop for ix from 0 below (%n-bignum-fragments a) do
         (let ((u 0)
               (pb (min (%n-bignum-fragments b)
                        (- digs ix))))
           (when (< pb 1)
             (return))
           (loop for iy from 0 to (1- pb) do
                (let ((r-hat (+ (%bignum-fragment c (+ iy ix))
                                (%%bignum-multiply-step
                                 (%bignum-fragment a ix)
                                 (%bignum-fragment b iy))
                                u)))
                  (setf (%bignum-fragment c (+ iy ix))
                        (ldb (byte 64 0) r-hat))
                  (setf u (ash r-hat -64))))
           (when (< (+ ix pb) digs)
             (setf (%bignum-fragment c (+ ix pb)) u))))
    (%%canonicalize-bignum c)))

(defun %%bignum-multiply-signed (a b)
  "Multiply two integers together. A and B can be bignums or fixnums."
  (let ((a-negative (< a 0))
        (b-negative (< b 0))
        (c nil))
    (when a-negative
      (setf a (- a)))
    (when b-negative
      (setf b (- b)))
    (when (fixnump a)
      (setf a (%make-bignum-from-fixnum a)))
    (when (fixnump b)
      (setf b (%make-bignum-from-fixnum b)))
    (setf c (%%bignum-multiply-unsigned a b))
    (when (not (eql a-negative b-negative))
      (setf c (- c)))
    c))

(defun %bignum-left-shift (integer count)
  (multiple-value-bind (quot rem)
      (truncate count 32)
    (dotimes (i quot)
      (setf integer (%%bignum-left-shift integer 32)))
    (%%bignum-left-shift integer rem)))

(defun %bignum-right-shift (integer count)
  (multiple-value-bind (quot rem)
      (truncate count 32)
    (dotimes (i quot
              (%%bignum-right-shift integer rem))
      (setf integer (%%bignum-right-shift integer 32))
      (cond ((eql integer 0)
             (return 0))
            ((eql integer -1)
             (return -1))
            ((fixnump integer)
             (setf integer (%make-bignum-from-fixnum integer)))))))

(defun %bignum-integer-length (integer)
  (do* ((n-fragments (sys.int::%n-bignum-fragments integer))
        (last (sys.int::%object-ref-signed-byte-64 integer (1- n-fragments)))
        (len 0 (1+ len)))
       ((or (eq last 0) (eq last -1))
        (+ (* (1- n-fragments) 64) len))
    (setf last (ash last -1))))
