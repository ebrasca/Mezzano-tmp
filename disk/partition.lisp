;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(in-package :mezzano.disk)

(defclass disk-mixin ()
  ((%writable-p   :initarg :writable-p      :accessor disk-writable-p)
   (%n-sectors    :initarg :n-sectors       :accessor disk-n-sectors)
   (%sector-size  :initarg :sector-size     :accessor disk-sector-size)))

(defclass disk-pt-mixin ()
  ((%pt-type     :initarg :pt-type         :accessor pt-type)
   (%disk-id     :initarg :disk-id         :accessor disk-id)
   (%first-lba   :initarg :pt-first-lba    :accessor pt-first-lba)
   (%last-lba    :initarg :pt-last-lba     :accessor pt-last-lba))
  )

(defclass disk-partition-mixin ()
  ((%disk             :initarg :disk             :accessor dp-disk)
   (%partition-num    :initarg :partition-num    :accessor dp-partition-num)
   (%partition-type   :initarg :partition-type   :accessor dp-partition-type)
   (%start-lba        :initarg :start-lba        :accessor dp-start-lba)
   (%size             :initarg :size             :accessor dp-size)))

;;======================================================================
;; Support functions
;;======================================================================

(defmacro def-accessor (name offset size)
  (let ((acc-name (intern (concatenate 'string "GET-" (symbol-name name)))))
    `(progn
       (defun ,acc-name (buf &optional (base 0))
         (let ((idx (+ ,offset base)))
           ,(case size
              (1 '(aref buf idx))
              (2 '(logior (aref buf idx)
                          (ash (aref buf (1+ idx)) 8)))
              (4 '(logior (aref buf idx)
                          (ash (aref buf (+ idx 1))  8)
                          (ash (aref buf (+ idx 2)) 16)
                          (ash (aref buf (+ idx 3)) 24)))
              (8 '(logior (aref buf idx)
                          (ash (aref buf (+ idx 1))  8)
                          (ash (aref buf (+ idx 2)) 16)
                          (ash (aref buf (+ idx 3)) 24)
                          (ash (aref buf (+ idx 4)) 32)
                          (ash (aref buf (+ idx 5)) 40)
                          (ash (aref buf (+ idx 6)) 48)
                          (ash (aref buf (+ idx 7)) 56)))
              (16 '(logior (aref buf idx)
                           (ash (aref buf (+ idx  1))   8)
                           (ash (aref buf (+ idx  2))  16)
                           (ash (aref buf (+ idx  3))  24)
                           (ash (aref buf (+ idx  4))  32)
                           (ash (aref buf (+ idx  5))  40)
                           (ash (aref buf (+ idx  6))  48)
                           (ash (aref buf (+ idx  7))  56)
                           (ash (aref buf (+ idx  8))  64)
                           (ash (aref buf (+ idx  9))  72)
                           (ash (aref buf (+ idx 10))  80)
                           (ash (aref buf (+ idx 11))  88)
                           (ash (aref buf (+ idx 12))  96)
                           (ash (aref buf (+ idx 13)) 104)
                           (ash (aref buf (+ idx 14)) 112)
                           (ash (aref buf (+ idx 15)) 120)))
              (T (error "def-accessor: invalid field ~A~%" name )))))
       (defun (setf ,acc-name) (value buf &optional (base 0))
         (let ((idx (+ ,offset base)))
           ,(case size
              (1 '(setf (aref buf idx) value))
              (2 '(setf (aref buf idx)      (logand value #xFF)
                        (aref buf (1+ idx)) (logand (ash value -8) #xFF)))
              (4 '(setf (aref buf idx)       (logand value #xFF)
                        (aref buf (1+ idx))  (logand (ash value  -8) #xFF)
                        (aref buf (+ idx 2)) (logand (ash value -16) #xFF)
                        (aref buf (+ idx 3)) (logand (ash value -24) #xFF)))
              (8 '(setf (aref buf idx)       (logand value #xFF)
                        (aref buf (1+ idx))  (logand (ash value  -8) #xFF)
                        (aref buf (+ idx 2)) (logand (ash value -16) #xFF)
                        (aref buf (+ idx 3)) (logand (ash value -24) #xFF)
                        (aref buf (+ idx 4)) (logand (ash value -32) #xFF)
                        (aref buf (+ idx 5)) (logand (ash value -40) #xFF)
                        (aref buf (+ idx 6)) (logand (ash value -48) #xFF)
                        (aref buf (+ idx 7)) (logand (ash value -56) #xFF)))
              (16 '(setf (aref buf idx)       (logand value #xFF)
                         (aref buf (1+ idx))  (logand (ash value    -8) #xFF)
                         (aref buf (+ idx  2)) (logand (ash value  -16) #xFF)
                         (aref buf (+ idx  3)) (logand (ash value  -24) #xFF)
                         (aref buf (+ idx  4)) (logand (ash value  -32) #xFF)
                         (aref buf (+ idx  5)) (logand (ash value  -40) #xFF)
                         (aref buf (+ idx  6)) (logand (ash value  -48) #xFF)
                         (aref buf (+ idx  7)) (logand (ash value  -56) #xFF)
                         (aref buf (+ idx  8)) (logand (ash value  -64) #xFF)
                         (aref buf (+ idx  9)) (logand (ash value  -72) #xFF)
                         (aref buf (+ idx 10)) (logand (ash value  -80) #xFF)
                         (aref buf (+ idx 11)) (logand (ash value  -88) #xFF)
                         (aref buf (+ idx 12)) (logand (ash value  -96) #xFF)
                         (aref buf (+ idx 13)) (logand (ash value -104) #xFF)
                         (aref buf (+ idx 14)) (logand (ash value -112) #xFF)
                         (aref buf (+ idx 15)) (logand (ash value -120) #xFF)))))
         value))))

;;======================================================================
;; GUID Partition Table Code (GPT)
;;======================================================================

;; Partition Table Header fields
(def-accessor :header-size     12 4)
(def-accessor :header-crc      16 4)
(def-accessor :first-lba       40 8)
(def-accessor :last-lba        48 8)
(def-accessor :disk-id         56 16)
(def-accessor :pt-lba          72 8)
(def-accessor :num-pt-entries  80 4)
(def-accessor :pt-entry-size   84 4)
(def-accessor :pt-crc          88 4)

;; Partition Table Entry fields
(def-accessor :part-first-lba   32  8)
(def-accessor :part-last-lba    40  8)
(def-accessor :part-attributes  48  8)

;; (def-accessor :part-type-guid    0 16)
(defun get-part-type-guid (buf &optional (base 0))
  (get-guid buf base))

(defun (setf get-part-type-guid) (value buf &optional (base 0))
  (setf-guid value buf base))

  ;; (def-accessor :part-guid        16 16)
(defun get-part-guid (buf &optional (base 0))
  (get-guid buf (+ base 16)))

(defun (setf get-part-guid) (value buf &optional (base 0))
  (setf-guid value buf (+ base 16)))

(defun check-gpt-crc (buf)
  (let ((header-size (get-header-size buf))
        (expected-crc (get-header-crc buf)))
    (setf (get-header-crc buf) 0)
    (let ((actual-crc (crc-32 buf :end header-size)))
      (setf (get-header-crc buf) expected-crc)
      (when (/= expected-crc actual-crc)
        (error "GUID partition table header CRC error. Expected ~8,'0X, got ~8,'0X" expected-crc actual-crc)))))

(defun read-gpt-partition-table (disk header-buf)
  (let* ((offset (get-pt-lba header-buf))
         (num-entries (get-num-pt-entries header-buf))
         (entry-size (get-pt-entry-size header-buf))
         (num-bytes (* num-entries entry-size))
         (num-sectors (ceiling num-bytes (block-device-sector-size disk)))
         (pt-buf (make-array num-bytes :element-type '(unsigned-byte 8))))
    (block-device-read disk offset num-sectors pt-buf)
    (values num-entries entry-size pt-buf)))

(defun parse-guid-partition-table (disk)
  (let* ((sector-size (block-device-sector-size disk))
         (header-buf (make-array sector-size :element-type '(unsigned-byte 8)))
         (result NIL))
    (block-device-read disk 1 1 header-buf)

    ;; check signature "EFI PART"
    (loop
       for idx = 0 then (1+ idx)
       for sig-value in '(#x45 #x46 #x49 #x20 #x50 #x41 #x52 #x54)
       when (/= (aref header-buf idx) sig-value) do
         (return-from parse-guid-partition-table NIL))

    (sup:debug-print-line "Detected GPT on disk " disk)

    ;; check header CRC
    (check-gpt-crc header-buf)

    (multiple-value-bind (num-entries entry-size pt-buf)
        (read-gpt-partition-table disk header-buf)
      (dotimes (i num-entries)
        (let* ((base (* i entry-size))
               (partition-type (get-part-type-guid pt-buf base))
               (start-lba (get-part-first-lba pt-buf base))
               (size (1+ (- (get-part-last-lba pt-buf base) start-lba))))
          (when (string/= partition-type "00000000-0000-0000-0000-000000000000")
            (push (list :disk disk
                        :partition-num i
                        :partition-type partition-type
                        :start-lba start-lba
                        :size size
                        #+nil
                        :attributes #+nil (get-part-attributes pt-buf base))
                  result)))))
    (nreverse result)))

;;======================================================================
;; MBR Partition Table Code (MPT)
;;======================================================================

;; Partition Table Entry fields
(def-accessor :boot-flag        0 1)
(def-accessor :partition-type   4 1)
(def-accessor :first-sector     8 4)
(def-accessor :partition-size  12 4)

(defun decode-ebr (buf)
  ;; return <partition type> <data offset> <partition size> <ebr offset>
  (if (and (= (aref buf #x1FE) #x55)
           (= (aref buf #x1FF) #xAA))
      (values (get-partition-type buf #x1BE)
              (get-first-sector buf #x1BE)
              (get-partition-size buf #x1BE)
              (get-first-sector buf #x1CE))
      (values 0 0 0 0)))

(defun parse-mbr-partition-table (disk)
  (let* ((sector-size (block-device-sector-size disk))
         (buf (make-array sector-size :element-type '(unsigned-byte 8)))
         (ebr-lba NIL)
         (result NIL))
    (block-device-read disk 0 1 buf)

    ;; check signature #x55 #xAA
    (when (or (/= (aref buf #x1FE) #x55)
              (/= (aref buf #x1FF) #xAA))
      (return-from parse-mbr-partition-table NIL))

    (sup:debug-print-line "Detected MBR partition table" disk)

    (dotimes (i 4)
      (let* ((base (+ (* i 16) #x1BE))
             (part-type (get-partition-type buf base))
             (start-lba (get-first-sector buf base))
             (size (get-partition-size buf base)))
        (when (and (/= part-type 0)
                   (/= size 0))
          (sup:debug-print-line "Detected partition " i
                                " on disk " disk
                                ". Start: " start-lba
                                ", size: " size)
          (push (list :disk disk
                      :partition-num i
                      :partition-type part-type
                      :start-lba start-lba
                      :size size)
                result)

          (when (or (= part-type #x05)
                    (= part-type #x0F))
            (setf ebr-lba start-lba)))))

    ;; Handle extended partition documentation at:
    ;; https://thestarman.pcministry.com/asm/mbr/PartTables.htm
    (when ebr-lba
      (block-device-read disk ebr-lba 1 buf)

      (loop with part-type and data-offset and size and ebr-offset
         with part-num = 4
         do (setf (values part-type data-offset size ebr-offset)
                  (decode-ebr buf))
         unless (or (eql data-offset 0)
                    (eql size 0))
         do
           (sup:debug-print-line "Extended partition " part-num
                                 " on disk " disk
                                 ". Type: " part-type
                                 ", start: " (+ ebr-lba data-offset)
                                 ", size: " size)
           (push (list :disk disk
                       :partition-num part-num
                       :partition-type part-type
                       :start-lba (+ ebr-lba data-offset)
                       :size size)
                 result)
           (incf part-num)
         if (eql ebr-offset 0)
         do (return nil)
         else do
           (setf ebr-lba (+ ebr-lba ebr-offset))
           (block-device-read disk ebr-lba 1 buf)))
    (nreverse result)))

;;======================================================================
;;======================================================================

(defun parse-partition-table (disk)
  (or (parse-guid-partition-table disk)
      (parse-mbr-partition-table disk)))

(defun parse-disk-info (disk)
  (list
   :sector-size (block-device-sector-size disk)
   :n-sectors (block-device-n-sectors disk)))

(defun print-disk-info (stream disk)
  (let* ((disk-info (parse-disk-info disk))
         (sector-size (getf disk-info :sector-size))
         (n-sectors (getf disk-info :n-sectors))
         (num-bytes (* sector-size n-sectors))
         (num-bytes-gb (/ num-bytes 1024.0 1024.0 1024.0)))
    (format stream "Sector Size: ~D    Num Sectors: ~D"
            sector-size
            n-sectors)
    (format stream "    Bytes: ~D (~4,2F GB)~%"
            num-bytes
            num-bytes-gb))
  (values))

(defun print-partitions (stream disk)
  (let ((partitions (parse-partition-table disk)))
    (format stream "~&Num    Type ~15T         Start LBA               Size~%")
    (dolist (part partitions)
        (format stream " ~D     #x~2,'0X ~15T~10D (#x~8,'0X) ~10D (#x~8,'0X)~%"
                (getf part :partition-num)
                (getf part :partition-type)
                (getf part :start-lba)
                (getf part :start-lba)
                (getf part :size)
                (getf part :size))))
  (values))
