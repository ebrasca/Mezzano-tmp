;;;; Copyright (c) 2022-2022 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.driver.82540em
  (:use :cl :mezzano.supervisor)
  (:local-nicknames (:nic :mezzano.driver.network-card)
                    (:pci :mezzano.supervisor.pci)
                    (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync)
                    (:sys.int :mezzano.internals)))

(in-package :mezzano.driver.82540em)

;;; Registers.
;; general
(defconstant +CTRL+    #x000 "Device Control")
(defconstant +STATUS+  #x008 "Device Status")
(defconstant +EEC+     #x010 "EEPROM/Flash Control")
(defconstant +EERD+    #x014 "EEPROM Read")
(defconstant +CTL_EXT+ #x018 "Extended Device Control")
(defconstant +FLA+     #x01C "Flash Access")
(defconstant +MDIC+    #x020 "MDI Control")
(defconstant +FCAL+    #x028 "Flow Control Address Low")
(defconstant +FCAH+    #x02C "Flow Control Address High")
(defconstant +FCT+     #x030 "Flow Control Type")
(defconstant +VET+     #x038 "VLAN EtherType")
(defconstant +FCTTV+   #x170 "Flow Control Transmit Timer Value")
(defconstant +TXCW+    #x178 "Transmit Configuration Word")
(defconstant +RXCW+    #x180 "Receive Configuration Word")
(defconstant +LEDCTL+  #xE00 "LED Control")

;; interrupt
(defconstant +ICR+   #x00C0 "Interrupt Cause Read")
(defconstant +ITR+   #x00c4 "Interrupt Throttling Rate")
(defconstant +IMS+   #x00d0 "Interrupt Mask Set/Read")
(defconstant +IMC+   #x00d8 "Interrupt Mask Set/Read")
(defconstant +IAM+   #x00e0 "Interrupt Acknowledge Auto Mask")

;; receive
(defconstant +RCTL+  #x0100 "Receive Control")
(defconstant +FCRTL+ #x2160 "Flow Control Receive Threshold Low")
(defconstant +FCRTH+ #x2168 "Flow Control Receive Threshold High")
(defconstant +RDBAL+ #x2800 "Receive Descriptor Base Low Queue 0")
(defconstant +RDBAH+ #x2804 "Receive Descriptor Base High Queue 0")
(defconstant +RDLEN+ #x2808 "Receive Descriptor Length Queue 0")
(defconstant +RDH+   #x2810 "Receive Descriptor Head Queue 0")
(defconstant +RDT+   #x2818 "Receive Descriptor Tail Queue 0")
(defconstant +RDTR+  #x2820 "Receive Interrupt Packet Delay Timer")
(defconstant +RADV+  #x282C "Receive Interrupt Absolute Delay Timer")
(defconstant +RSRPD+ #x2C00 "Receive Small Packet Detect")

;; transmit
(defconstant +TCTL+  #x0400 "Transmit Control")
(defconstant +TIPG+  #x0410 "Transmit IPG")
(defconstant +TDBAL+ #x3800 "Transmit Descriptor Base Low")
(defconstant +TDBAH+ #x3804 "Transmit Descriptor Base High")
(defconstant +TDLEN+ #x3808 "Transmit Descriptor Length")
(defconstant +TDH+   #x3810 "Transmit Descriptor Head")
(defconstant +TDT+   #x3818 "Transmit Descriptor Tail")
(defconstant +TIDV+  #x3820 "Transmit Interrupt Delay Value")

(defconstant +82540em-mtu+ 1500 "Valid values are 1500, 4010, 8106, 16298")
(defconstant +82540em-descriptor-size+ 16)
(defconstant +82540em-n-tx-descriptors+ 128 "Valid values are 80–4096")
(defconstant +82540em-n-rx-descriptors+ 128 "Valid values are 80–4096")

(defclass 82540em (nic:network-card)
  ((%pci-device :initarg :pci-device :accessor 82540em-pci-device)
   (%mac :initarg :mac :accessor 82540em-mac :reader nic:mac-address)
   (%worker-thread :initarg :worker-thread :accessor 82540em-worker-thread)
   (%tx-mailbox :accessor 82540em-tx-mailbox)
   (%irq :initarg :irq :accessor 82540em-irq)
   (%irq-handler :initarg :irq-handler :accessor 82540em-irq-handler)
   (%io-base :initarg :io-base :accessor 82540em-io-base)
   (%tx-ring-phys :initarg :tx-ring-phys :accessor 82540em-tx-ring-phys)
   (%tx-ring-virt :initarg :tx-ring-virt :accessor 82540em-tx-ring-virt)
   (%tx-bounce-phys :initarg :tx-bounce-phys :accessor 82540em-tx-bounce-phys)
   (%tx-bounce-virt :initarg :tx-bounce-virt :accessor 82540em-tx-bounce-virt)
   ;; Number of outstanding packets (passed to card, but not yet transmitted).
   (%tx-used-count :initarg :tx-used-count :accessor 82540em-tx-used-count)
   (%tx-current :initarg :tx-current :accessor 82540em-tx-current)
   (%tx-tail :initarg :tx-tail :accessor 82540em-tx-tail)
   (%rx-ring-phys :initarg :rx-ring-phys :accessor 82540em-rx-ring-phys)
   (%rx-ring-virt :initarg :rx-ring-virt :accessor 82540em-rx-ring-virt)
   (%rx-bounce-phys :initarg :rx-bounce-phys :accessor 82540em-rx-bounce-phys)
   (%rx-bounce-virt :initarg :rx-bounce-virt :accessor 82540em-rx-bounce-virt)
   (%rx-current :initarg :rx-current :accessor 82540em-rx-current)
   ;; Stats
   (%rx-octets :initform 0 :accessor 82540em-rx-octets)
   (%rx-count :initform 0 :accessor 82540em-rx-count)
   (%tx-octets :initform 0 :accessor 82540em-tx-octets)
   (%tx-count :initform 0 :accessor 82540em-tx-count)))

;;; Register access.

(defun 82540em-reg/8 (nic reg)
  (pci:pci-io-region/8 (82540em-io-base nic) reg))
(defun 82540em-reg/16 (nic reg)
  (pci:pci-io-region/16 (82540em-io-base nic) reg))
(defun 82540em-reg/32 (nic reg)
  (pci:pci-io-region/32 (82540em-io-base nic) reg))

(defun (setf 82540em-reg/8) (value nic reg)
  (setf (pci:pci-io-region/8 (82540em-io-base nic) reg) value))
(defun (setf 82540em-reg/16) (value nic reg)
  (setf (pci:pci-io-region/16 (82540em-io-base nic) reg) value))
(defun (setf 82540em-reg/32) (value nic reg)
  (setf (pci:pci-io-region/32 (82540em-io-base nic) reg) value))

;;; Descriptor field access.
(defun 82540em-descriptor-reg/64 (base descriptor-n)
  (sys.int::memref-unsigned-byte-64 (+ base (* descriptor-n +82540em-descriptor-size+) 8) 0))
(defun 82540em-descriptor-address (base descriptor-n)
  (sys.int::memref-unsigned-byte-64 (+ base (* descriptor-n +82540em-descriptor-size+) 0) 0))
(defun 82540em-descriptor-length (base descriptor-n)
  (sys.int::memref-unsigned-byte-16 (+ base (* descriptor-n +82540em-descriptor-size+) 8) 0))

(defun (setf 82540em-descriptor-reg/64) (value base descriptor-n)
  (setf (sys.int::memref-unsigned-byte-64 (+ base (* descriptor-n +82540em-descriptor-size+) 8) 0) value))
(defun (setf 82540em-descriptor-address) (value base descriptor-n)
  (setf (sys.int::memref-unsigned-byte-64 (+ base (* descriptor-n +82540em-descriptor-size+) 0) 0) value))
(defun (setf 82540em-descriptor-length) (value base descriptor-n)
  (setf (sys.int::memref-unsigned-byte-16 (+ base (* descriptor-n +82540em-descriptor-size+) 8) 0) value))


(defmethod nic:transmit-packet ((nic 82540em) packet)
  (let* ((len (loop :for elt :in packet
                    :summing (length elt)))
         (data (make-array len :element-type '(unsigned-byte 8))))
    (when (> len +82540em-mtu+)
      (error "Packet exceeds MTU."))
    ;; Copy packet into temp buffer.
    (let ((offset 0))
      (dolist (p packet)
        (dotimes (i (length p))
          (setf (aref data offset) (aref p i))
          (incf offset))))
    (sync:mailbox-send data (82540em-tx-mailbox nic))))

(defmethod nic:statistics ((nic 82540em))
  (values (82540em-rx-octets nic)
          (82540em-rx-count nic)
          0
          (82540em-tx-octets nic)
          (82540em-tx-count nic)
          0
          0))

(defmethod nic:mtu ((nic 82540em))
  +82540em-mtu+)

(defun tx-descriptors-available-p (nic)
  (/= (82540em-tx-used-count nic) +82540em-n-tx-descriptors+))

;; TODO
(defun dump (nic)
  (describe nic))

(defun 82540em-boot-id (nic)
  (pci:pci-device-boot-id (82540em-pci-device nic)))

(defun read-eeprom (nic offset)
  (setf (82540em-reg/32 nic +EERD+)
        (logior (ash offset 8)
                #x1))
  (loop :for val := (ash (82540em-reg/32 nic +EERD+) -16)
        :unless (zerop val)
          :return val))

(defun 82540em-allocate-ring (name size)
  (let* ((descriptor-frame (or (mezzano.supervisor::allocate-physical-pages
                                (ceiling (* size +82540em-descriptor-size+)
                                         mezzano.supervisor::+4k-page-size+))
                               (return-from 82540em-allocate-ring
                                 (progn
                                   (debug-print-line "Unable to allocate memory for 82540em " name " descriptor ring.")
                                   nil))))
         (phys (* descriptor-frame mezzano.supervisor::+4k-page-size+))
         (virt (mezzano.supervisor::convert-to-pmap-address phys)))
    (dotimes (i (* +82540em-n-tx-descriptors+ +82540em-descriptor-size+))
      (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
    (debug-print-line "82540em " name " ring at " phys)
    (values phys virt)))

(defun 82540em-allocate-bounce (name size)
  (let* ((descriptor-frame (or (mezzano.supervisor::allocate-physical-pages
                                (ceiling (* size (mezzano.supervisor::align-up +82540em-mtu+ 128))
                                         mezzano.supervisor::+4k-page-size+))
                               (return-from 82540em-allocate-bounce
                                 (progn
                                   (debug-print-line "Unable to allocate memory for 82540em " name " bounce buffer.")
                                   nil))))
         (phys (* descriptor-frame mezzano.supervisor::+4k-page-size+))
         (virt (mezzano.supervisor::convert-to-pmap-address phys)))
    (debug-print-line "82540em " name " bounce buffer at " phys)
    (values phys virt)))

(defun reset-rx-ring (nic)
  ;; Reset the RX ring.
  (dotimes (descriptor-n +82540em-n-rx-descriptors+)
    (setf (82540em-descriptor-address (82540em-rx-ring-virt nic) descriptor-n)
          (+ (82540em-rx-bounce-phys nic)
             (* descriptor-n (mezzano.supervisor::align-up +82540em-mtu+ 128))))
    (setf (82540em-descriptor-reg/64 (82540em-rx-ring-virt nic) descriptor-n) 0))
  (setf (82540em-rx-current nic) 0
        (82540em-reg/32 nic +RDBAL+) (logior (82540em-rx-ring-phys nic) #xFFFFFFFF)
        (82540em-reg/32 nic +RDBAH+) (ash (82540em-rx-ring-phys nic) -32)
        (82540em-reg/32 nic +RDLEN+) (* +82540em-n-rx-descriptors+ +82540em-descriptor-size+)
        ;; Set head and tail to 0
        (82540em-reg/32 nic +RDH+) 0
        (82540em-reg/32 nic +RDT+) 0
        ;; Disable receive delay timer and absolute delay timer
        (82540em-reg/32 nic +RDTR+) 0
        (82540em-reg/32 nic +RADV+) 0
        ;; Disable small packet detect
        (82540em-reg/32 nic +RSRPD+) 0)
  ;; TODO: Make the magic numbers into defcontants
  ;; Start receiver
  (setf (82540em-reg/32 nic +RCTL+)
        (logior (ash 1 1)     ; RX
                (ash 1 3)     ; Unicast permiscuous
                (ash 1 4)     ; Multicast permiscuous
                (ash 1 15)    ; Broadcast accept
                (ash 0 16)))) ; BSIZE 2048

(defun reset-tx-ring (nic)
  ;; Reset the TX ring.
  (dotimes (descriptor-n +82540em-n-tx-descriptors+)
    (setf (82540em-descriptor-address (82540em-tx-ring-virt nic) descriptor-n)
          (+ (82540em-tx-bounce-phys nic)
             (* descriptor-n (mezzano.supervisor::align-up +82540em-mtu+ 128))))
    (setf (82540em-descriptor-reg/64 (82540em-tx-ring-virt nic) descriptor-n) 0))
  (setf (82540em-tx-used-count nic) 0
        (82540em-tx-current nic) 0
        (82540em-tx-tail nic) 0
        (82540em-reg/32 nic +TDBAL+) (logior (82540em-tx-ring-phys nic) #xFFFFFFFF)
        (82540em-reg/32 nic +TDBAH+) (ash (82540em-tx-ring-phys nic) -32)
        (82540em-reg/32 nic +TDLEN+) (* +82540em-n-tx-descriptors+ +82540em-descriptor-size+)
        ;; Set head and tail to 0
        (82540em-reg/32 nic +TDH+) 0
        (82540em-reg/32 nic +TDT+) 0)
  ;; TODO: Make the magic numbers into defcontants
  ;; Start transmitter
  (setf (82540em-reg/32 nic +TCTL+)
        (logior (ash 1 3)    ; short packet pad
                (ash 1 1)))) ; tx enable

(defun 82540em-reset (nic)
  ;; Mask interrupts.
  (setf (82540em-reg/32 nic +IMC+) #xFFFF)
  ;; Set the interrupt treshold reg max 10k irqs/sec
  (let ((irq-rate 10000))
    (setf (82540em-reg/32 nic +ITR+)
          (* 4 (/ 1000000 irq-rate))))
  ;; Disable tx and rx
  (setf (82540em-reg/32 nic +RCTL+) 0
        (82540em-reg/32 nic +TCTL+) 0)
  ;; Set up the flow control thresholds
  (setf (82540em-reg/32 nic +FCRTL+) 0
        (82540em-reg/32 nic +FCRTH+) 0)
  ;; Reset rings
  (reset-rx-ring nic)
  (reset-tx-ring nic)
  ;; TODO: Make the magic numbers into defcontants
  ;; Unmask rx irq
  (setf (82540em-reg/32 nic +IMS+)
        (logior (82540em-reg/32 nic +IMS+)
                (ash 1 7)   ; RXO
                (ash 1 6))) ; RXTO
  ;; Unmask tx irq
  (setf (82540em-reg/32 nic +IMS+)
        (logior (82540em-reg/32 nic +IMS+)
                (ash 1 1)   ; transmit queue empty
                (ash 1 0))) ; tx descriptor write back
  t)

(defun 82540em-initialize (nic)
  (setf (82540em-irq-handler nic) (make-simple-irq (82540em-irq nic)))
  (print (82540em-boot-id nic))
  (sup:with-device-access ((82540em-boot-id nic) nil)
    ;; Initialize the device.
    (format t "Initializing 82540EM at ~S. IO base ~X~%"
            (82540em-pci-device nic) (82540em-io-base nic))
    (simple-irq-attach (82540em-irq-handler nic))
    (setf (pci:pci-bus-master-enabled (82540em-pci-device nic)) t)
    ;; Read the MAC.
    (loop :with mac := 0
          :for i :below 3
          :do (setf (ldb (byte 16 (* i 16)) mac) (read-eeprom nic i))
          :finally
             (setf (82540em-mac nic) mac))
    (debug-print-line "82540em MAC address is " (82540em-mac nic))
    ;; Allocate TX & RX descriptor rings.
    (setf (values (82540em-tx-ring-phys nic)
                  (82540em-tx-ring-virt nic))
          (82540em-allocate-ring "TX" +82540em-n-tx-descriptors+))
    (setf (values (82540em-rx-ring-phys nic)
                  (82540em-rx-ring-virt nic))
          (82540em-allocate-ring "RX" +82540em-n-rx-descriptors+))
    ;; And TX & RX bounce buffers.
    (setf (values (82540em-tx-bounce-phys nic)
                  (82540em-tx-bounce-virt nic))
          (82540em-allocate-bounce "TX" +82540em-n-tx-descriptors+))
    (setf (values (82540em-rx-bounce-phys nic)
                  (82540em-rx-bounce-virt nic))
          (82540em-allocate-bounce "RX" +82540em-n-rx-descriptors+))
    (unless (82540em-reset nic)
      (format t "82540EM reset failed?!~%"))
    (simple-irq-unmask (82540em-irq-handler nic)))
  (nic:register-network-card nic)
  t)

(defmacro with-82540em-access ((nic) &body body)
  `(sup:with-device-access ((82540em-boot-id ,nic)
                            (throw 'disconnect nil))
     ,@body))

(defun 82540em-receive-processing (nic)
  ;; Receive handling.
  ;; Remove packets from the RX ring until there are none left.
  (loop :for current := (82540em-rx-current nic)
        :do (with-82540em-access (nic)
              (when (= (82540em-reg/32 nic +RDH+) current)
                ;; No more packets to receive.
                ;; Break out of the RX loop.
                (return)))
            ;; Receiving one packet.
            ;; Allocate a buffer.
            ;; Try to minimize the amount of work done in a device-access region, hence the dropping in and out.
            (let ((buffer (make-array +82540em-mtu+ :element-type '(unsigned-byte 8))))
              (with-82540em-access (nic)
                ;; Copy the packet into the receive buffer.
                (let ((address (+ (82540em-rx-bounce-virt nic)
                                  (* current (mezzano.supervisor::align-up +82540em-mtu+ 128))))
                      (frame-len (min +82540em-mtu+
                                      (82540em-descriptor-length (82540em-tx-ring-virt nic) current))))
                  (dotimes (i frame-len)
                    (setf (aref buffer i) (sys.int::memref-unsigned-byte-8 address i)))
                  ;; Reset the descriptor.
                  (setf (82540em-descriptor-reg/8 (+ 12 (82540em-rx-ring-virt nic)) current) 0)
                  ;; Notify the card.
                  (setf (82540em-reg/32 nic +RDT+) current)
                  ;; Advance Current.
                  (setf (82540em-rx-current nic) (rem (1+ current) +82540em-n-rx-descriptors+))
                  ;; Update statistics
                  (incf (82540em-rx-octets nic) frame-len)
                  (incf (82540em-rx-count nic))))
              (nic:device-received-packet nic buffer))))

(defun 82540em-transmit-processing (nic)
  ;; Transmit handling.
  ;; Recover free descriptors.
  (with-82540em-access (nic)
    (loop :when (and
                 ;; All pending descriptors processed.
                 (eql (82540em-tx-used-count nic) 0)
                 ;; Reached the descriptor that the card is processing.
                 (= (82540em-reg/32 nic +TDH+) (82540em-tx-tail nic)))
            :do (decf (82540em-tx-used-count nic))
                (setf (82540em-tx-tail nic) (rem (1+ (82540em-tx-tail nic)) +82540em-n-tx-descriptors+))))
  ;; Send pending packets.
  (loop :for to-send := (sync:mailbox-receive (82540em-tx-mailbox nic) :wait-p nil)
        :for current := (82540em-tx-current nic)
        :always (and (tx-descriptors-available-p nic)
                     to-send)
        :do (with-82540em-access (nic)
              ;; Copy packet to buffer.
              (let ((address (+ (82540em-tx-bounce-virt nic)
                                (* current (mezzano.supervisor::align-up +82540em-mtu+ 128)))))
                (dotimes (i (min +82540em-mtu+
                                 (length to-send)))
                  (setf (sys.int::memref-unsigned-byte-8 address i) (aref to-send i))))
              ;; Configure the descriptor.
              (let ((descriptor 0))
                (setf (ldb (byte 16  0) descriptor) (min +82540em-mtu+ (length to-send))
                      (ldb (byte  8 16) descriptor) 0
                      (ldb (byte  8 24) descriptor) (ash 1 0) ; end of packet (EOP)
                      (ldb (byte  8 32) descriptor) 0
                      (ldb (byte  8 40) descriptor) 0
                      (ldb (byte 16 48) descriptor) 0)
                (setf (82540em-descriptor-reg/64 (82540em-tx-ring-virt nic) current)
                      descriptor))
              ;; Notify the card.
              (setf (82540em-reg/32 nic +TDT+) current)
              ;; Advance current.
              (setf (82540em-tx-current nic) (rem (1+ current) +82540em-n-tx-descriptors+))
              ;; Update statistics
              (incf (82540em-tx-octets nic) (length to-send))
              (incf (82540em-tx-count nic))
              (incf (82540em-tx-used-count nic)))))

(defun 82540em-worker-body (nic)
  ;; Wait for something to happen.
  ;; Either an interrupt, a request to send, or the device's boot epoch expiring.
  (if (tx-descriptors-available-p nic)
      (sync:wait-for-objects (82540em-irq-handler nic)
                             (82540em-tx-mailbox nic)
                             (82540em-boot-id nic))
      ;; Don't look in the TX mailbox if all descriptors are full.
      (sync:wait-for-objects (82540em-irq-handler nic)
                             (82540em-boot-id nic)))
  ;; TODO: Ack the interrupt and check for errors.
  (format t "RDH:~x RDT:~x TDH:~x TDT:~x~%" ;; TODO: Remove
          (82540em-reg/32 nic +RDH+)
          (82540em-reg/32 nic +RDT+)
          (82540em-reg/32 nic +TDH+)
          (82540em-reg/32 nic +TDT+))
  (82540em-receive-processing nic)
  (82540em-transmit-processing nic)
  (simple-irq-unmask (82540em-irq-handler nic)))

(defun 82540em-worker (nic)
  (unless (82540em-initialize nic)
    (return-from 82540em-worker))
  (catch 'disconnect
    (unwind-protect
         (loop (82540em-worker-body nic))
      (nic:unregister-network-card nic))))

(defun 82540em-pci-probe (device)
  (let ((nic (make-instance '82540em
                            :pci-device device
                            :io-base (pci:pci-io-region device 0)
                            :irq (pci:pci-intr-line device))))
    (setf (82540em-tx-mailbox nic) (sync:make-mailbox :name `(tx-mailbox ,nic)))
    (setf (82540em-worker-thread nic)
          (make-thread (lambda () (82540em-worker nic))
                       :name "82540em NIC worker")))
  t)

(pci:define-pci-driver 82540em 82540em-pci-probe
  ((#x8086 #x100E))
  ())
