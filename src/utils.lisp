(in-package #:cl-lz4)

(defun estimate-compressed-size (input-size)
  "Calculate the maximum possible size of compressed data for a given input size.
   This provides a 'worst-case' buffer size for LZ4."
  (declare (type (integer 0 *) input-size))
  (+ input-size (floor input-size 255) 16))

;;; Low-level integer reading/writing
(defun read-u16-le (vector offset)
  "Read a 16-bit little-endian integer from a vector."
  (logior (aref vector offset)
          (ash (aref vector (+ offset 1)) 8)))

(defun write-u16-le (value vector offset)
  "Write a 16-bit little-endian integer to a vector."
  (setf (aref vector offset) (logand value #xFF)
        (aref vector (+ offset 1)) (logand (ash value -8) #xFF)))

(defun read-u32-le (vector offset)
  "Read a 32-bit little-endian integer from a vector."
  (declare (type (simple-array (unsigned-byte 8) (*)) vector)
           (type fixnum offset)
           (optimize (speed 3) (safety 0)))
  (logior (the (unsigned-byte 8) (aref vector offset))
          (ash (the (unsigned-byte 8) (aref vector (+ offset 1))) 8)
          (ash (the (unsigned-byte 8) (aref vector (+ offset 2))) 16)
          (ash (the (unsigned-byte 8) (aref vector (+ offset 3))) 24)))

(defun read-u64-le (vector offset)
  "Read a 64-bit little-endian integer from a vector."
  (declare (type (simple-array (unsigned-byte 8) (*)) vector)
           (type fixnum offset)
           (optimize (speed 3) (safety 0)))
  (logior (aref vector offset)
          (ash (aref vector (+ offset 1)) 8)
          (ash (aref vector (+ offset 2)) 16)
          (ash (aref vector (+ offset 3)) 24)
          (ash (aref vector (+ offset 4)) 32)
          (ash (aref vector (+ offset 5)) 40)
          (ash (aref vector (+ offset 6)) 48)
          (ash (aref vector (+ offset 7)) 56)))

(defun write-u64-le (value vector offset)
  "Write a 64-bit little-endian integer to a vector."
  (loop for i from 0 below 8
        for byte-offset = (* i 8)
        do (setf (aref vector (+ offset i)) (logand (ash value (- byte-offset)) #xFF))))

;;; String/data conversion optimizations
(declaim (inline string-to-bytes-fast))
(defun string-to-bytes-fast (string)
  "Fast string to byte vector conversion."
  (declare (type string string)
           (optimize (speed 3) (safety 0)))
  (let* ((len (length string))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (declare (type fixnum len))
    (loop for i of-type fixnum from 0 below len
          do (setf (aref result i) (char-code (char string i))))
    result))
