(in-package #:cl-lz4)

(defconstant +min-match-length+ 4
  "Minimum match length for LZ4 compression.")

(defconstant +max-distance+ 65535
  "Maximum distance for back-references (16-bit offset).")

;;; Hash Table and Compression Strategy Constants
(defconstant +hash-log+ 16
  "Log of the hash table size. 16 means a size of 65536.")

(defconstant +hash-table-size+ (ash 1 +hash-log+)
  "Size of the hash table for finding matches.")

(defconstant +hash-magic+ 2654435761
  "A prime number used in the hash function for better distribution.")

;;; NOTE: constants for multi-probe hashing
(defconstant +hash-probe-2-xor+ #x5555)
(defconstant +hash-probe-3-xor+ #xAAAA)

(defconstant +skip-strength+ 6
  "Controls the 'lazy matching' heuristic. Higher values favor speed, lower values favor compression ratio.")

(defconstant +lazy-match-threshold+ 32
  "Threshold for triggering lazy matching. Only shorter matches will be checked for a better one.")

(defconstant +ml-bits+ 4
  "Number of bits in the token dedicated to match length.")

(defconstant +ml-mask+ (1- (ash 1 +ml-bits+))
  "Mask to extract the match length from a token.")

(defconstant +run-bits+ (- 8 +ml-bits+)
  "Number of bits in the token dedicated to literal length.")

(defconstant +run-mask+ (1- (ash 1 +run-bits+))
  "Mask to extract the literal length from a token.")


;;; The Condition System
(define-condition lz4-error (error)
  ((message :initarg :message :reader lz4-error-message))
  (:report (lambda (condition stream)
             (format stream "LZ4 Error: ~A" (lz4-error-message condition)))))

(define-condition lz4-compression-error (lz4-error)
  ()
  (:report (lambda (condition stream)
             (format stream "LZ4 Compression Error: ~A" (lz4-error-message condition)))))

(define-condition lz4-decompression-error (lz4-error)
  ()
  (:report (lambda (condition stream)
             (format stream "LZ4 Decompression Error: ~A" (lz4-error-message condition)))))

(define-condition lz4-invalid-data-error (lz4-decompression-error)
  ()
  (:report (lambda (condition stream)
             (format stream "LZ4 Invalid Data Error: ~A" (lz4-error-message condition)))))
