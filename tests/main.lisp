(defpackage #:cl-lz4-tests
  (:use #:cl #:cl-lz4 #:fiveam #:uiop))

(in-package #:cl-lz4-tests)

(def-suite lz4-tests
  :description "Test suite for the upgraded LZ4 library")

(in-suite lz4-tests)

(defun round-trip-test (data-generator name)
  "Helper function to run a standard compression/decompression test."
  (let* ((original-data (funcall data-generator))
         ;; Coerce to the correct type for comparison after decompression.
         (original-byte-data (coerce original-data '(vector (unsigned-byte 8))))
         (uncompressed-size (length original-byte-data))
         ;; Use the main 'compress' function, which handles various input vector types.
         (compressed (compress original-data))
         (decompressed (decompress-block compressed uncompressed-size)))
    (is (equalp original-byte-data decompressed)
        (format nil "~A: Decompressed data should match original" name))))

(test basic-compression
  "Test basic compression and decompression of a simple string."
  (round-trip-test (lambda () (map '(vector (unsigned-byte 8)) #'char-code "Hello, World! This is a test string for LZ4 compression."))
                   "Basic Compression"))

(test empty-data
  "Test compression of empty data."
  (round-trip-test (lambda () (make-array 0 :element-type '(unsigned-byte 8)))
                   "Empty Data"))

(test repetitive-data
  "Test compression of highly repetitive data, expecting good compression ratio."
  (let* ((original-data (make-array 10000 :element-type '(unsigned-byte 8) :initial-element 65))
         (compressed (compress original-data)))
    (is (< (length compressed) 200) "Repetitive data should compress very well.")
    (is (equalp original-data (decompress compressed :uncompressed-size 10000)))))

(test random-data
  "Test compression of random (incompressible) data."
  (let* ((original-data (make-array 1024 :element-type '(unsigned-byte 8)))
         (_ (loop for i from 0 below 1024 do (setf (aref original-data i) (random 256)))))
    (declare (ignore _))
    (round-trip-test (lambda () original-data) "Random Data")))

(test large-data
  "Test compression of a larger, semi-repetitive block of data."
  (let* ((large-string (with-output-to-string (s)
                         (loop repeat 500
                               do (format s "This is line ~D of the test data. The quick brown fox. "
                                          (random 100)))))
         (original-data (map '(vector (unsigned-byte 8)) #'char-code large-string)))
    (round-trip-test (lambda () original-data) "Large Data")))

(test error-handling
  "Test that the library signals appropriate errors for invalid operations."
  (signals lz4-decompression-error
    (decompress (make-array 10) :uncompressed-size 100000)
    "Should signal error on output buffer overflow.")
  (signals lz4-decompression-error
    (decompress #())
    "Should signal error if uncompressed-size is not provided."))

(test file-operations
  "Test file compression and decompression using the wrapper functions."
  (with-temporary-file (:pathname test-file :directory (temporary-directory))
    (with-temporary-file (:pathname compressed-file :directory (temporary-directory))
      (with-temporary-file (:pathname decompressed-file :directory (temporary-directory))
        (let ((test-content "This is a test file for LZ4 compression testing.
It contains multiple lines of text and repeats some content.
It contains multiple lines of text and repeats some content.
This ensures that the file I/O wrappers correctly handle the size header."))
          (with-open-file (s test-file :direction :output :if-exists :supersede)
            (write-string test-content s))

          (let ((original-size (length (map 'vector #'char-code test-content)))
                (compressed-out 0)
                (uncompressed-in 0))
            (multiple-value-bind (uncompressed-out-bind compressed-out-bind)
                (compress-file test-file compressed-file)
              (is (= original-size uncompressed-out-bind))
              (is (> compressed-out-bind 8))
              (setf compressed-out compressed-out-bind))

            (multiple-value-bind (compressed-in-bind uncompressed-in-bind)
                (decompress-file compressed-file decompressed-file)
              (is (= compressed-out compressed-in-bind))
              (setf uncompressed-in uncompressed-in-bind))

            (is (= original-size uncompressed-in))
            (let ((restored-content (uiop:read-file-string decompressed-file)))
              (is (string= test-content restored-content)))))))))
(defun run-tests ()
  "Run all LZ4 tests"
  (run! 'lz4-tests))
