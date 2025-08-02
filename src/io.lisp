(in-package #:cl-lz4)

(defun read-file-to-vector (filename)
  "Read file contents into a byte vector."
  (with-open-file (stream filename :element-type '(unsigned-byte 8))
    (let ((vector (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence vector stream)
      vector)))

(defun write-vector-to-file (vector filename)
  "Write byte vector to file."
  (with-open-file (stream filename
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists :supersede)
    (write-sequence vector stream)))

(defun compress-file (input-filename output-filename)
  "Compresses a file. Prepends the 8-byte uncompressed size to the output file."
  (handler-case
      (let* ((input-data (read-file-to-vector input-filename))
             (uncompressed-size (length input-data))
             (compressed-block (compress-block input-data))
             (header (make-array 8 :element-type '(unsigned-byte 8))))
        (write-u64-le uncompressed-size header 0)
        (with-open-file (stream output-filename
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          (write-sequence header stream)
          (write-sequence compressed-block stream))
        (values uncompressed-size (+ 8 (length compressed-block))))
    (error (e)
      (error 'lz4-compression-error
             :message (format nil "Failed to compress file ~A: ~A" input-filename e)))))

(defun decompress-file (input-filename output-filename)
  "Decompresses a file that was compressed with compress-file."
  (handler-case
      (with-open-file (stream input-filename :element-type '(unsigned-byte 8))
        (let* ((header (make-array 8 :element-type '(unsigned-byte 8)))
               (bytes-read (read-sequence header stream)))
          (unless (= bytes-read 8)
            (error "Invalid or truncated LZ4 file header."))
          (let* ((uncompressed-size (read-u64-le header 0))
                 (compressed-size (- (file-length stream) 8))
                 (compressed-block (make-array compressed-size :element-type '(unsigned-byte 8))))
            (read-sequence compressed-block stream)
            (let ((decompressed-data (decompress-block compressed-block uncompressed-size)))
              (write-vector-to-file decompressed-data output-filename)
              (values (+ 8 compressed-size) (length decompressed-data))))))
    (error (e)
      (error 'lz4-decompression-error
             :message (format nil "Failed to decompress file ~A: ~A" input-filename e)))))

(defun compress-stream (input-stream output-stream &key (buffer-size 65536))
  "Compresses a stream. Note: This reads the entire input stream into memory."
  (let* ((input-data (make-array buffer-size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (loop for bytes-read = (read-sequence buffer input-stream)
          while (> bytes-read 0)
          do (let ((current-size (fill-pointer input-data)))
               (when (> (+ current-size bytes-read) (array-dimension input-data 0))
                 (adjust-array input-data (+ current-size bytes-read)))
               (replace input-data buffer :start1 current-size :end2 bytes-read)
               (incf (fill-pointer input-data) bytes-read)))
    (let* ((uncompressed-size (length input-data))
           (compressed-block (compress-block input-data))
           (header (make-array 8 :element-type '(unsigned-byte 8))))
      (write-u64-le uncompressed-size header 0)
      (write-sequence header output-stream)
      (write-sequence compressed-block output-stream)
      (values uncompressed-size (+ 8 (length compressed-block))))))

(defun decompress-stream (input-stream output-stream)
  "Decompresses a stream. Note: This reads the entire input stream into memory."
  (let* ((header (make-array 8 :element-type '(unsigned-byte 8)))
         (bytes-read (read-sequence header input-stream)))
    (unless (= bytes-read 8)
      (error "Invalid or truncated LZ4 file header."))
    (let* ((uncompressed-size (read-u64-le header 0))
           (compressed-block
            (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
                  (total-bytes '()))
              (loop for bytes-read = (read-sequence buffer input-stream)
                    while (> bytes-read 0)
                    do (push (subseq buffer 0 bytes-read) total-bytes))
              (apply #'concatenate 'vector (nreverse total-bytes)))))
      (let ((decompressed-data (decompress-block compressed-block uncompressed-size)))
        (write-sequence decompressed-data output-stream)
        (values (+ 8 (length compressed-block)) (length decompressed-data))))))
