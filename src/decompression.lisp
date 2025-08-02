(in-package #:cl-lz4)

(defun decompress-block (compressed-data uncompressed-size)
  "Decompress a block of LZ4 data, given the uncompressed size."
  (let ((output (make-array uncompressed-size :element-type '(unsigned-byte 8)))
        (input-pos 0)
        (output-pos 0)
        (input-end (length compressed-data)))

    (loop while (< input-pos input-end)
          do (let* ((token (aref compressed-data input-pos))
                    (literal-len (ash token -4))
                    (match-len (logand token #x0F)))
               (incf input-pos)

               ;; Handle extended literal length
               (when (= literal-len #xF)
                 (loop for byte = (aref compressed-data input-pos)
                       do (incf input-pos)
                          (incf literal-len byte)
                       while (= byte #xFF)))

               ;; Copy literals
               (when (> literal-len 0)
                 (when (> (+ output-pos literal-len) uncompressed-size)
                   (error 'lz4-decompression-error :message "Output buffer overflow during literal copy."))
                 (when (> (+ input-pos literal-len) input-end)
                   (error 'lz4-decompression-error :message "Input buffer underflow during literal copy."))

                 (replace output compressed-data :start1 output-pos :start2 input-pos :end2 (+ input-pos literal-len))
                 (incf input-pos literal-len)
                 (incf output-pos literal-len))

               ;; Process match (if not at end of block)
               (when (< input-pos input-end)
                 (let ((distance (read-u16-le compressed-data input-pos)))
                   (incf input-pos 2)

                   (when (or (= distance 0) (> distance output-pos))
                     (error 'lz4-decompression-error :message "Invalid match distance."))

                   ;; Handle extended match length
                   (when (= match-len #xF)
                     (loop for byte = (aref compressed-data input-pos)
                           do (incf input-pos)
                              (incf match-len byte)
                           while (= byte #xFF)))
                   (incf match-len +min-match-length+)

                   (when (> (+ output-pos match-len) uncompressed-size)
                     (error 'lz4-decompression-error :message "Match extends beyond output buffer."))

                   ;; Copy match
                   (let ((match-start (- output-pos distance)))
                     (loop for i from 0 below match-len
                           do (setf (aref output (+ output-pos i))
                                    (aref output (+ match-start i))))
                     (incf output-pos match-len))))))
    
    ;; Final validation: ensure the decompressed size is exactly what was expected.
    (unless (= output-pos uncompressed-size)
      (error 'lz4-decompression-error
             :message (format nil "Decompression failed: expected ~D bytes, but produced ~D bytes."
                              uncompressed-size output-pos)))
    
    output))

(defun decompress (compressed-data &key (uncompressed-size nil))
  "Decompress LZ4 block data. Uncompressed size is required."
  (unless (and uncompressed-size (>= uncompressed-size 0))
    (error 'lz4-decompression-error :message "Valid uncompressed-size is required for decompression."))
  (typecase compressed-data
    (vector (decompress-block compressed-data uncompressed-size))
    (t (error 'lz4-decompression-error :message "Invalid compressed data type"))))
