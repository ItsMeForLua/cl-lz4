;;; HELP ME

(in-package #:cl-lz4)

(defun compress (input-data)
  "High-level LZ4 compression for various data types.
   Accepts a string or a byte-vector.
   Returns a (vector (unsigned-byte 8))."
  (let ((byte-vector
          (etypecase input-data
            (string (string-to-bytes-fast input-data))
            ((vector (unsigned-byte 8)) input-data))))
    ;; Ensure the vector is simple for performance sake
    (unless (typep byte-vector '(simple-array (unsigned-byte 8) (*)))
      (setf byte-vector (coerce byte-vector '(simple-array (unsigned-byte 8) (*)))))
    (compress-block-optimized byte-vector)))

(defun compress-block (input-data &key (start 0) (end (length input-data)))
  "A convenience wrapper for the default optimized block compressor."
  (compress-block-optimized input-data :start start :end end))

(declaim (inline hash-sequence-fast))
(defun hash-sequence-fast (sequence)
  "Optimized hash function with better avalanche properties."
  (declare (type (unsigned-byte 32) sequence)
           (optimize (speed 3) (safety 0)))
  (let* ((h32 (ldb (byte 32 0) (* sequence +hash-magic+)))
         (h32 (logxor h32 (ash h32 -16))))
    (ash h32 (- (- 32 +hash-log+)))))

(declaim (inline hash-position-fast))
(defun hash-position-fast (data position)
  "Optimized hash position calculation."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum position)
           (optimize (speed 3) (safety 0)))
  (hash-sequence-fast (read-u32-le data position)))

;;; Faster 4-byte/1-byte comparison logic. Previous iterations were slower. This one was the fastest.
(declaim (inline match-length-fast))
(defun match-length-fast (data pos1 pos2 end)
  "Optimized match length calculation with unrolled loops."
  (declare (optimize (speed 3) (safety 0))
           (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum pos1 pos2 end))
  (let ((len 0)
        (limit (min end (+ pos1 (- end pos2)))))
    (declare (type fixnum len limit))
    
    ;; Fast 4-byte comparison when possible
    (loop while (and (<= (+ pos1 len 4) limit) (<= (+ pos2 len 4) end))
          do (if (= (read-u32-le data (+ pos1 len)) (read-u32-le data (+ pos2 len)))
                 (incf len 4)
                 (return)))
                 
    ;; Fall back to byte-by-byte for the remainder
    (loop while (and (< (+ pos1 len) limit) (< (+ pos2 len) end) (= (aref data (+ pos1 len)) (aref data (+ pos2 len))))
          do (incf len))
    len))

;;; Pre-allocated buffer
(defvar *temp-buffer* nil)
(defvar *temp-buffer-size* 0)

(defun ensure-temp-buffer (size)
  "Ensure temporary buffer is large enough."
  (when (or (null *temp-buffer*) (< *temp-buffer-size* size))
    (setf *temp-buffer* (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)
          *temp-buffer-size* size))
  (setf (fill-pointer *temp-buffer*) 0)
  *temp-buffer*)

;;; Manual writers for fixed-buffer operations
(declaim (inline write-literal-len-manual))
(defun write-literal-len-manual (output-buffer output-pos len)
  (declare (type (vector (unsigned-byte 8)) output-buffer) (type fixnum output-pos len) (optimize (speed 3) (safety 0)))
  (if (< len +run-mask+) output-pos
      (let ((remaining (- len +run-mask+)))
        (loop while (>= remaining #xFF)
              do (setf (aref output-buffer output-pos) #xFF) (incf output-pos) (decf remaining #xFF))
        (setf (aref output-buffer output-pos) remaining)
        (1+ output-pos))))

(declaim (inline write-match-len-manual))
(defun write-match-len-manual (output-buffer output-pos len)
  (declare (type (vector (unsigned-byte 8)) output-buffer) (type fixnum output-pos len) (optimize (speed 3) (safety 0)))
  (if (< len +ml-mask+) output-pos
      (let ((remaining (- len +ml-mask+)))
        (loop while (>= remaining #xFF)
              do (setf (aref output-buffer output-pos) #xFF) (incf output-pos) (decf remaining #xFF))
        (setf (aref output-buffer output-pos) remaining)
        (1+ output-pos))))

;;; Hash table pooling for performance
(defvar *hash-table-pool* '())
(defvar *max-pooled-hash-tables* 4)

(defun get-pooled-hash-table ()
  (let ((table (pop *hash-table-pool*)))
    (if table (progn (fill table 0) table)
        (make-array +hash-table-size+ :element-type 'fixnum :initial-element 0))))

(defun return-pooled-hash-table (table)
  (when (< (length *hash-table-pool*) *max-pooled-hash-tables*)
    (push table *hash-table-pool*)))

(defun compress-block-preallocated (input-data output-buffer hash-table &key (start 0) (end (length input-data)))
  "Expert-level: Compresses into a pre-allocated buffer with a pre-allocated hash-table.
   Returns the number of bytes written. No allocations, no safety checks."
  (declare (type (simple-array (unsigned-byte 8) (*)) input-data)
           (type (vector (unsigned-byte 8)) output-buffer)
           (type (simple-array fixnum (*)) hash-table)
           (type fixnum start end)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((pos start) (anchor start) (output-pos 0) (input-limit (- end +min-match-length+)))
    (declare (type fixnum pos anchor output-pos input-limit))
    (when (<= (- end start) 0) (return-from compress-block-preallocated 0))
    (loop
      (let ((best-match-len 0) (match-pos 0))
        (declare (type fixnum best-match-len match-pos))

        ;; Step 1: Find a match at the current position
        (when (< pos input-limit)
          (let* ((hash (hash-position-fast input-data pos))
                 (masked-hash (logand hash (1- +hash-table-size+)))
                 (candidate (aref hash-table masked-hash)))
            (setf (aref hash-table masked-hash) pos)
            (when (and (> (- pos candidate) 0)
                       (< (- pos candidate) +max-distance+)
                       (>= candidate start))
              (let ((ml (match-length-fast input-data candidate pos end)))
                (when (>= ml +min-match-length+)
                  (setf best-match-len ml match-pos candidate))))))

        ;; Step 2: Lazy Matching
        (when (and (> best-match-len 0) (< best-match-len +lazy-match-threshold+))
          (let ((next-pos (1+ pos)))
            (when (< next-pos input-limit)
              (let* ((hash (hash-position-fast input-data next-pos))
                     (masked-hash (logand hash (1- +hash-table-size+)))
                     (candidate (aref hash-table masked-hash)))
                (when (and (> (- next-pos candidate) 0)
                           (< (- next-pos candidate) +max-distance+)
                           (>= candidate start))
                  (let ((next-ml (match-length-fast input-data candidate next-pos end)))
                    (when (> next-ml best-match-len)
                      (setf best-match-len 0))))))))

        ;; Step 3: Emit sequence or advance
        (if (>= best-match-len +min-match-length+)
            (let* ((literal-len (- pos anchor)) (encoded-mlen (- best-match-len +min-match-length+))
                   (token (logior (ash (min literal-len +run-mask+) 4) (min encoded-mlen +ml-mask+))))
              (setf (aref output-buffer output-pos) token) (incf output-pos)
              (setf output-pos (write-literal-len-manual output-buffer output-pos literal-len))
              (dotimes (i literal-len)
                (setf (aref output-buffer (+ output-pos i)) (aref input-data (+ anchor i))))
              (incf output-pos literal-len)
              (write-u16-le (- pos match-pos) output-buffer output-pos) (incf output-pos 2)
              (setf output-pos (write-match-len-manual output-buffer output-pos encoded-mlen))
              (incf pos best-match-len) (setf anchor pos))
            (incf pos))
        (when (>= pos end) (return))))
    (let ((literal-len (- end anchor)))
      (when (> literal-len 0)
        (let ((token (ash (min literal-len +run-mask+) 4)))
          (setf (aref output-buffer output-pos) token) (incf output-pos)
          (setf output-pos (write-literal-len-manual output-buffer output-pos literal-len))
          (dotimes (i literal-len)
            (setf (aref output-buffer (+ output-pos i)) (aref input-data (+ anchor i))))
          (incf output-pos literal-len))))
    output-pos))

(defun compress-into-buffer (input-data output-buffer &key (start 0) (end (length input-data)))
  (declare (type (simple-array (unsigned-byte 8) (*)) input-data) (type (vector (unsigned-byte 8)) output-buffer) (type fixnum start end) (optimize (speed 3) (safety 0)))
  (let ((hash-table (get-pooled-hash-table)))
    (unwind-protect
         (let ((bytes-written (compress-block-preallocated input-data output-buffer hash-table :start start :end end)))
           (setf (fill-pointer output-buffer) bytes-written)
           output-buffer)
      (return-pooled-hash-table hash-table))))

(defun compress-block-optimized (input-data &key (start 0) (end (length input-data)))
  (declare (type (simple-array (unsigned-byte 8) (*)) input-data) (type fixnum start end) (optimize (speed 3) (safety 0)))
  (let* ((input-len (- end start)) (output (ensure-temp-buffer (estimate-compressed-size input-len))))
    (let ((result-buffer (compress-into-buffer input-data output :start start :end end)))
      (subseq result-buffer 0 (fill-pointer result-buffer)))))

(defvar *output-buffer-pool* '())
(defvar *max-pooled-buffers* 4)

(defun get-pooled-buffer (size)
  (let ((buffer (pop *output-buffer-pool*)))
    (if buffer
        (progn (when (< (array-total-size buffer) size) (adjust-array buffer size)) (setf (fill-pointer buffer) 0) buffer)
        (make-array size :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))))

(defun return-pooled-buffer (buffer)
  (when (< (length *output-buffer-pool*) *max-pooled-buffers*)
    (push buffer *output-buffer-pool*)))

;;; Beautiful buffer pooling
(defun compress-block-pooled (input-data &key (start 0) (end (length input-data)))
  (declare (type (simple-array (unsigned-byte 8) (*)) input-data) (type fixnum start end) (optimize (speed 3) (safety 0)))
  (let* ((input-len (- end start)) (output-buffer (get-pooled-buffer (estimate-compressed-size input-len))))
    (unwind-protect
         (progn (compress-into-buffer input-data output-buffer :start start :end end)
                (subseq output-buffer 0 (fill-pointer output-buffer)))
      (return-pooled-buffer output-buffer))))

(defun compress-block-unsafe (input-data &key (start 0) (end (length input-data)))
  "Ultra-fast LZ4 compression with minimal safety checks. Allocates its own buffers."
  (declare (type (simple-array (unsigned-byte 8) (*)) input-data)
           (type fixnum start end)
           (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((input-len (- end start))
         (output-buffer (make-array (estimate-compressed-size input-len) :element-type '(unsigned-byte 8)))
         (hash-table (make-array +hash-table-size+ :element-type 'fixnum)))
    (fill hash-table 0)
    (let ((bytes-written (compress-block-preallocated input-data output-buffer hash-table :start start :end end)))
      (subseq output-buffer 0 bytes-written))))
