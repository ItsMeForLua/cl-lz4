(defpackage #:cl-lz4
  (:use #:cl)
  (:export
   #:compress
   #:decompress

   #:compress-file
   #:decompress-file

   #:compress-stream
   #:decompress-stream

   #:compress-block
   #:decompress-block

   #:compress-block-optimized
   #:compress-block-pooled
   #:compress-block-unsafe
   #:compress-block-preallocated

   #:estimate-compressed-size

   #:+min-match-length+
   #:+max-distance+
   
   #:lz4-error
   #:lz4-compression-error
   #:lz4-decompression-error
   #:lz4-invalid-data-error))

(in-package #:cl-lz4)
