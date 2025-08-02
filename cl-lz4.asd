
(defsystem #:cl-lz4
  :description "A pure Common Lisp implementation of the LZ4 compression algorithm."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "Public Domain"
  :version "1.0.0"
  :homepage "https://github.com/ItsMeForLua/cl-lz4"
  :bug-tracker "https://github.com/ItsMeForLua/cl-lz4/issues"
  :source-control (:git "https://github.com/ItsMeForLua/cl-lz4.git")

  ;; All source files are located in the src/ directory.
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "constants" :depends-on ("package"))
                 (:file "utils" :depends-on ("package"))
                 (:file "compression" :depends-on ("package" "constants" "utils"))
                 (:file "decompression" :depends-on ("package" "constants" "utils"))
                 (:file "io" :depends-on ("package" "compression" "decompression")))))
  :encoding :utf-8)
