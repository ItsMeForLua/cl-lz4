(defsystem #:cl-lz4-benchmarks
  :description "Benchmark suite for the cl-lz4 library."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "1.0.0"
  :depends-on (#:cl-lz4 #:salza2 #:chipz #:trivial-benchmark #:uiop)
  :components ((:module "benchmarks"
                :components
                ((:file "main")))))