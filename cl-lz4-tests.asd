(defsystem #:cl-lz4-tests
  :description "Test suite for the cl-lz4 library."
  :author "Andrew D. France andrewforlua@gmail.com"
  :license "MIT"
  :version "1.0.0"
  :depends-on (#:cl-lz4 #:fiveam #:uiop)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :perform (test-op (op c)
                    (symbol-call '#:cl-lz4-tests '#:run-tests)))