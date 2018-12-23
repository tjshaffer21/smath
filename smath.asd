(asdf:defsystem #:smath
  :description "A simple math library."
  :version "0.1.1"
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:iterate)
  :components ((:file "src/package")
               (:file "src/smath")
               (:file "src/primes")
               (:file "src/sieves" :depends-on ("src/smath"))
               (:file "src/fibonacci")))

(asdf:defsystem #:smath-tests
  :description "Unit tests for smath."
  :author "Thomas Shaffer <tjshaffer21@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (:lisp-unit
               :trivial-benchmark
               :smath)
  :components ((:file "tests/package")
               (:file "tests/test")
               (:file "tests/benchmarks")))