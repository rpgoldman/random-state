(asdf:defsystem random-state
  :version "1.0.1"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Portable random number generation."
  :homepage "https://Shinmera.github.io/random-state/"
  :bug-tracker "https://github.com/Shinmera/random-state/issues"
  :source-control (:git "https://github.com/Shinmera/random-state.git")
  :in-order-to ((test-op (test-op "random-state-test")))
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "generator")
               (:file "protocol")
               (:file "primes")
               (:file "adler32")
               (:file "cityhash")
               (:file "hammersley")
               (:file "linear-congruence")
               (:file "kiss")
               (:file "mersenne-twister")
               (:file "middle-square")
               (:file "murmurhash")
               (:file "pcg")
               (:file "quasi")
               (:file "rc4")
               (:file "sobol")
               (:file "squirrel")
               (:file "tt800")
               (:file "xkcd")
               (:file "xorshift")
               (:file "implementation")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :random-state-test))))
