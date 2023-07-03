(asdf:defsystem random-state-test
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Portable random number generation."
  :homepage "https://Shinmera.github.io/random-state/"
  :bug-tracker "https://github.com/Shinmera/random-state/issues"
  :source-control (:git "https://github.com/Shinmera/random-state.git")
  :serial T
  :components ((:file "test"))
  :depends-on (:random-state :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.random-state.test)))
