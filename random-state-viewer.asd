(asdf:defsystem random-state-viewer
  :version "1.0.0"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :license "zlib"
  :description "Visualiser for the random number generators"
  :homepage "https://Shinmera.github.io/random-state/"
  :bug-tracker "https://github.com/Shinmera/random-state/issues"
  :source-control (:git "https://github.com/Shinmera/random-state.git")
  :serial T
  :components ((:file "viewer"))
  :depends-on (:random-state
               :zpng
               :trivial-features
               :uiop))
