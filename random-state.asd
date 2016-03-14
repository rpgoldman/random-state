#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem random-state
  :version "0.1.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Portable random number generation."
  :homepage "https://github.com/Shinmera/random-state"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "generator")
               (:file "middle-square")
               (:file "linear-congruence")
               (:file "tt800")
               (:file "mersenne-twister")
               (:file "rc4"))
  :depends-on ())
