#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem random-state-viewer
  :version "1.0.0"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :license "Artistic"
  :description "Visualiser for the random number generators"
  :homepage "https://github.com/Shinmera/random-state"
  :serial T
  :components ((:file "viewer"))
  :depends-on (:qtools
               :qtcore
               :qtgui))
