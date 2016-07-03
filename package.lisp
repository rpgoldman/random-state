#|
This file is a part of random-state
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:random-state
  (:nicknames #:org.shirakumo.random-state)
  (:use #:cl)
  ;; generator.lisp
  (:export
   #:generator
   #:seed
   #:bytes
   #:generator-class
   #:make-generator
   #:random-byte
   #:random-bytes
   #:random-unit
   #:random-float
   #:random-int
   #:reseed)
  ;; linear-congruence.lisp
  (:export
   #:linear-congruence)
  ;; mersenne-twister.lisp
  (:export
   #:mersenne-twister-32
   #:mersenne-twister-64)
  ;; middle-square.lisp
  (:export
   #:middle-square)
  ;; pcg.lisp
  (:export
   #:pcg)
  ;; rc4.lisp
  (:export
   #:rc4)
  ;; tt800.lisp
  (:export
   #:tt800))
