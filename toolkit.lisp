#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(declaim (inline truncate32))
(declaim (ftype (function (integer) (unsigned-byte 32)) truncate32))
(defun truncate32 (x)
  (logand x #xFFFFFFFF))

(declaim (inline truncate64))
(declaim (ftype (function (integer) (unsigned-byte 64)) truncate64))
(defun truncate64 (x)
  (logand x #xFFFFFFFFFFFFFFFF))
