#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

;; Adapted from
;;   http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/VERSIONS/C-LANG/tt800.c

(defclass tt800 (generator)
  ((magic :initform (barr 32 0 #x8ebfd028) :reader magic)
   (shiftops :initform #((  7 #x2b5b2500)
                         ( 15 #xdb8b0000)
                         (-16 #xffffffff)) :reader shiftops)
   (n :initform 25 :reader n)
   (m :initform 7 :reader m)
   (index :initform 0 :accessor index)
   (matrix :initform NIL :reader matrix :writer set-matrix)))

(defmethod reseed ((generator tt800) &optional new-seed)
  (set-matrix (32bit-seed-array (n generator) new-seed) generator))

(defmethod random-unit ((generator tt800))
  (let ((i 0)
        (n (n generator))
        (m (m generator))
        (matrix (matrix generator))
        (magic (magic generator))
        (shiftops (shiftops generator)))
    (declare (optimize speed)
             (type (simple-array (unsigned-byte 32)) matrix magic)
             (type (unsigned-byte 8) n m i))
    (flet ((matrix (n) (aref matrix n))
           (magic (n) (aref magic n)))
      (declare (inline matrix magic))
      (when (= (the integer (index generator)) n)
        (loop while (< i (- n m))
              do (setf (aref matrix i)
                       (logxor (logxor (matrix (+ i m))
                                       (ash (matrix i) -1))
                               (magic (mod (matrix i) 2))))
                 (incf i))
        (loop while (< i m)
              do (setf (aref matrix i)
                       (logxor (logxor (matrix (+ i (- m n)))
                                       (ash (matrix i) -1))
                               (magic (mod (matrix i) 2))))
                 (incf i))
        (setf (index generator) 0))
      (let ((result (matrix (index generator))))
        (declare (type (unsigned-byte 32) result))
        (incf (index generator))
        (loop for (shift mask) across shiftops
              do (setf result (logxor result (logand (ash result (the (signed-byte 6) shift))
                                                     (the (unsigned-byte 32) mask)))))
        (/ (float result 0.0d0) #xffffffff)))))
