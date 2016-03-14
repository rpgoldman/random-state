#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass rc4 (generator)
  ((index1 :initform 0 :accessor index1)
   (index2 :initform 0 :accessor index2)
   (state :initform (make-array 256 :element-type '(unsigned-byte 8)) :reader state)))

(defmethod reseed ((generator rc4) &optional new-seed)
  (let ((state (state generator)))
    (declare (optimize speed))
    (declare (type (simple-array (unsigned-byte 8) 1) state))
    (loop for i from 0 below 256
          do (setf (aref state i) i))
    (setf (index1 generator) 0)
    (setf (index2 generator) 0)
    (let ((seed-length (1+ (floor (log new-seed 2)))))
      (loop for i of-type (integer 0 256) from 0 below 256
            for j of-type (integer 0 256) = 0
            then (mod (+ j
                         (aref state i)
                         (ldb (byte 8 (mod (* 8 i) seed-length)) new-seed))
                      256)
            do (rotatef (aref state i)
                        (aref state j))))))

(defmethod random-unit ((generator rc4))
  (flet ((get-byte ()
           (let ((state (state generator)))
             (declare (optimize speed))
             (declare (type (simple-array (unsigned-byte 8) 1) state)
                      (ftype (function (rc4) (integer 0 256)) index1 index2))
             (incfmod (index1 generator) 256)
             (incfmod (index2 generator) 256 (aref state (index1 generator)))
             (rotatef (aref state (index1 generator))
                      (aref state (index2 generator)))
             (aref state
                   (mod
                    (+ (aref state (index1 generator))
                       (aref state (index2 generator)))
                    256)))))
    ;; A single byte of randomness is a bit little, so we compose more of it.
    (let ((num 0))
      (setf (ldb (byte 8 24) num) (get-byte))
      (setf (ldb (byte 8 16) num) (get-byte))
      (setf (ldb (byte 8 8) num) (get-byte))
      (setf (ldb (byte 8 0) num) (get-byte))
      (/ (float num 0.0d0) #xFFFFFFFF))))
