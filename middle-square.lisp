#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass middle-square (generator)
  ((maximum :initarg :maximum :reader maximum :writer set-maximum)
   (state :accessor state)))

(defmethod reseed ((generator middle-square) &optional new-seed)
  (setf (state generator) new-seed)
  (set-seed new-seed generator)
  (set-maximum (1- (expt 2 (integer-length new-seed))) generator))

(defmethod random-unit ((generator middle-square))
  (let* ((digits (integer-length (maximum generator)))
         (square (expt (state generator) 2))
         (offset (floor (/ (max 0 (- (integer-length square) digits)) 2)))
         (new (ldb (byte digits offset) square)))
    (setf (state generator) new)
    (float
     (/ new (maximum generator))
     0.0d0)))
