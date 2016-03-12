#|
This file is a part of random-state
(c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass middle-square (generator)
  ((maximum :reader maximum :writer %set-max)
   (minimum :reader minimum :writer %set-min)
   (state :accessor state)))

(defmethod reseed ((generator middle-square) &optional new-seed)
  (let ((new-seed (or new-seed (get-universal-time))))
    (setf (state generator) new-seed)
    (%set-seed new-seed generator)
    (%set-min 0 generator)
    (%set-max (1- (expt 2 (integer-length new-seed))) generator))
  generator)

(defmethod random-int ((generator middle-square) (from integer) (to integer))
  (round (random-float generator from to)))

(defmethod random-float ((generator middle-square) (from real) (to real))
  (let* ((digits (integer-length (maximum generator)))
         (square (expt (state generator) 2))
         (offset (floor (/ (max 0 (- (integer-length square) digits)) 2)))
         (new (ldb (byte digits offset) square)))
    (setf (state generator) new)
    (+ (float
        (/ (* (- to from)
              (- new 0))
           (- (maximum generator)
              (minimum generator)))
        0.0d0)
       from)))
