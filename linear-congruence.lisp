#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass linear-congruence (generator)
  ((state :accessor state)
   (multiplier :initarg :multiplier :reader multiplier :writer set-multiplier)
   (increment :initarg :increment :reader increment :writer set-increment)
   (bytes :initarg :bytes :writer set-bytes))
  (:default-initargs
   :bytes 64
   :multiplier 6364136223846793005
   :increment 1442695040888963407))

(defmethod reseed ((generator linear-congruence) &optional new-seed)
  (setf (state generator) (mod new-seed (1- (ash 1 (bytes generator))))))

(defmethod random-byte ((generator linear-congruence))
  (let ((c (increment generator))
        (a (multiplier generator))
        (x (state generator))
        (b (bytes generator)))
    (declare (optimize speed)
             (type integer b c a x))
    (let ((new (mod (+ c (* x a)) (1- (ash 1 b)))))
      (setf (state generator) new)
      new)))
