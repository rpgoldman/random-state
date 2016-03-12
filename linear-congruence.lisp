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
   (modulus :initarg :modulus :reader modulus :writer set-modulus))
  (:default-initargs
   :modulus (expt 2 64)
   :multiplier 6364136223846793005
   :increment 1442695040888963407))

(defmethod reseed ((generator linear-congruence) &optional new-seed)
  (destructuring-bind (seed &optional (a (multiplier generator))
                                      (c (increment generator))
                                      (m (modulus generator)))
      (if (listp new-seed) new-seed (list new-seed))
    (let ((seed (mod seed m)))
      (setf (state generator) seed)
      (set-seed seed generator)
      (set-multiplier a generator)
      (set-increment c generator)
      (set-modulus m generator))))

(defmethod random-unit ((generator linear-congruence))
  (let ((new (mod (+ (increment generator)
                     (* (state generator)
                        (multiplier generator)))
                  (modulus generator))))
    (setf (state generator) new)
    (float (/ new (modulus generator)) 0.0d0)))
