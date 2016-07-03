#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass pcg (generator)
  ((state :initarg :state :accessor state)
   (inc :initarg :inc :accessor inc)
   (bytes :initform 32))
  (:default-initargs
   :state #x853c49e6748fea9b
   :inc #xda3e39cb94b95bdb))

(defmethod reseed ((generator pcg) &optional new-seed)
  (setf (state generator) 0)
  (setf (inc generator) (truncate64 (logior 1 (ash new-seed 1))))
  (random-byte generator)
  (setf (state generator) (truncate64 (+ (state generator) new-seed)))
  (random-byte generator))

(defmethod random-byte ((generator pcg))
  (declare (optimize speed))
  (let ((oldstate (the (unsigned-byte 64) (state generator))))
    (setf (state generator) (truncate64
                             (+ (truncate64 (* oldstate #x6364136223846793005))
                                (the (unsigned-byte 64) (inc generator)))))
    (let* ((xorshifted (ash (logxor (ash oldstate -18) oldstate) -27))
           (rot (ash oldstate -59)))
      (truncate32
       (logior (ash xorshifted (- rot))
               (ash xorshifted (logand (- rot) 31)))))))
