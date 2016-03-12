#|
 This file is a part of random-state
 (c) 2015 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.random-state)

(defclass generator ()
  ((seed :initarg :seed :reader seed :writer set-seed))
  (:default-initargs
   :seed (error "SEED required.")))

(defmethod print-object ((generator generator) stream)
  (print-unreadable-object (generator stream :type T)
    (format stream "~s" (seed generator))))

(defgeneric random-unit (generator))

(defgeneric random-float (generator from to))

(defmethod random-float :around ((generator generator) from to)
  (if (< from to)
      (call-next-method)
      (call-next-method generator to from)))

(defmethod random-float ((generator generator) from to)
  (+ from (* (random-unit generator) (- to from))))

(defgeneric random-int (generator from to))

(defmethod random-int :around ((generator generator) from to)
  (if (< from to)
      (call-next-method)
      (call-next-method generator to from)))

(defmethod random-int ((generator generator) from to)
  (round (random-float generator from to)))

(defgeneric reseed (generator &optional new-seed))

(defmethod reseed :around ((generator generator) &optional new-seed)
  (let ((seed (or new-seed (get-universal-time))))
    (set-seed seed generator)
    (call-next-method generator seed))
  generator)

(defvar *generator*)

(defun make-generator (type &optional seed &rest initargs)
  (let ((generator (apply #'make-instance type :seed seed initargs)))
    (reseed generator seed)))

(defun random (limit &optional (generator *generator*))
  (random-int generator 0 limit))
